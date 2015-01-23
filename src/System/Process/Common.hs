{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Common where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent
import Control.Exception as E (SomeException, onException, catch, mask, throw)
import Control.Monad
import Data.ListLike (null)
import Data.ListLike.IO (ListLikeIO, hGetContents, hPutStr)
import Data.Monoid (Monoid(mempty, mappend), (<>))
import GHC.IO.Exception (IOErrorType(ResourceVanished), IOException(ioe_type))
import Prelude hiding (null)
import System.Exit (ExitCode(ExitFailure))
import System.IO (Handle, hClose, hFlush)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process
import Utils (forkWait)

class Monoid b => ProcessOutput a b | b -> a where
    pidf :: ProcessHandle -> b
    outf :: a -> b
    errf :: a -> b
    intf :: SomeException -> b
    codef :: ExitCode -> b

instance ListLikeProcessIO a c => ProcessOutput a (ExitCode, a, a) where
    pidf _ = mempty
    codef c = (c, mempty, mempty)
    outf x = (mempty, x, mempty)
    errf x = (mempty, mempty, x)
    intf e = throw e

-- | A process usually has one 'ExitCode' at the end of its output, this 'Monoid'
-- instance lets us build the type returned by 'System.Process.readProcessWithExitCode'.
instance Monoid ExitCode where
    mempty = ExitFailure 0
    mappend x (ExitFailure 0) = x
    mappend _ x = x

class ListLikeIO a c => ListLikeProcessIO a c where
    forceOutput :: a -> IO a
    readChunks :: Handle -> IO [a]
    -- ^ Read from a handle, returning a lazy list of the monoid a.

-- | Like 'System.Process.readProcessWithExitCode', but with generalized input and output type.
readProcessWithExitCode
    :: ListLikeProcessIO a c =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

readCreateProcessWithExitCode
    :: ListLikeProcessIO a c =>
       CreateProcess            -- ^ command and arguments to run
    -> a               -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = readCreateProcess

readCreateProcess :: (ProcessOutput a b, ListLikeProcessIO a c) => CreateProcess -> a -> IO b
readCreateProcess p input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess p{ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    flip onException
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      waitOut <- forkWait $ outf <$> (hGetContents outh >>= forceOutput)

      -- fork off a thread to start consuming stderr
      waitErr <- forkWait $ errf <$> (hGetContents errh >>= forceOutput)

      -- now write and flush any input
      unless (null input) $ do hPutStr inh input; hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      out <- waitOut
      err <- waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- codef <$> waitForProcess pid

      return $ out <> err <> ex

-- | Like readCreateProcess, but the output is read lazily.
readCreateProcessLazy :: (ProcessOutput a b, ListLikeProcessIO a c) => CreateProcess -> a -> IO b
readCreateProcessLazy p input = mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess p{ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    onException
      (restore $
       do -- fork off a thread to start consuming stdout
          -- Without unsafeIntereleaveIO the pid messsage gets stuck
          -- until some additional output arrives from the process.
          waitOut <- forkWait $ (<>) <$> pure (pidf pid)
                                     <*> unsafeInterleaveIO (readInterleaved [(outf, outh), (errf, errh)] (codef <$> waitForProcess pid))
          writeInput inh input
          waitOut)
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid)

-- | Helper function for readCreateProcessLazy.
readInterleaved :: (ListLikeProcessIO a c, ProcessOutput a b) =>
                   [(a -> b, Handle)] -> IO b -> IO b
readInterleaved pairs finish = newEmptyMVar >>= readInterleaved' pairs finish

readInterleaved' :: forall a b c. (ListLikeProcessIO a c, ProcessOutput a b) =>
                    [(a -> b, Handle)] -> IO b -> MVar (Either Handle b) -> IO b
readInterleaved' pairs finish res = do
  mapM_ (forkIO . uncurry readHandle) pairs
  takeChunks (length pairs)
    where
      -- Forked thread to read the input and send it to takeChunks via
      -- the MVar.
      readHandle :: (a -> b) -> Handle -> IO ()
      readHandle f h = do
        cs <- readChunks h
        -- If the type returned as stdout and stderr is lazy we need
        -- to force it here in the producer thread - I'm not exactly
        -- sure why.  And why is String lazy?
        -- when (lazy (undefined :: a)) (void cs)
        mapM_ (\ c -> putMVar res (Right (f c))) cs
        hClose h
        putMVar res (Left h)
      takeChunks :: Int -> IO b
      takeChunks 0 = finish
      takeChunks openCount = takeChunk >>= takeMore openCount
      takeMore :: Int -> Either Handle b -> IO b
      takeMore openCount (Left h) = hClose h >> takeChunks (openCount - 1)
      takeMore openCount (Right x) =
          do xs <- unsafeInterleaveIO $ takeChunks openCount
             return (x <> xs)
      takeChunk = takeMVar res `E.catch` (\ (e :: SomeException) -> return $ Right $ intf e)

-- | Write and flush process input, closing the handle when done.
-- Catch and ignore Resource Vanished exceptions, they just mean the
-- process exited before all of its output was read.
writeInput :: ListLikeProcessIO a c => Handle -> a -> IO ()
writeInput inh input = do
  (do unless (null input) (hPutStr inh input >> hFlush inh)
      hClose inh) `E.catch` resourceVanished (\ _ -> return ())

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
resourceVanished :: (IOError -> IO a) -> IOError -> IO a
resourceVanished epipe e = if ioe_type e == ResourceVanished then epipe e else ioError e
