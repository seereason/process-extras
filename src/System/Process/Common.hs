{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Common
    ( ProcessMaker(process)
    , ListLikeProcessIO(forceOutput, readChunks)
    , ProcessOutput(pidf, outf, errf, intf, codef)
    , readProcessWithExitCode
    , readCreateProcessWithExitCode
    , readCreateProcess
    , readCreateProcessLazy
    ) where

import Control.Concurrent
import Control.DeepSeq (NFData)
import Control.Exception as E (SomeException, onException, catch, mask, throw, try)
import Control.Monad
import Data.ListLike (null)
import Data.ListLike.IO (ListLikeIO, hGetContents, hPutStr)
import Data.Monoid ((<>))
import GHC.IO.Exception (IOErrorType(ResourceVanished), IOException(ioe_type))
import Prelude hiding (null)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hFlush, BufferMode, hSetBuffering)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CreateProcess(std_err, std_in, std_out), StdStream(CreatePipe), ProcessHandle, createProcess, proc, waitForProcess, terminateProcess)
import Utils (forkWait)

#if __GLASGOW_HASKELL__ <= 709
import Control.Applicative (pure, (<$>), (<*>))
import Data.Monoid (Monoid(mempty, mappend))
#else
import GHC.Generics

#if __GLASGOW_HASKELL__ <= 710
deriving instance Generic ExitCode
#endif
#endif

-- | This instance lets us use DeepSeq's force function on a stream of Chunks.
instance NFData ExitCode

class ProcessMaker a where
    process :: a -> IO (Handle, Handle, Handle, ProcessHandle)

-- | This is the usual maker argument to 'readCreateProcess'.
instance ProcessMaker CreateProcess where
    process p = do
      (Just inh, Just outh, Just errh, pid) <- createProcess p { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      return (inh, outh, errh, pid)

-- | Passing this to 'readCreateProcess' as the maker argument allows
-- you to set the buffer mode of the process stdout and stderr handles
-- just after the handles are created.  These are set to
-- BlockBuffering by default, but for running console commands
-- LineBuffering is probably what you want.
instance ProcessMaker (CreateProcess, BufferMode, BufferMode) where
    process (p, outmode, errmode) = do
      (Just inh, Just outh, Just errh, pid) <- createProcess p { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      hSetBuffering outh outmode
      hSetBuffering errh errmode
      return (inh, outh, errh, pid)

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
    :: (ProcessMaker maker, ListLikeProcessIO a c) =>
       maker               -- ^ command and arguments to run
    -> a                   -- ^ standard input
    -> IO (ExitCode, a, a) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = readCreateProcess

readCreateProcess :: (ProcessMaker maker, ProcessOutput a b, ListLikeProcessIO a c) => maker -> a -> IO b
readCreateProcess maker input = mask $ \restore -> do
    (inh, outh, errh, pid) <- process maker
    flip onException
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      waitOut <- forkWait $ outf <$> (hGetContents outh >>= forceOutput)

      -- fork off a thread to start consuming stderr
      waitErr <- forkWait $ errf <$> (hGetContents errh >>= forceOutput)

      -- now write and flush any input
      unless (null input) $ do ignoreResourceVanished (hPutStr inh input); hFlush inh
      hClose inh -- done with stdin

      -- wait on the output
      out <- waitOut
      err <- waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- codef <$> waitForProcess pid

      return $ out <> err <> ex

ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished action =
    try action >>= either ignoreResourceVanished' return
    where
      ignoreResourceVanished' e | ioe_type e == ResourceVanished = return ()
      ignoreResourceVanished' e = throw e

-- | Like readCreateProcess, but the output is read lazily.
readCreateProcessLazy :: (ProcessMaker maker, ProcessOutput a b, ListLikeProcessIO a c) => maker -> a -> IO b
readCreateProcessLazy maker input = mask $ \restore -> do
    (inh, outh, errh, pid) <- process maker
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
