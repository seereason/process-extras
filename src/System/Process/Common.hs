{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Common
    ( ProcessMaker(process, showProcessMakerForUser)
    , ListLikeProcessIO(forceOutput, readChunks)
    , ProcessText
    , ProcessResult(pidf, outf, errf, intf, codef)
    , readProcessWithExitCode
    , readCreateProcessWithExitCode
    , readCreateProcessStrict
    , readCreateProcessLazy
    , showCmdSpecForUser
    , showCreateProcessForUser
    ) where

import Control.Concurrent
import Control.Exception as E (SomeException, onException, catch, mask, throw)
import Control.Monad
import Data.ListLike as ListLike (ListLike, null)
import Data.ListLike.IO (ListLikeIO, hGetContents, hPutStr)
import Data.Monoid ((<>))
import Data.String (IsString)
import Generics.Deriving.Instances ()
import GHC.IO.Exception (IOErrorType(ResourceVanished), IOException(ioe_type))
import Prelude hiding (null)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hFlush, BufferMode, hSetBuffering)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process (CmdSpec(..), CreateProcess(cmdspec, cwd, std_err, std_in, std_out), StdStream(CreatePipe), ProcessHandle, createProcess, proc, showCommandForUser, waitForProcess, terminateProcess)
import Utils (forkWait)

#if __GLASGOW_HASKELL__ <= 709
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid(mempty, mappend))
#endif

#if !MIN_VERSION_deepseq(1,4,2)
import Control.DeepSeq (NFData)
-- | This instance lets us use DeepSeq's force function on a stream of Chunks.
instance NFData ExitCode
#endif

class ProcessMaker a where
    process :: a -> IO (Handle, Handle, Handle, ProcessHandle)
    showProcessMakerForUser :: a -> String

-- | This is the usual maker argument to 'readCreateProcessLazy'.
instance ProcessMaker CreateProcess where
    process p = do
      (Just inh, Just outh, Just errh, pid) <- createProcess p { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      return (inh, outh, errh, pid)
    showProcessMakerForUser = showCreateProcessForUser

-- | Passing this to 'readCreateProcessLazy' as the maker argument allows
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
    showProcessMakerForUser (p, outmode, errmode) =
        showCreateProcessForUser p ++ " outmode=" ++ show outmode ++ ", errmode=" ++ show errmode

class (IsString text, Monoid text, ListLike text char) => ProcessText text char

class Monoid result => ProcessResult text result | result -> text where
    pidf :: ProcessHandle -> result
    outf :: text -> result
    errf :: text -> result
    intf :: SomeException -> result
    codef :: ExitCode -> result

instance ListLikeProcessIO text char => ProcessResult text (ExitCode, text, text) where
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

#if MIN_VERSION_base(4,11,0)
instance Semigroup ExitCode where
  (<>) = mappend
#endif

-- | Process IO is based on the 'ListLikeIO' class from the ListLike
-- package
class ListLikeIO text char => ListLikeProcessIO text char where
    forceOutput :: text -> IO text
    readChunks :: Handle -> IO [text]
    -- ^ Read from a handle, returning a lazy list of the monoid a.

-- | Like 'System.Process.readProcessWithExitCode', but with
-- generalized input and output type.  Aside from the usual text-like
-- types, the output can be a list of Chunk a.  This lets you process
-- the chunks received from stdout and stderr lazil, in the order they
-- are received, as well as the exit code.  Utilities to handle Chunks
-- are provided in System.Process.ListLike.
readProcessWithExitCode
    :: ListLikeProcessIO text char =>
       FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> text               -- ^ standard input
    -> IO (ExitCode, text, text) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args input = readCreateProcessWithExitCode (proc cmd args) input

readCreateProcessWithExitCode
    :: (ProcessMaker maker, ListLikeProcessIO text char) =>
       maker                     -- ^ command and arguments to run
    -> text                      -- ^ standard input
    -> IO (ExitCode, text, text) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = readCreateProcessStrict

readCreateProcessStrict :: (ProcessMaker maker, ProcessResult text result, ListLikeProcessIO text char) =>
                           maker -> text -> IO result
readCreateProcessStrict maker input = mask $ \restore -> do
    (inh, outh, errh, pid) <- process maker
    flip onException
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid) $ restore $ do

      -- fork off a thread to start consuming stdout
      waitOut <- forkWait $ outf <$> (hGetContents outh >>= forceOutput)

      -- fork off a thread to start consuming stderr
      waitErr <- forkWait $ errf <$> (hGetContents errh >>= forceOutput)

      -- now write and flush any input.
      writeInput inh input

      -- wait on the output
      out <- waitOut
      err <- waitErr

      hClose outh
      hClose errh

      -- wait on the process
      ex <- codef <$> waitForProcess pid

      return $ out <> err <> ex

-- | Like readCreateProcessStrict, but the output is read lazily.
readCreateProcessLazy :: (ProcessMaker maker, ProcessResult a b, ListLikeProcessIO a c) => maker -> a -> IO b
readCreateProcessLazy maker input = mask $ \restore -> do
    (inh, outh, errh, pid) <- process maker
    onException
      (restore $
       do -- fork off a thread to start consuming stdout
          -- Without unsafeIntereleaveIO the pid messsage gets stuck
          -- until some additional output arrives from the process.
          waitOut <- forkWait $ (<>) <$> return (pidf pid)
                                     <*> unsafeInterleaveIO (readInterleaved [(outf, outh), (errf, errh)] (codef <$> waitForProcess pid))
          writeInput inh input
          waitOut)
      (do terminateProcess pid; hClose inh; hClose outh; hClose errh;
          waitForProcess pid)

-- | Helper function for readCreateProcessLazy.
readInterleaved :: (ListLikeProcessIO a c, ProcessResult a b) =>
                   [(a -> b, Handle)] -> IO b -> IO b
readInterleaved pairs finish = newEmptyMVar >>= readInterleaved' pairs finish

readInterleaved' :: forall a b c. (ListLikeProcessIO a c, ProcessResult a b) =>
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
writeInput inh input =
    ignoreResourceVanished $ do
      unless (ListLike.null input) $ do
        hPutStr inh input
        hFlush inh
      hClose inh -- stdin has been fully written

-- | Wrapper for a process that provides a handler for the
-- ResourceVanished exception.  This is frequently an exception we
-- wish to ignore, because many processes will deliberately exit
-- before they have read all of their input.
ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished action =
    action `E.catch` (\e -> if ioe_type e == ResourceVanished then return () else ioError e)

-- | System.Process utility functions.
showCreateProcessForUser :: CreateProcess -> String
showCreateProcessForUser p =
    showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> " (in " ++ d ++ ")") (cwd p)

showCmdSpecForUser :: CmdSpec -> String
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand p args) = showCommandForUser p args
