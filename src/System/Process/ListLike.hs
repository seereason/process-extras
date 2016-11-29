-- | Re-export all symbols and instances of the process-extras
-- package.  Adds the Chunk type with a ProcessOutput instance, and a
-- collectOutput function to turn a list of chunks into any instance
-- of ProcessOutput, such as (ExitCode, String, String).  This means
-- you can have readCreateProcess output a list of Chunk, operate on
-- it to do progress reporting, and finally convert it to the type
-- that readProcessWithExitCode woud have returned.
{-# LANGUAGE CPP, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module System.Process.ListLike
    (
    -- * Classes for process IO monad, output type, and creation type
      ListLikeProcessIO(forceOutput)
    , ProcessOutput(pidf, outf, errf, codef, intf)
    , ProcessMaker(process, showProcessMakerForUser)

    -- * The generalized process runners
    , readCreateProcess
    , readCreateProcessStrict
    , readCreateProcessLazy
    , readCreateProcessWithExitCode
    , readProcessWithExitCode

    -- * Utility functions based on showCommandForUser
    , showCreateProcessForUser
    , showCmdSpecForUser

    -- * The Chunk type
    , Chunk(..)
    , collectOutput
    , foldOutput
    , writeOutput
    , writeChunk

    -- * Re-exports from process
    , CmdSpec(..)
    , CreateProcess(..)
    , proc
    , shell
    , showCommandForUser
    ) where

import Control.DeepSeq (force)
import Control.Exception as C (evaluate, SomeException, throw)
import Data.ListLike.IO (hGetContents, hPutStr, ListLikeIO)
#if __GLASGOW_HASKELL__ <= 709
import Data.Monoid (mempty, mconcat)
#endif
import Data.Text (unpack)
import Data.Text.Lazy (Text, toChunks)
import System.Exit (ExitCode)
import System.IO (stdout, stderr)
import System.Process (CmdSpec(..), CreateProcess(..), proc, ProcessHandle, shell, showCommandForUser)
import System.Process.ByteString ()
import System.Process.ByteString.Lazy ()
import System.Process.Common
    (ProcessMaker(process, showProcessMakerForUser), ListLikeProcessIO(forceOutput, readChunks),
     ProcessOutput(pidf, outf, errf, codef, intf), readCreateProcessStrict, readCreateProcessLazy,
     readCreateProcessWithExitCode, readProcessWithExitCode, showCmdSpecForUser, showCreateProcessForUser)
import System.Process.Text ()
import System.Process.Text.Builder ()
import System.Process.Text.Lazy ()

readCreateProcess :: (ProcessMaker maker, ProcessOutput text result, ListLikeProcessIO text char) => maker -> text -> IO result
readCreateProcess = readCreateProcessLazy

-- | Like 'System.Process.readProcessWithExitCode' that takes a 'CreateProcess'.
instance ListLikeProcessIO String Char where
    -- | This is required because strings are magically lazy.  Without it
    -- processes get exit status 13 - file read failures.
    forceOutput = evaluate . force
    -- | Read the handle as lazy text, convert to chunks of strict text,
    -- and then unpack into strings.
    readChunks h = do
      t <- hGetContents h :: IO Text
      return $ map unpack $ toChunks t

-- | This type is a concrete representation of the methods of class
-- ProcessOutput.  If you take your process output as this type you
-- could, for example, echo all the output and then use collectOutput
-- below to convert it to any other instance of ProcessOutput.
data Chunk a
    = ProcessHandle ProcessHandle
      -- ^ This will always come first, before any output or exit code.
    | Stdout a
    | Stderr a
    | Result ExitCode
    | Exception SomeException
      -- ^ Note that the instances below do not use this constructor.
    deriving Show

instance Show ProcessHandle where
    show _ = "<process>"

instance ListLikeProcessIO a c => ProcessOutput a [Chunk a] where
    pidf p = [ProcessHandle p]
    outf x = [Stdout x]
    errf x = [Stderr x]
    intf e = throw e
    codef c = [Result c]

instance ListLikeProcessIO a c => ProcessOutput a (ExitCode, [Chunk a]) where
    pidf p = (mempty, [ProcessHandle p])
    codef c = (c, mempty)
    outf x = (mempty, [Stdout x])
    errf x = (mempty, [Stderr x])
    intf e = throw e

foldOutput :: (ProcessHandle -> r) -- ^ called when the process handle becomes known
           -> (a -> r) -- ^ stdout handler
           -> (a -> r) -- ^ stderr handler
           -> (SomeException -> r) -- ^ exception handler
           -> (ExitCode -> r) -- ^ exit code handler
           -> Chunk a
           -> r
foldOutput p _ _ _ _ (ProcessHandle x) = p x
foldOutput _ o _ _ _ (Stdout x) = o x
foldOutput _ _ e _ _ (Stderr x) = e x
foldOutput _ _ _ i _ (Exception x) = i x
foldOutput _ _ _ _ r (Result x) = r x

-- | Turn a @[Chunk a]@ into any other instance of 'ProcessOutput'.  I
-- usually use this after processing the chunk list to turn it into
-- the (ExitCode, String, String) type returned by readProcessWithExitCode.
collectOutput :: ProcessOutput a b => [Chunk a] -> b
collectOutput xs = mconcat $ map (foldOutput pidf outf errf intf codef) xs

-- | Send Stdout chunks to stdout and Stderr chunks to stderr.
-- Returns input list unmodified.
writeOutput :: ListLikeIO a c => [Chunk a] -> IO [Chunk a]
writeOutput [] = return []
writeOutput (x : xs) = (:) <$> writeChunk x <*> writeOutput xs


writeChunk :: ListLikeIO a c => Chunk a -> IO (Chunk a)
writeChunk x =
    foldOutput (\_ -> return x)
               (\s -> hPutStr stdout s >> return x)
               (\s -> hPutStr stderr s >> return x)
               (\_ -> return x)
               (\_ -> return x) x
