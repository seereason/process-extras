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
    ( ProcessMaker(process)
    , ListLikeProcessIO(forceOutput)
    , ProcessOutput(pidf, outf, errf, codef, intf)
    , readCreateProcess
    , readCreateProcessLazy
    , readCreateProcessWithExitCode
    , readProcessWithExitCode
    , Chunk(..)
    , collectOutput
    , showCreateProcessForUser
    , showCmdSpecForUser
    ) where

import Control.DeepSeq (force)
import Control.Exception as C (evaluate, SomeException, throw)
import Data.ListLike.IO (hGetContents)
#if __GLASGOW_HASKELL__ <= 709
import Data.Monoid (mempty, mconcat)
#endif
import Data.Text (unpack)
import Data.Text.Lazy (Text, toChunks)
import System.Exit (ExitCode)
import System.Process (CmdSpec(..), CreateProcess(..), ProcessHandle, showCommandForUser)
import System.Process.ByteString ()
import System.Process.ByteString.Lazy ()
import System.Process.Common (ProcessMaker(process), ListLikeProcessIO(forceOutput, readChunks), ProcessOutput(pidf, outf, errf, codef, intf),
                              readCreateProcess, readCreateProcessLazy, readCreateProcessWithExitCode, readProcessWithExitCode)
import System.Process.Text ()
import System.Process.Text.Lazy ()

-- | System.Process utility functions.
showCreateProcessForUser :: CreateProcess -> String
showCreateProcessForUser p =
    showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> " (in " ++ d ++ ")") (cwd p)

showCmdSpecForUser :: CmdSpec -> String
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand p args) = showCommandForUser p args

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

-- | Turn a @[Chunk a]@ into any other instance of 'ProcessOutput'.
collectOutput :: ProcessOutput a b => [Chunk a] -> b
collectOutput xs = mconcat $ map (\ chunk -> case chunk of
                                               ProcessHandle x -> pidf x
                                               Stdout x -> outf x
                                               Stderr x -> errf x
                                               Result x -> codef x
                                               Exception x -> intf x) xs
