{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Text.Lazy where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.DeepSeq (force)
import qualified Control.Exception as C (evaluate)
import Data.ListLike.IO (hGetContents)
import Data.Text.Lazy (Text, fromStrict, toChunks)
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)

instance ProcessText Text Char

-- | Like 'System.Process.readProcessWithExitCode', but using 'Text'
instance ListLikeProcessIO Text Char where
    forceOutput = C.evaluate . force
    readChunks h = (map fromStrict . toChunks) <$> hGetContents h

-- | Specialized version for backwards compatibility.
readProcessWithExitCode
    :: FilePath                  -- ^ command to run
    -> [String]                  -- ^ any arguments
    -> Text                      -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = System.Process.Common.readProcessWithExitCode

readCreateProcessWithExitCode
    :: CreateProcess             -- ^ command and arguments to run
    -> Text                      -- ^ standard input
    -> IO (ExitCode, Text, Text) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = System.Process.Common.readCreateProcessWithExitCode
