{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Text.Builder where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.DeepSeq (force)
import qualified Control.Exception as C (evaluate)
import Data.ListLike.IO (hGetContents)
import Data.Text.Lazy (toChunks)
import Data.Text.Lazy.Builder (Builder, fromText)
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)

-- | Like 'System.Process.readProcessWithExitCode', but using 'Text'
instance ListLikeProcessIO Builder Char where
    forceOutput = C.evaluate . force
    readChunks h = (map fromText . toChunks) <$> hGetContents h

-- | Specialized version for backwards compatibility.
readProcessWithExitCode
    :: FilePath                        -- ^ command to run
    -> [String]                        -- ^ any arguments
    -> Builder                         -- ^ standard input
    -> IO (ExitCode, Builder, Builder) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = System.Process.Common.readProcessWithExitCode

readCreateProcessWithExitCode
    :: CreateProcess                   -- ^ command and arguments to run
    -> Builder                         -- ^ standard input
    -> IO (ExitCode, Builder, Builder) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = System.Process.Common.readCreateProcessWithExitCode
