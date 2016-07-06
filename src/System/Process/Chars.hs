{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Chars where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.DeepSeq (force)
import qualified Control.Exception as C (evaluate)
import Data.ListLike.IO (hGetContents)
import Data.Text.Lazy (fromStrict, toChunks)
import Data.ListLike.Chars (Chars(..))
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)

-- | Like 'System.Process.readProcessWithExitCode', but using 'Text'
instance ListLikeProcessIO Chars Char where
    forceOutput = C.evaluate . force
    readChunks h = (map (T . fromStrict) . toChunks) <$> hGetContents h

-- | Specialized version for backwards compatibility.
readProcessWithExitCode
    :: FilePath                        -- ^ command to run
    -> [String]                        -- ^ any arguments
    -> Chars                         -- ^ standard input
    -> IO (ExitCode, Chars, Chars) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = System.Process.Common.readProcessWithExitCode

readCreateProcessWithExitCode
    :: CreateProcess                   -- ^ command and arguments to run
    -> Chars                         -- ^ standard input
    -> IO (ExitCode, Chars, Chars) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = System.Process.Common.readCreateProcessWithExitCode
