{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString.Lazy where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData, force)
import qualified Control.Exception as C (evaluate)
import Data.ByteString.Lazy (ByteString, toChunks, fromChunks)
import Data.ListLike.IO (hGetContents)
import Data.Word (Word8)
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)
import Utils (missingInstances, simpleMissingInstanceTest)

$(missingInstances simpleMissingInstanceTest [d|instance NFData ByteString|])

-- | Like 'System.Process.readProcessWithExitCode', but using 'ByteString'
instance ListLikeProcessIO ByteString Word8 where
    forceOutput = C.evaluate . force
    readChunks h = (map (fromChunks . (: [])) . toChunks) <$> hGetContents h

-- | Specialized version for backwards compatibility.
readProcessWithExitCode
    :: FilePath                              -- ^ command to run
    -> [String]                              -- ^ any arguments
    -> ByteString                            -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode = System.Process.Common.readProcessWithExitCode

readCreateProcessWithExitCode
    :: CreateProcess                         -- ^ command and arguments to run
    -> ByteString                            -- ^ standard input
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode = System.Process.Common.readCreateProcessWithExitCode
