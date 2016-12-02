{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.ByteString.Lazy where

#if __GLASGOW_HASKELL__ <= 709
import Control.Applicative ((<$>))
#endif
import Control.DeepSeq (force)
import qualified Control.Exception as C (evaluate)
import Data.ByteString.Lazy (ByteString, toChunks, fromChunks)
import Data.ListLike.IO (hGetContents)
import Data.Word (Word8)
import Prelude hiding (null)
import System.Process
import System.Process.Common
import System.Exit (ExitCode)

#if !MIN_VERSION_bytestring(0,10,0)
import Control.DeepSeq (NFData)

instance NFData ByteString
#endif

instance ProcessText ByteString Word8

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
