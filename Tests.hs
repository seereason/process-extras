{-# LANGUAGE FlexibleInstances #-}

import System.Exit (ExitCode(..), exitWith)
import System.Process.Run
import System.Process.ByteString ()
import System.Process.ByteString.Lazy ()
import System.Process.Text ()
import System.Process.Text.Lazy ()
import Test.HUnit

main :: IO ()
main = runTestTT (TestList [test0]) >>= \cts ->
       exitWith $ if errors cts + failures cts > 0
                  then ExitFailure 1
                  else ExitSuccess

instance Eq (Chunk String) where
    (Stdout a) == (Stdout b) = a == b
    (Stderr a) == (Stderr b) = a == b
    (Result a) == (Result b) = a == b
    _ == _ = False

-- Every fourth run these will say:
-- *System.Process.Run> _test4
-- -> ls
-- <interactive>: fd:12: hGetBuffering: illegal operation (handle is closed)
-- <interactive>: fd:14: hGetBuffering: illegal operation (handle is closed)
-- *** Exception: thread blocked indefinitely in an MVar operation

-- Oldest file in /usr/share/doc
file :: FilePath
file = "/usr/share/doc/cron/THANKS"
dir :: FilePath
dir = "/usr/share/doc/cron"

cmd :: CreateProcess
cmd = shell "echo a; echo b 1>&2; echo -n c"

omitProcessHandle :: [Chunk a] -> [Chunk a]
omitProcessHandle [] = []
omitProcessHandle (ProcessHandle _ : more) = omitProcessHandle more
omitProcessHandle (x : xs) = x : omitProcessHandle xs

test0 :: Test
test0 = TestCase $ do
          let expected = [Stdout "a\nc",Stderr "b\n",Result ExitSuccess]
          actual <- omitProcessHandle <$> runT (output >> run cmd "") :: IO [Chunk String]
          assertEqual "test1" expected actual

-- | What we want to test with these is what gets written to the console,
-- and I haven't yet invested the thought required to do that.  Divert the
-- console output somehow I guess...
_test1 :: IO [Chunk String]
_test1 = runT (output >> run (proc "ls" []) "")
_test2 :: IO [Chunk String]
_test2 = runT (vlevel 0 >> run (proc "ls" []) "")
_test2a :: IO (ExitCode, String, String)
_test2a = runT (silent >> run (proc "ls" []) "")
_test3 :: IO [Chunk String]
_test3 = runT (dots 10 >> run (proc "ls" []) "")
_test4 :: IO [Chunk String]
_test4 = runT (lazy >> indent (const "1> ") (const "2> ") >> run (proc "ls" []) "")
_test5 :: IO [Chunk String]
_test5 = runT (echoStart >> echoEnd >> run (proc "ls" []) "")
_test6 :: IO [Chunk String]
_test6 = (runT (silent >> echoStart >> echoEnd >> run (proc "yes" []) "") :: IO [Chunk String]) >>= return . take 2
