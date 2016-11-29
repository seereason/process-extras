# What is process-extras

You might want to use this package if
 * You want to read and write ByteStrings or Text to a process rather
   than just Strings
 * You want to read output from a non-terminating process (e.g. yes(1))
 * You want more flexibility in process creation
 * You want various types of progress output from the process to the
   console before capturing its output - indented and prefixed output,
   reporting of the command that started the process, dots to reflect
   the size of the process output, etc.

# Contributing

This project is available on [GitHub](https://github.com/seereason/process-extras). You may contribute changes there.

Please report bugs and feature requests using the [GitHub issue tracker](https://github.com/seereason/process-extras/issues).

# Examples:

The output type of the raw system process functions is ByteString.
Instances of ListLikeProcessIO are provided to read as type String,
Text, Lazy Text, ByteString, or Lazy ByteString.  Select by casting
the result, or by specifying the module containing the specialized
function:

    > :m +System.Process.ListLike Data.ByteString Data.Text.Lazy
    > readCreateProcessWithExitCode (shell "echo 'λ'") mempty :: IO (ExitCode, ByteString, ByteString)
    (ExitSuccess,"\206\187\n","")
    > readCreateProcessWithExitCode (shell "echo 'λ'") mempty :: IO (ExitCode, Text, Text)
    (ExitSuccess,"\955\n","")
    > readCreateProcessWithExitCode (shell "echo 'λ'") mempty :: IO (ExitCode, String, String)
    (ExitSuccess,"\955\n","")
    > System.Process.Text.readCreateProcessWithExitCode (shell "yes | head -10") mempty
    (ExitSuccess,"y\ny\ny\ny\ny\ny\ny\ny\ny\ny\n","")

Although the output *type* can be lazy, normal process functions still
need to read until EOF on the process output before returing anything.
If you have a process whose output never ends you can use the
readCreateProcessLazy function to read it.  Functions like readProcess
would block waiting for EOF on the process output:

    > (Prelude.take 4 <$> readCreateProcessLazy (proc "yes" []) mempty :: IO [Chunk Text]) >>= mapM_ (putStrLn . show)
    ProcessHandle <process>
    Stdout "y\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny ..."
    ...

The output type can be any instance of ProcessOutput, instances for
types (ExitCode, a, a), [Chunk a], and (ExitCode, [Chunk a]) are
provided.  [Chunk a] can be converted to any other instance of
ProcessOutput using collectOutput

    > (readCreateProcess (shell "gzip -v < /proc/uptime") mempty :: IO [Chunk ByteString]) >>= mapM_ (putStrLn . show)
    Stdout "\US\139\b\NUL\237\136\&7W\NUL\ETX345\183\&403\215\&31Q04267\177\&0\177\212\&33\225\STX\NUL_\169\142\178\ETB\NUL\NUL\NUL"
    Stderr "gzip: stdin: file size changed while zipping\n -8.7%\n"
    Result ExitSuccess
    > (readCreateProcess (shell "uptime") mempty :: IO [Chunk ByteString]) >>= writeOutput
     14:00:34 up 18 days,  7:16,  6 users,  load average: 0.04, 0.10, 0.08
    > collectOutput <$> (readCreateProcess (shell "gzip -v < /proc/uptime") mempty :: IO [Chunk ByteString]) :: IO (ExitCode, ByteString, ByteString)
    (ExitSuccess,"\US\139\b\NUL\185\137\&7W\NUL\ETX345\183\&427\212\&33W0426731\177\208\&35\225\STX\NUL\237\192\CAN\224\ETB\NUL\NUL\NUL","gzip: stdin: file size changed while zipping\n -8.7%\n")
    > collectOutput <$> (readCreateProcess (shell "gzip -v < /proc/uptime") mempty :: IO [Chunk ByteString]) :: IO (ExitCode, ByteString, ByteString)
    (ExitSuccess,"\US\139\b\NUL\185\137\&7W\NUL\ETX345\183\&427\212\&33W0426731\177\208\&35\225\STX\NUL\237\192\CAN\224\ETB\NUL\NUL\NUL","gzip: stdin: file size changed while zipping\n -8.7%\n")
    > (collectOutput . filter (\x -> case x of Stderr _ -> False; _ -> True)) <$> (readCreateProcess (shell "gzip -v < /proc/uptime") mempty :: IO [Chunk ByteString]) :: IO (ExitCode, ByteString, ByteString)
    (ExitSuccess,"\US\139\b\NUL<\138\&7W\NUL\ETX345\183\&410\210\&3\176P04267713\213\&37\224\STX\NULT\142\EOT\165\ETB\NUL\NUL\NUL","")

Some cases that need investigation:

    > (readCreateProcess (shell "gzip -v < /proc/uptime") mempty :: IO [Chunk String]) >>= mapM_ (putStrLn . show)
    *** Exception: fd:13: hGetContents: invalid argument (invalid byte sequence)
    > (readCreateProcess (shell "gzip -v < /proc/uptime") mempty :: IO [Chunk Text]) >>= mapM_ (putStrLn . show)
    *** Exception: fd:13: hClose: invalid argument (Bad file descriptor)
