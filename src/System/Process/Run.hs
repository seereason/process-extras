-- | Flexible control of progress reporting for readCreateProcess and friends.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Process.Run
    ( 
    -- * Monad transformer
      RunT
    , runT
    , RunState(..)
    , OutputStyle(..)
    -- * Monad class
    , RunM
    -- * Modify moand RunM state parameters
    , echoStart
    , echoEnd
    , output
    , silent
    , dots
    , indent
    , vlevel
    , quieter
    , noisier
    , lazy
    , strict
    , message
    -- * Monadic process runner
    , run
    -- * Re-exports
    , module System.Process.ListLike
    ) where

#if __GLASGOW_HASKELL__ <= 709
import Data.Monoid (Monoid, mempty)
#endif
import Control.Monad (when)
import Control.Monad.State (evalState, evalStateT, get, modify, MonadState, put, StateT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Char (ord)
import Data.Default (Default(def))
import Data.ListLike as ListLike (break, fromList, head, hPutStr, length, ListLike, ListLikeIO,
                                  null, putStr, singleton, tail)
import Data.Monoid ((<>))
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Text.Lazy as Lazy (Text)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process.ListLike

-- | This is the state record that controls the output style.
data RunState text
    = RunState
      { _output :: OutputStyle -- ^ Overall style of output
      , _outprefix :: text     -- ^ Prefix for lines of stdout
      , _errprefix :: text     -- ^ Prefix for lines of stderr
      , _echoStart :: Bool     -- ^ Echo command as process starts
      , _echoEnd :: Bool       -- ^ Echo command as process finishes
      , _verbosity :: Int      -- ^ A progression of progress modes
      , _lazy :: Bool          -- ^ Use the lazy or strict runner?
      , _message :: text       -- ^ Extra text for start/end message - e.g. the change root
      }

type RunT text m = StateT (RunState text) m

class (MonadState (RunState text) m,
       ProcessText text char,
       ListLikeProcessIO text char,
       MonadIO m, IsString text, Eq char, Dot char) =>
    RunM text char m

instance Dot Word8 where
    dot = fromIntegral (ord '.')

instance (MonadIO m, MonadState (RunState String) m) => RunM String Char m
instance (MonadIO m, MonadState (RunState Text) m) => RunM Text Char m
instance (MonadIO m, MonadState (RunState Lazy.Text) m) => RunM Lazy.Text Char m
instance (MonadIO m, MonadState (RunState ByteString) m) => RunM ByteString Word8 m
instance (MonadIO m, MonadState (RunState Lazy.ByteString) m) => RunM Lazy.ByteString Word8 m

runT :: forall m text char a. (MonadIO m, ProcessText text char) => RunT text m a -> m a
runT action = evalStateT action (def :: RunState text)

data OutputStyle
    = Dots Int  -- ^ Output one dot per n output characters
    | All       -- ^ send process stdout to console stdout and process stderr to console stderr
    | Indented  -- ^ Output with prefixes
    | Silent    -- ^ No output

instance ProcessText text char => Default (RunState text) where
    def = RunState { _outprefix = fromString "1> "
                   , _errprefix = fromString "2> "
                   , _output = All
                   , _echoStart = True
                   , _echoEnd = True
                   , _verbosity = 3
                   , _lazy = False
                   , _message = mempty }

{-
class (Monoid text, MonadIO m) => MonadRun m text where
    type Text m
    getRunState :: m (RunState text)
    putRunState :: RunState text -> m ()

instance Monoid text => MonadRun IO text where
    getRunState = return def
    putRunState _ = return ()

instance (MonadIO m, Monoid t, MonadState (RunState t) m) => MonadRun m t where
    getRunState = get
    putRunState = put
-}

noEcho :: (MonadIO m, MonadState (RunState t) m) => m ()
noEcho = modify (\x -> x { _echoStart = False, _echoEnd = False })

echoStart :: (MonadIO m, MonadState (RunState t) m) => m ()
echoStart = modify (\x -> x { _echoStart = True })

echoEnd :: (MonadIO m, MonadState (RunState t) m) => m ()
echoEnd = modify (\x -> x { _echoEnd = True })

output :: (MonadIO m, MonadState (RunState t) m) => m ()
output = modify (\x -> x { _output = All })

silent :: (MonadIO m, MonadState (RunState t) m) => m ()
silent = modify (\x -> x { _output = Silent })

dots :: (MonadIO m, MonadState (RunState t) m) => Int -> m ()
dots n = modify (\x -> x { _output = Dots n })

-- | Modify the indentation prefixes for stdout and stderr in the
-- progress monad.
indent :: (MonadIO m, MonadState (RunState t) m, ListLike t char) => (t -> t) -> (t -> t) -> m ()
indent so se = modify $ \x ->
    let so' = so (_outprefix x)
        se' = se (_errprefix x) in
    x { _outprefix = so'
      , _errprefix = se'
      , _output = if ListLike.null so' &&
                     ListLike.null se' then _output x else Indented }

noIndent :: (MonadIO m, MonadState (RunState text) m, ListLike text char) => m ()
noIndent = indent (const mempty) (const mempty)

-- | Set verbosity to a specific level from 0 to 3.
-- vlevel :: (MonadIO m, Monoid text, MonadState (RunState text) m) => Int -> m ()
-- vlevel :: forall m text char. (IsString text, ListLike text char, MonadIO m) => Int -> m ()
vlevel :: forall m text char.
          (IsString text, ListLike text char, MonadIO m, MonadState (RunState text) m) =>
          Int -> m ()
vlevel n = do
  modify (\x -> x {_verbosity = n})
  case n of
    _ | n <= 0 -> noEcho >> silent >> noIndent -- No output
    1 -> vlevel 0 >> echoStart                 -- Output command at start
    2 -> vlevel 1 >> echoEnd >> dots 100       -- Output command at start and end, dots to show output
    _ ->                                       -- echo command at start and end, and send all output
                                               -- to the console with channel prefixes 1> and 2>
          vlevel 2 >> output >> indent (const (fromString "1> ")) (const (fromString ("2> ")))

quieter :: RunM text char m => m ()
quieter = get >>= \x -> vlevel (_verbosity x - 1)

noisier :: RunM text char m => m ()
noisier = get >>= \x -> vlevel (_verbosity x + 1)

strict :: RunM text char m => m ()
strict = modify (\x -> x { _lazy = False })

lazy :: RunM text char m => m ()
lazy = modify (\x -> x { _lazy = True})

message :: RunM text char m => (text -> text) -> m ()
message f = modify (\x -> x { _message = f (_message x) })

class Dot c where
    dot :: c

instance Dot Char where
    dot = '.'

run' :: forall m maker text char.
        (RunM text char m,
         ProcessMaker maker) =>
        maker -> text -> m [Chunk text]
run' maker input = do
  st0 <- get
  when (_echoStart st0) (liftIO $ hPutStrLn stderr ("-> " ++ showProcessMakerForUser maker))
  result <- liftIO $ (if _lazy st0 then readCreateProcessLazy else readCreateProcess) maker input >>= doOutput st0
  when (_echoEnd st0) (liftIO $ hPutStrLn stderr ("<- " ++ showProcessMakerForUser maker))
  return result
    where
      doOutput :: RunState text -> [Chunk text] -> IO [Chunk text]
      doOutput (RunState {_output = Dots n}) cs = putDotsLn n cs
      doOutput (RunState {_output = Silent}) cs = return cs
      doOutput (RunState {_output = All}) cs = writeOutput cs
      doOutput (RunState {_output = Indented, _outprefix = outp, _errprefix = errp}) cs = writeOutputIndented outp errp cs

run :: forall m maker text char result.
       (RunM text char m,
        ProcessMaker maker,
        ProcessResult text result) =>
       maker -> text -> m result
run maker input = run' maker input >>= return . collectOutput

-- | Output the dotified text of a chunk list with a newline at EOF.
-- Returns the original list.
putDotsLn :: (ListLikeProcessIO text char, IsString text, Dot char) =>
             Int -> [Chunk text] -> IO [Chunk text]
putDotsLn cpd chunks = putDots cpd chunks >>= \ r -> System.IO.hPutStr stderr "\n" >> return r

-- | Output the dotified text of a chunk list. Returns the original
-- (undotified) list.
putDots :: (ListLikeProcessIO text char, Dot char) => Int -> [Chunk text] -> IO [Chunk text]
putDots charsPerDot chunks =
    evalStateT (mapM (\ x -> dotifyChunk charsPerDot x >>= mapM_ (lift . putChunk) >> return x) chunks) 0

-- | dotifyChunk charsPerDot dot chunk - Replaces every charsPerDot
-- characters in the Stdout and Stderr chunks with one dot.  Runs in
-- the state monad to keep track of how many characters had been seen
-- when the previous chunk finished.  chunks.
dotifyChunk :: forall text char m. (Monad m, ListLike text char, Dot char) =>
               Int -> Chunk text -> StateT Int m [Chunk text]
dotifyChunk charsPerDot chunk =
    case chunk of
      Stdout x -> doChars (ListLike.length x)
      Stderr x -> doChars (ListLike.length x)
      _ -> return [chunk]
    where
      doChars :: Int -> StateT Int m [Chunk text]
      doChars count = do
        remaining <- get
        let (count', remaining') = divMod (remaining + count) (fromIntegral charsPerDot)
        put remaining'
        if (count' > 0) then return [Stderr (ListLike.fromList (replicate count' dot))] else return []

-- | Write the Stdout chunks to stdout and the Stderr chunks to stderr.
putChunk :: ListLikeProcessIO text char => Chunk text -> IO ()
putChunk (Stdout x) = ListLike.putStr x
putChunk (Stderr x) = ListLike.hPutStr stderr x
putChunk _ = return ()

writeOutputIndented :: (ListLikeIO text char, ListLikeProcessIO text char, Eq char, IsString text) =>
                       text -> text -> [Chunk text] -> IO [Chunk text]
writeOutputIndented outp errp chunks =
    mapM (\(c, cs) -> mapM_ writeChunk cs >> return c) (indentChunks outp errp chunks)

-- | Pure function to indent the text of a chunk list.
indentChunks :: forall text char. (ListLikeProcessIO text char, Eq char, IsString text) =>
                text -> text -> [Chunk text] -> [(Chunk text, [Chunk text])]
indentChunks outp errp chunks =
    evalState (mapM (indentChunk nl outp errp) chunks) BOL
    where
      nl :: char
      nl = ListLike.head (fromString "\n" :: text)

-- | The monad state, are we at the beginning of a line or the middle?
data BOL = BOL | MOL deriving (Eq)

-- | Indent the text of a chunk with the prefixes given for stdout and
-- stderr.  The state monad keeps track of whether we are at the
-- beginning of a line - when we are and more text comes we insert one
-- of the prefixes.
indentChunk :: forall m text char.
               (Eq char, ListLike text char, MonadState BOL m) =>
               char -> text -> text -> Chunk text -> m (Chunk text, [Chunk text])
indentChunk nl outp errp chunk =
    case chunk of
      Stdout x -> doText Stdout outp x >>= return . (chunk,)
      Stderr x -> doText Stderr errp x >>= return . (chunk,)
      _ -> return (chunk, [chunk])
    where
      -- doText :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doText con pre x = do
        let (hd, tl) = ListLike.break (== nl) x
        hd' <- doHead con pre hd
        tl' <- doTail con pre tl
        return $ hd' <> tl'
      -- doHead :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doHead _ _ x | ListLike.null x = return []
      doHead con pre x = do
        bol <- get
        case bol of
          BOL -> put MOL >> return [con (pre <> x)]
          MOL -> return [con x]
      -- doTail :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doTail _ _ x | ListLike.null x = return []
      doTail con pre x = do
        bol <- get
        put BOL
        tl <- doText con pre (ListLike.tail x)
        return $ (if bol == BOL then [con pre] else []) <> [con (singleton nl)] <> tl
