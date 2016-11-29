-- | Flexible control of progress reporting for readCreateProcess and friends.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Process.Run
    ( OutputStyle(..)
    , RunState(..)
    , run
    , echoStart
    , echoEnd
    , output
    , silent
    , dots
    , indent
    ) where

#if __GLASGOW_HASKELL__ <= 709
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mempty)
#endif
import Control.Monad (when)
import Control.Monad.State (evalState, evalStateT, get, modify, MonadState, put, StateT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Default (Default(def))
import Data.ListLike as ListLike (break, fromList, head, hPutStr, length, ListLike, ListLikeIO,
                                  null, putStr, singleton, tail)
import Data.Monoid ((<>))
import Data.String (IsString, fromString)
import System.Exit (ExitCode(..))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process.ListLike
    (Chunk(..), ProcessMaker, ProcessOutput, ListLikeProcessIO, collectOutput, proc, readCreateProcessLazy,
     showProcessMakerForUser, writeChunk, writeOutput)

-- | This is the state record that controls the output style.
data RunState t
    = RunState
      { _output :: OutputStyle -- ^ Overall style of output
      , _outprefix :: t        -- ^ Prefix for lines of stdout
      , _errprefix :: t        -- ^ Prefix for lines of stderr
      , _echoStart :: Bool     -- ^ Echo command as process starts
      , _echoEnd :: Bool       -- ^ Echo command as process finishes
      }

data OutputStyle
    = Dots Int  -- ^ Output one dot per n output characters
    | All       -- ^ send process stdout to console stdout and process stderr to console stderr
    | Indented  -- ^ Output with prefixes
    | Silent    -- ^ No output

instance Monoid t => Default (RunState t) where
    def = RunState { _outprefix = mempty
                   , _errprefix = mempty
                   , _output = All
                   , _echoStart = False
                   , _echoEnd = False }

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

indent :: (MonadIO m, MonadState (RunState t) m, ListLike t item) => (t -> t) -> (t -> t) -> m ()
indent so se = modify $ \x ->
    let so' = so (_outprefix x)
        se' = se (_errprefix x) in
    x { _outprefix = so'
      , _errprefix = se'
      , _output = if ListLike.null so' &&
                     ListLike.null se' then _output x else Indented }

class Dot c where
    dot :: c

instance Dot Char where
    dot = '.'

run' :: forall m maker text char.
        (MonadIO m, Monoid text, IsString text, Dot char, Eq char,
         MonadState (RunState text) m,
         ProcessMaker maker,
         ListLikeProcessIO text char) =>
        maker -> text -> m [Chunk text]
run' maker input = do
  st0 <- get
  when (_echoStart st0) (liftIO $ hPutStrLn stderr ("-> " ++ showProcessMakerForUser maker))
  result <- liftIO $ readCreateProcessLazy maker input >>= doOutput st0
  when (_echoEnd st0) (liftIO $ hPutStrLn stderr ("<- " ++ showProcessMakerForUser maker))
  return result
    where
      doOutput :: RunState text -> [Chunk text] -> IO [Chunk text]
      doOutput (RunState {_output = Dots n}) cs = putDotsLn n cs
      doOutput (RunState {_output = Silent}) cs = return cs
      doOutput (RunState {_output = All}) cs = writeOutput cs
      doOutput (RunState {_output = Indented, _outprefix = outp, _errprefix = errp}) cs = writeOutputIndented outp errp cs

run :: forall m maker text char result.
       (MonadIO m, IsString text, Dot char, Eq char,
        MonadState (RunState text) m,
        ProcessMaker maker,
        ProcessOutput text result,
        ListLikeProcessIO text char) =>
       maker -> text -> m result
run maker input = run' maker input >>= return . collectOutput

-- | Output the dotified text of a chunk list with a newline at EOF.
-- Returns the original list.
putDotsLn :: forall text char. (ListLikeProcessIO text char, IsString text, Dot char) =>
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
dotifyChunk :: forall text char m. (Monad m, Functor m, ListLikeProcessIO text char, Dot char) =>
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
      Stdout x -> (chunk,) <$> doText Stdout outp x
      Stderr x -> (chunk,) <$> doText Stderr errp x
      _ -> return (chunk, [chunk])
    where
      -- doText :: (a -> Chunk a) -> a -> a -> StateT BOL m [Chunk a]
      doText con pre x = do
        let (hd, tl) = ListLike.break (== nl) x
        (<>) <$> doHead con pre hd <*> doTail con pre tl
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

_test1 :: IO [Chunk String]
_test1 = evalStateT (run (proc "ls" []) "") def
_test2 :: IO [Chunk String]
_test2 = evalStateT (silent >> run (proc "ls" []) "") def
_test2a :: IO (ExitCode, String, String)
_test2a = evalStateT (silent >> run (proc "ls" []) "") def
_test3 :: IO [Chunk String]
_test3 = evalStateT (dots 10 >> run (proc "ls" []) "") def
_test4 :: IO [Chunk String]
_test4 = evalStateT (indent (<> "1> ") (<> "2> ") >> run (proc "ls" []) "") def
_test5 :: IO [Chunk String]
_test5 = evalStateT (echoStart >> echoEnd >> run (proc "ls" []) "") def
_test6 :: IO [Chunk String]
_test6 = take 2 <$> evalStateT (silent >> echoStart >> echoEnd >> run (proc "yes" []) "") def :: IO [Chunk String]
