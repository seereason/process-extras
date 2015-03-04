{-# LANGUAGE TemplateHaskell #-}
module Utils where

import Control.Concurrent
import Control.Exception
import Data.Maybe (catMaybes)
import Language.Haskell.TH (Dec, Dec(InstanceD), Type(ConT, AppT), pprint, Q, reifyInstances)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | Apply a filter such as 'simpleMissingInstanceTest' to a list of
-- instances, resulting in a non-overlapping list of instances.
missingInstances :: (Dec -> Q Bool) -> Q [Dec] -> Q [Dec]
missingInstances test decs = decs >>= mapM (\ dec -> test dec >>= \ flag -> return $ if flag then (Just dec) else Nothing) >>= return . catMaybes

-- | Return True if no instance matches the types (ignoring the instance context)
simpleMissingInstanceTest :: Dec -> Q Bool
simpleMissingInstanceTest dec@(InstanceD _ typ _) =
    case unfoldInstance typ of
      Just (name, types) -> reifyInstances name types >>= return . null
      Nothing -> error $ "simpleMissingInstanceTest - invalid instance: " ++ pprint dec
    where
      unfoldInstance (ConT name) = Just (name, [])
      unfoldInstance (AppT t1 t2) = maybe Nothing (\ (name, types) -> Just (name, types ++ [t2])) (unfoldInstance t1)
      unfoldInstance _ = Nothing
simpleMissingInstanceTest dec = error $ "simpleMissingInstanceTest - invalid instance: " ++ pprint dec
