{-# LANGUAGE NoImplicitPrelude #-}
module StackMap.Impl.CTrie where

import BasicPrelude hiding (Map)
import Control.Concurrent.Map (Map)
import qualified Control.Concurrent.Map as Map

import StackMap

withHandle :: (Eq i, Hashable i) => (StackMap.Handle i a IO -> IO b) -> IO b
withHandle cont = do
  m <- Map.empty
  cont StackMap.Handle
    { push = pushM m
    , pop = popM m
    , size = sizeM m
    }

pushM :: (Eq i, Hashable i) => Map i [a] -> i -> a -> IO ()
pushM m index item = do
  stack' <- Map.lookup index m
  case stack' of
    Nothing -> Map.insert index [item] m
    Just stack -> Map.insert index (item:stack) m

popM :: (Eq i, Hashable i) => Map i [a] -> i -> IO (Maybe a)
popM m index = do
  stack' <- Map.lookup index m
  case stack' of
    Nothing -> return Nothing
    Just [] -> do
      Map.delete index m
      return Nothing
    Just [item] -> do
      Map.delete index m
      return $ Just item
    Just (item:rest) -> do
      Map.insert index rest m
      return $ Just item

sizeM :: (Eq i, Hashable i) => Map i [a] -> i -> IO Integer
sizeM m index = do
  stack' <- Map.lookup index m
  case stack' of
    Nothing -> return 0
    Just xs -> return $ toInteger (length xs)


