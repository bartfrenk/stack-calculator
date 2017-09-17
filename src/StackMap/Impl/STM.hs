module StackMap.Impl.STM where

import STMContainers.Map (Map, Key)
import qualified STMContainers.Map as Map
import Control.Concurrent.STM

import StackMap

withHandle :: Key i => (StackMap.Handle i a IO -> IO b) -> IO b
withHandle cont = do
  m <- Map.newIO
  cont StackMap.Handle
    { push = \i a -> atomically $ pushM m i a
    , pop = atomically . popM m
    , size = atomically . sizeM m
    }

pushM :: Key i => Map i [a] -> i -> a -> STM ()
pushM m index item = do
  stack' <- Map.lookup index m
  case stack' of
    Nothing -> Map.insert [item] index m
    Just xs -> Map.insert (item:xs) index m

popM :: Key i => Map i [a] -> i -> STM (Maybe a)
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
      Map.insert rest index m
      return $ Just item

sizeM :: Key i => Map i [a] -> i -> STM Integer
sizeM m index = do
  stack' <- Map.lookup index m
  case stack' of
    Nothing -> return 0
    Just xs -> return $ toInteger (length xs)
