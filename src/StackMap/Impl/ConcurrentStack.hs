{-# LANGUAGE NoImplicitPrelude #-}
module StackMap.Impl.ConcurrentStack where

import           BasicPrelude hiding (read)
import           Control.Concurrent.Stack
import           Data.IORef
import qualified Data.Map.Strict as Map

import StackMap

type StackMap i a = Map i (Stack a)

withHandle :: Ord i => (StackMap.Handle i a IO -> IO b) -> IO b
withHandle cont = do
  ref <- newIORef Map.empty
  cont StackMap.Handle
    { push = \i a -> ref `modify` pushM i a
    , pop = \i -> ref `modify` popM i
    , size = \i -> ref `read` sizeM i
    }

pushM :: Ord i => i -> a -> StackMap i a -> IO (Maybe (StackMap i a), ())
pushM index item sm =
  case Map.lookup index sm of -- not safe when creating new stacks
    Nothing -> do
      singleton <- stackNew
      stackPush singleton item
      return (Just $ Map.insert index singleton sm, ())
    Just stack -> do
      stackPush stack item
      return (Just $ Map.insert index stack sm, ())

popM :: Ord i => i -> StackMap i a -> IO (Maybe (StackMap i a), Maybe a)
popM index sm =
  case Map.lookup index sm of
    Nothing -> return (Nothing, Nothing)
    Just stack -> do
      item <- stackTryPop stack
      next <- stackTryPeek stack
      case next of
        Nothing -> -- remove stack when it is empty
          return (Just $ Map.delete index sm, item)
        Just _ ->
          return (Just $ Map.insert index stack sm, item)

sizeM :: Ord i => i -> StackMap i a -> IO Integer
sizeM index sm =
  case Map.lookup index sm of
    Nothing -> return 0
    Just stack -> toInteger <$> stackSize stack

modify :: IORef a -> (a -> IO (Maybe a, b)) -> IO b
modify ref f = do
  a <- readIORef ref
  (a', b) <-  f a
  case a' of
    Nothing -> return b
    Just m -> do
      writeIORef ref m
      return b

read :: IORef a -> (a -> IO b) -> IO b
read ref f = readIORef ref >>= f

