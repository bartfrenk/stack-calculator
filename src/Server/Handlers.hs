{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Server.Handlers where

import Control.Monad.Reader
import Control.Lens hiding (Context, index)
import Servant hiding (Context)

import qualified StackMap

data Context i a m = Context
  { _stackMap :: StackMap.Handle i a m
  }

makeLenses ''Context

push :: (MonadReader (Context i a IO) m, MonadIO m) => i -> a -> m NoContent
push index item = do
  sm <- view stackMap
  liftIO $ StackMap.push sm index item
  return NoContent

pop :: (MonadReader (Context i a IO) m, MonadIO m) => i -> m (Maybe a)
pop index = do
  sm <- view stackMap
  liftIO $ StackMap.pop sm index

size :: (MonadReader (Context i a IO) m, MonadIO m) => i -> m Integer
size index = do
  sm <- view stackMap
  liftIO $ StackMap.size sm index
