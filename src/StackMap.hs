module StackMap where

-- |Abstract interface for an indexed collection of stacks.
data Handle i a m = Handle
  { push :: i -> a -> m ()
  , pop :: i -> m (Maybe a)
  , size :: i -> m Integer
  }

