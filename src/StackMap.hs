module StackMap where

-- |Abstract interface for an indexed collection of stacks.
data Handle index value m = Handle
  { push :: index -> value -> m ()
  , pop :: index -> m (Maybe value)
  , size :: index -> m Integer
  }

