{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server.API where

import Data.Proxy
import           Servant

type StackAPI a =
  "push" :> Capture "item" a :> Patch '[JSON] NoContent :<|>
  "pop" :> Patch '[JSON] (Maybe a) :<|>
  "size" :> Get '[JSON] Integer

type StackMapAPI i a =
  Capture "index" i :> StackAPI a

stackMapAPI :: Proxy (StackMapAPI i a)
stackMapAPI = Proxy
