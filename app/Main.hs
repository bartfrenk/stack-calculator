{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import           Control.Monad.Managed

import qualified Server
import qualified StackMap
import qualified StackMap.Impl.STM     as StackMap



serverC :: Server.Config
serverC = Server.Config
  { Server.port = 4444
  , Server.host = "127.0.0.1"
  }

stackMapWithHandle :: (StackMap.Handle Int Double IO -> IO b) -> IO b
stackMapWithHandle = StackMap.withHandle

main :: IO ()
main =
  void $ runManaged $ do
    stackMapH <- managed stackMapWithHandle
    serverH <- managed $ Server.withHandle serverC stackMapH
    liftIO (Server.runServer serverH)
