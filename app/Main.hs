{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import           Control.Monad.Managed
import           Options.Applicative

import qualified Server
import qualified StackMap
import qualified StackMap.Impl.ConcurrentStack as STACK
import qualified StackMap.Impl.CTrie           as CTRIE
import qualified StackMap.Impl.STM             as HAMT_STM


data Impl
  = HAMT_STM
  | CTRIE
  | STACK_STM

opts :: ParserInfo Impl
opts = (helper <*> parser) `info` progDesc "LemonPI stack calculator"
  where parser =
          option (eitherReader impl) (
          short 'i' <>
          metavar "IMPL" <>
          help "Which map of stacks implementation to use" <>
          value HAMT_STM)
        impl "hamt"  = Right HAMT_STM
        impl "ctrie" = Right CTRIE
        impl "stack" = Right STACK_STM
        impl _       = Left "not an implementation"



serverC :: Server.Config
serverC = Server.Config
  { Server.port = 4444
  , Server.host = "127.0.0.1"
  }

stackMapWithHandle :: Impl -> (StackMap.Handle Int Double IO -> IO b) -> IO b
stackMapWithHandle HAMT_STM  = HAMT_STM.withHandle
stackMapWithHandle CTRIE     = CTRIE.withHandle
stackMapWithHandle STACK_STM = STACK.withHandle

main :: IO ()
main = do
  impl <- execParser opts
  void $ runManaged $ do
    stackMapH <- managed $ stackMapWithHandle impl
    serverH <- managed $ Server.withHandle serverC stackMapH
    liftIO (Server.runServer serverH)
