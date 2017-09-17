{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Server where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.String
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  hiding (Context)
import           Server.API
import           Server.Handlers

import qualified StackMap

data Config = Config
  { port :: Int,
    host :: String
  }

data Handle = Handle
  { runServer :: IO ()
  }

-- |Wraps a synchronous stack map and wraps it in a server.
withHandle :: (FromHttpApiData i, FromHttpApiData a, ToJSON a)
           => Config -> StackMap.Handle i a IO -> (Server.Handle -> IO b) -> IO b
withHandle config sm cont =
  let h = Handle
        { runServer =
            let settings = mkSettings config
                ctx = Context sm
            in Warp.runSettings settings $
               serve stackMapAPI $ withContext ctx `enter` mkServer
        }
  in cont h

withContext :: Context i a IO -> (ReaderT (Context i a IO) Handler :~> Handler)
withContext ctx = NT (`runReaderT` ctx)

mkServer :: (MonadIO m, MonadReader (Context i a IO) m)
         => ServerT (StackMapAPI i a) m
mkServer index =
  push index :<|>
  pop index :<|>
  size index

mkSettings :: Config -> Warp.Settings
mkSettings Config{..} =
  let logger _ _ _ = return ()
      act = do
        modify $ Warp.setPort port
        modify $ Warp.setHost (fromString host)
        modify $ Warp.setLogger logger
  in execState act Warp.defaultSettings

