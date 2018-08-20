{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module Waziup.API
  where

import Waziup.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant-- (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData
import Keycloak.Client as KC
import qualified Orion.Client as O
import Control.Exception.Lifted
import Network.HTTP.Client hiding (Proxy)
import Network.Wreq hiding (Proxy)

-- | Servant type-level API
type WaziupAPI = AuthAPI :<|> SensorsAPI

type AuthAPI = "auth" :> "permissions" :> Get '[JSON] [Perm]
          :<|> "auth" :> "token" :> ReqBody '[JSON] AuthBody :> Post '[JSON] (Maybe Text)

type SensorsAPI = "sensors" :> Get '[JSON] [Sensor]
              :<|>"sensors" :> Capture "id" Text :> Get '[JSON] Sensor

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

server :: Server WaziupAPI
server = ((getPerms "cdupont" "password") :<|> postAuth)
     :<|> (getSensors :<|> getSensor)

getPerms :: Text -> Text -> ExceptT ServantErr IO [Perm]
getPerms username password = do
  mps <- liftIO $ getPermissions username password
  case mps of
    Just ps -> return $ map (\p -> Perm (rsname p) (map (\(Scope s) -> s) (scopes p))) ps
    Nothing -> return []

postAuth :: AuthBody -> ExceptT ServantErr IO (Maybe Text)
postAuth (AuthBody username password) = liftIO $ getUserAuthToken username password

getSensors :: ExceptT ServantErr IO [Sensor]
getSensors = liftIO $ O.getSensorsOrion

getSensor :: Text -> ExceptT ServantErr IO Sensor
getSensor id = getS `catch` handler where
  getS :: ExceptT ServantErr IO Sensor
  getS = do
    sensor <- (liftIO $ O.getSensorOrion id) -- `catch` (\(e:: HttpException) -> Left err404)
    case sensor of
      Just s -> return s
      Nothing -> throwError err404
  handler :: HttpException -> ExceptT ServantErr IO Sensor
  handler e = throwError err404
    

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

waziupServer :: Application
waziupServer = serve waziupAPI server
