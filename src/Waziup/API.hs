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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module Waziup.API
  where

import Waziup.Types

import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Aeson
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text hiding (map)
import Data.Text.Encoding
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Client as HC hiding (Proxy)
import Network.HTTP.Types.Status as HTS
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant-- (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData
import Keycloak as KC hiding (info, warn, debug, error) 
import qualified Orion as O
import Network.Wreq hiding (Proxy)
import Control.Monad.Reader
import Control.Monad.Catch as C
import System.Log.Logger
import Web.HttpApiData
import qualified Data.ByteString.Lazy as BL 

-- | Servant type-level API
type WaziupAPI = "api" :> "v1" :> (AuthAPI :<|> SensorsAPI)

type AuthAPI = 
  "auth" :>  ("permissions" :> Get '[JSON] [Perm]
         :<|> "token"       :> ReqBody '[JSON] AuthBody :> Post '[PlainText] KC.Token)

type SensorsAPI = 
  "sensors" :>  (Get '[JSON] [Sensor]
            :<|> ReqBody '[JSON] Sensor :> PostNoContent '[JSON] NoContent
            :<|> Capture "id" Text :> Header "Authorization" KC.Token  :> Get '[JSON] Sensor)


instance FromHttpApiData KC.Token where
  parseHeader ((stripPrefix "Bearer ") . decodeUtf8 -> Just tok) = Right $ KC.Token tok

instance MimeRender PlainText KC.Token where
  mimeRender _ (KC.Token tok) = BL.fromStrict $ encodeUtf8 tok

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String   -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

server :: Server WaziupAPI
server = authServer 
    :<|> sensorsServer

authServer :: Server AuthAPI
authServer = (getPerms "cdupont" "password") 
        :<|> postAuth

sensorsServer :: Server SensorsAPI
sensorsServer = getSensors 
           :<|> postSensor
           :<|> getSensor

getPerms :: Username -> Password -> ExceptT ServantErr IO [Perm]
getPerms username password = do
  liftIO $ putStrLn "Get token"
  tok <- runKeycloak $ getUserAuthToken username password
  liftIO $ putStrLn "Get Permissions"
  ps <- runKeycloak (getAllPermissions tok)
  let getP :: KC.Permission -> Perm
      getP (KC.Permission rsname _ scopes) = Perm rsname (mapMaybe readScope scopes)
  return $ map getP ps 

postAuth :: AuthBody -> ExceptT ServantErr IO KC.Token
postAuth (AuthBody username password) = do
  tok <- runKeycloak (getUserAuthToken username password)
  liftIO $ putStrLn $ show tok
  return tok

getSensors :: ExceptT ServantErr IO [Sensor]
getSensors = runOrion O.getSensorsOrion

getSensor :: SensorId -> Maybe KC.Token -> ExceptT ServantErr IO Sensor
getSensor sid (Just tok) = do
--getSensor sid (Just (stripPrefix "Bearer " -> Just tok)) = do
  isAuth <- runKeycloak (isAuthorized sid (pack $ show SensorsView) tok)
  debug $ "is auth:" ++ (show isAuth)
  if isAuth 
    then runOrion (O.getSensorOrion sid)
    else throwError err403 {errBody = "Sensor not authorized"}
getSensor sid Nothing = runOrion (O.getSensorOrion sid)

postSensor :: Sensor -> ExceptT ServantErr IO NoContent
postSensor s@(Sensor id _ _ _ _ _ _ _ _ vis _) = do
  let res = KC.Resource {
     resId      = Nothing,
     resName    = id,
     resType    = Nothing,
     resUris    = [],
     resScopes  = map (pack.show) [SensorsView, SensorsUpdate, SensorsDelete, SensorsDataCreate, SensorsDataView],
     resOwner   = Owner Nothing "cdupont",
     resOwnerManagedAccess = True,
     resAttributes = if (isJust vis) then [Attribute "visibility" [pack $ show $ fromJust vis]] else []
     }
  debug "Get token"
  tok <- runKeycloak getClientAuthToken
  debug "Create resource"
  resId <- runKeycloak (createResource res tok)
  debug "Create entity"
  res <- C.try $ runOrion (O.postSensorOrion (s {senKeycloakId = Just resId}))
  case res of
    Right _ -> return NoContent
    Left (err :: HttpException) -> do
      warn "Orion error"
      return NoContent
 

runOrion :: O.Orion a -> ExceptT ServantErr IO a
runOrion orion = withExceptT fromOrionError (runReaderT orion O.defaultOrionConfig)

runKeycloak :: KC.Keycloak a -> ExceptT ServantErr IO a
runKeycloak kc = withExceptT fromKCError (runReaderT kc defaultConfig)

fromOrionError :: O.OrionError -> ServantErr
fromOrionError (O.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = ServantErr { errHTTPCode = HTS.statusCode $ HC.responseStatus r, 
                                                                                           errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                                                                                           errBody = "",
                                                                                           errHeaders = []}
fromOrionError (O.HTTPError (HttpExceptionRequest _ (ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Orion: " ++ show a} 
fromOrionError (O.HTTPError (HttpExceptionRequest _ s)) = err500 {errBody = encode $ show s} 
fromOrionError (O.ParseError s) = err500 {errBody = encode s} 
fromOrionError O.EmptyError = err500 {errBody = "EmptyError"}

fromKCError :: KC.KCError -> ServantErr
fromKCError (KC.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = ServantErr { errHTTPCode = HTS.statusCode $ HC.responseStatus r, 
                                                                                           errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                                                                                           errBody = "",
                                                                                           errHeaders = []}
fromKCError (KC.HTTPError (HttpExceptionRequest _ (ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Keycloak: " ++ show a} 
fromKCError (KC.ParseError s) = err500 {errBody = encode s} 
fromKCError KC.EmptyError = err500 {errBody = "EmptyError"}

waziupAPI :: Proxy WaziupAPI
waziupAPI = Proxy

waziupServer :: Application
waziupServer = serve waziupAPI server

warn, info :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s = liftIO $ infoM "API" s
warn s = liftIO $ warningM "API" s
err s = liftIO $ errorM "API" s
