{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Keycloak.Client where

import Network.Wreq as W hiding (statusCode)
import Network.Wreq.Types
import Control.Lens
import Data.Aeson as JSON
import Data.Aeson.BetterErrors as AB
import Data.Text hiding (head, tail, map)
import Data.Text.Encoding
import Data.Maybe
import Control.Monad.Reader as R
import Keycloak.Types
import Network.HTTP.Client as HC hiding (responseBody)
import Network.HTTP.Types.Status
import Data.Monoid
import qualified Control.Monad.Catch as C
import Control.Monad.Except (throwError, catchError, MonadError)
import System.Log.Logger
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types.Method 
import Data.ByteString.Base64 as B64

checkPermission :: ResourceId -> Scope -> Maybe Token -> Keycloak ()
checkPermission res scope tok = do
  debug $ "Checking permissions: " ++ (show res) ++ " " ++ (show scope)
  client <- asks clientId
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "permission"  := res <> "#" <> scope]
  keycloakReqDef POST "protocol/openid-connect/token" (Just dat) tok AB.asValue
  return ()

isAuthorized :: ResourceId -> Scope -> Maybe Token -> Keycloak Bool
isAuthorized res scope tok = do
  r <- try $ checkPermission res scope tok
  case r of
    Right _ -> return True
    Left e | (statusCode <$> getErrorStatus e) == Just 403 -> return False
    Left e -> throwError e --rethrow the error

getAllPermissions :: Maybe Token -> Keycloak [Permission]
getAllPermissions tok = do
  debug "Get all permissions"
  client <- asks clientId
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "response_mode" := ("permissions" :: Text)]
  keycloakReqDef POST "protocol/openid-connect/token" (Just dat) tok (eachInArray parsePermission)

  
getUserAuthToken :: Text -> Text -> Keycloak Token
getUserAuthToken username password = do 
  debug "Get user token"
  client <- asks clientId
  secret <- asks clientSecret
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("password" :: Text),
             "password" := password,
             "username" := username]
  keycloakReq POST "protocol/openid-connect/token" (Just dat) Nothing (Token . encodeUtf8 <$> AB.key "access_token" asText) 

getClientAuthToken :: Keycloak Token
getClientAuthToken = do
  debug "Get client token"
  client <- asks clientId
  secret <- asks clientSecret
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("client_credentials" :: Text)]
  keycloakReqDef POST "protocol/openid-connect/token" (Just dat) Nothing (Token . encodeUtf8 <$> AB.key "access_token" asText) 

createResource :: Resource -> Maybe Token -> Keycloak ResourceId
createResource r mtok = do
  debug $ BS.unpack $ BL.toStrict $ "Creating resource: " <> (JSON.encode r)
  keycloakReqDef POST "authz/protection/resource_set" (Just $ toJSON r) mtok (AB.key "_id" asText) 

deleteResource :: ResourceId -> Maybe Token -> Keycloak ()
deleteResource rid tok = do
  keycloakReqDef DELETE ("authz/protection/resource_set/" <> rid) (Nothing :: Maybe BS.ByteString) tok (AB.asValue) 
  return ()

-- Perform post to Keycloak with token.
-- If there is no token, retrieve a guest token
keycloakReqDef :: (Postable dat, Show dat, Show b) => StdMethod -> Path -> Maybe dat -> Maybe Token -> Parse Text b -> Keycloak b
keycloakReqDef met path dat mtok parser = do
  (KCConfig baseUrl realm _ _ _ _ guestId guestPass) <- ask
  tok <- case mtok of
       Just tok -> return tok
       Nothing -> getUserAuthToken guestId guestPass
  keycloakReq met path dat (Just tok) parser 

-- Perform post to Keycloak.
keycloakReq :: (Postable dat, Show dat, Show b) => StdMethod -> Path -> Maybe dat -> Maybe Token -> Parse Text b -> Keycloak b
keycloakReq method path mdat mtok parser = do 
  (KCConfig baseUrl realm _ _ _ _ _ _) <- ask
  let opts = case mtok of
       Just tok -> W.defaults & W.header "Authorization" .~ ["Bearer " <> (unToken tok)]
       Nothing -> W.defaults
  let url = (unpack $ baseUrl <> "/realms/" <> realm <> "/" <> path) 
  info $ "Issuing KEYCLOAK " ++ (show method) ++ " with url: " ++ (show url) 
  debug $ "  data: " ++ (show mdat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ case mdat of
     Just dat -> W.customPayloadMethodWith (show method) opts url dat
     Nothing -> W.customMethodWith (show method) opts url
  case eRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> do
          debug $ "Keycloak success: " ++ (show ret) 
          return ret
        Left err2 -> do
          debug $ "Keycloak parse error: " ++ (show err2) 
          throwError $ ParseError $ pack (show err2)
    Left err -> do
      warn $ "Keycloak HTTP error: " ++ (show err)
      throwError $ HTTPError err

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s  = liftIO $ infoM "API" s
warn s  = liftIO $ warningM "API" s
err s   = liftIO $ errorM "API" s

getErrorStatus :: KCError -> Maybe Status 
getErrorStatus (HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = Just $ HC.responseStatus r
getErrorStatus _ = Nothing

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

getUsername :: Token -> Username
getUsername (Token tok) = undefined $ B64.decodeLenient tok 
