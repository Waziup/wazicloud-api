{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Keycloak.Client where

import Network.Wreq as W hiding (statusCode)
import Network.Wreq.Types
import Control.Lens
import Control.Monad
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
import Control.Monad.Except (ExceptT, throwError, catchError, MonadError)
import System.Log.Logger
 

checkPermission :: ResourceId -> Scope -> Maybe Token -> Keycloak ()
checkPermission res scope tok = do 
  client <- asks clientId
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "permission"  := res <> "#" <> scope]
  keycloakPost "protocol/openid-connect/token" dat tok AB.asValue
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
  client <- asks clientId
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "response_mode" := ("permissions" :: Text)]
  keycloakPost "protocol/openid-connect/token" dat tok (eachInArray parsePermission)

  
getUserAuthToken :: Text -> Text -> Keycloak Token
getUserAuthToken username password = do 
  client <- asks clientId
  secret <- asks clientSecret
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("password" :: Text),
             "password" := password,
             "username" := username]
  keycloakPost "protocol/openid-connect/token" dat Nothing (Token <$> AB.key "access_token" asText) 

getClientAuthToken :: Keycloak Token
getClientAuthToken = do
  client <- asks clientId
  secret <- asks clientSecret
  let dat = ["client_id" := client, 
             "client_secret" := secret,
             "grant_type" := ("client_credentials" :: Text)]
  keycloakPost "protocol/openid-connect/token" dat Nothing (Token <$> AB.key "access_token" asText) 

createResource :: Resource -> Maybe Token -> Keycloak ResourceId
createResource r tok = keycloakPost "authz/protection/resource_set" (toJSON r) tok (AB.key "_id" asText) 
   
keycloakPost :: (Postable a, Show a) => Path -> a -> Maybe Token -> Parse Text b -> Keycloak b
keycloakPost path dat mtok parser = do 
  (KCConfig baseUrl realm _ _ _ _ guestId guestPass) <- ask
  tok <- case mtok of
       Just tok -> return tok
       Nothing -> getUserAuthToken guestId guestPass
  let opts = W.defaults & W.header "Authorization" .~ [encodeUtf8 ("Bearer " <> (unToken tok))]
  let url = (unpack $ baseUrl <> "/realms/" <> realm <> "/" <> path) 
  info $ "Issuing KEYCLOAK post with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  postRes <- C.try $ liftIO $ W.postWith opts url dat
  case postRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> return ret
        Left err2 -> throwError $ ParseError $ pack (show err2)
    Left err -> throwError $ HTTPError err


debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s = liftIO $ infoM "API" s
warn s = liftIO $ warningM "API" s
err s = liftIO $ errorM "API" s

getErrorStatus :: KCError -> Maybe Status 
getErrorStatus (HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) = Just $ HC.responseStatus r
getErrorStatus _ = Nothing

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

