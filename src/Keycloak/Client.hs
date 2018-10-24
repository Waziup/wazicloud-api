{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keycloak.Client where

import Network.Wreq as W
import Control.Lens
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Aeson.Casing
import Data.Text hiding (head, tail, map)
import GHC.Generics (Generic)
import Data.Text.Encoding
import Data.Maybe
import Data.Foldable
import Control.Monad.Reader as R
import Keycloak.Types
import Network.HTTP.Client (HttpException)

data KCError = HTTPError HttpException  -- ^ Keycloak returned an HTTP error.
             | ParseError String        -- ^ Failed when parsing the response
             | EmptyError               -- ^ Empty error to serve as a zero element for Monoid.

type Keycloak a = ReaderT KCConfig IO (Either KCError a)

getAllPermissions :: Text -> Text -> Keycloak [Permission]
getAllPermissions username password = do
  (KCConfig _ client _ _ _) <- ask 
  Right (token :: Text) <- getUserAuthToken username password
  let opts = W.defaults &
             W.header "Authorization" .~ [encodeUtf8 (append "Bearer " token)]
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := client,
             "response_mode" := ("permissions" :: Text)]
  res <- liftIO $ W.postWith opts "http://localhost:8080/auth/realms/waziup/protocol/openid-connect/token" dat
  let res2 = fromJust $ res ^? responseBody
  case JSON.eitherDecode res2 of
     Right a -> return $ Right a
     Left e -> do
       liftIO $ putStrLn $ "Error while decoding JSON: " ++ e
       return $ Left $ ParseError $ "Error while decoding JSON: " ++ e

getUserAuthToken :: Text -> Text -> Keycloak Token
getUserAuthToken username password = do
  (KCConfig realm client secret login password) <- ask 
  let dat = 
        ["client_id" := client, 
         "client_secret" := secret,
         "grant_type" := ("password" :: Text),
         "password" := password,
         "username" := login]
  perms <- liftIO $ W.post  "http://localhost:8080/auth/realms/waziup/protocol/openid-connect/token" dat 
  return $ Right $ fromJust $ perms ^? responseBody . key "access_token" . _String

getClientAuthToken :: Keycloak Token
getClientAuthToken = do
  (KCConfig realm client secret login password) <- ask
  let dat = 
        ["client_id" := client, 
         "client_secret" := secret,
         "grant_type" := ("client_credentials" :: Text)]
  liftIO $ putStrLn $ show dat
  perms <- liftIO $ W.post ("http://localhost:8080/auth/realms/" ++ unpack realm ++ "/protocol/openid-connect/token") dat 
  return $ Right $ fromJust $ perms ^? responseBody . key "access_token" . _String


createResource :: Resource -> Keycloak ResourceId
createResource r = do
  Right token <- getClientAuthToken
  let opts = W.defaults &
             W.header "Authorization" .~ [encodeUtf8 (append "Bearer " token)]
  liftIO $ putStrLn $ show opts
  liftIO $ putStrLn $ show (encode r)
  perms <- liftIO $ W.postWith opts "http://localhost:8080/auth/realms/waziup/authz/protection/resource_set" (toJSON r) 
  return $ Right $ fromJust $ perms ^? responseBody . key "_id" . _String
   
