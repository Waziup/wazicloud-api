
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keycloak.Client where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Lens
import Data.Text hiding (head, tail)
import GHC.Generics (Generic)
import Data.Text.Encoding
import Data.Maybe
import Data.Foldable

newtype Scope = Scope Text deriving (Generic, Show)

instance FromJSON Scope

data Permission = Permission 
  { rsname :: Text,
    rsid   :: Text,
    scopes :: [Scope]
  } deriving (Generic, Show)

instance FromJSON Permission where
  parseJSON (Object v) = Permission <$>
                            v .: "rsname" <*>
                            v .: "rsid" <*>
                            (fromMaybe [] <$> v .:? "scopes")
  parseJSON _          = mzero


getPermissions :: Text -> Text -> IO (Maybe [Permission])
getPermissions username password = do
  Just (token :: Text) <- getUserAuthToken username password
  let opts = defaults &
             header "Authorization" .~ [encodeUtf8 (append "Bearer " token)]
  let dat = ["grant_type" := ("urn:ietf:params:oauth:grant-type:uma-ticket" :: Text),
             "audience" := ("api-server" :: Text),
             "response_mode" := ("permissions" :: Text)]
  res <- postWith opts "http://localhost:8080/auth/realms/waziup/protocol/openid-connect/token" dat
  let res2 = fromJust $ res ^? responseBody
  case JSON.eitherDecode res2 of
     Right a -> return $ Just a
     Left e -> do
       putStrLn $ "Error while decoding JSON: " ++ e
       return Nothing

getUserAuthToken :: Text -> Text -> IO (Maybe Text)
getUserAuthToken username password = do
  let dat = 
        ["client_id" := ("api-server" :: Text), 
         "client_secret" := ("4e9dcb80-efcd-484c-b3d7-1e95a0096ac0" :: Text),
         "grant_type" := ("password" :: Text),
         "password" := password,
         "username" := username]
  perms <- post  "http://localhost:8080/auth/realms/waziup/protocol/openid-connect/token" dat 
  return $ perms ^? responseBody . key "access_token" . _String

getClientAuthToken :: IO (Maybe Text)
getClientAuthToken = do
  let dat = 
        ["client_id" := ("api-server" :: Text), 
         "client_secret" := ("4e9dcb80-efcd-484c-b3d7-1e95a0096ac0" :: Text),
         "grant_type" := ("password" :: Text),
         "password" := ("password" :: Text),
         "username" := ("cdupont" :: Text)] --(toString $ queryString d)
  perms <- post  "http://localhost:8080/auth/realms/waziup/protocol/openid-connect/token" dat 
  return $ perms ^? responseBody . key "access_token" . _String
