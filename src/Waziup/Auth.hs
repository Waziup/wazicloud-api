{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Auth where

import           Waziup.Types as W
import           Waziup.Utils
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.Maybe
import           Data.Map as M hiding (map, mapMaybe, filter, lookup, insert, delete)
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Scientific
import qualified Data.HashMap.Strict as H
import           Data.Aeson as JSON hiding (Options)
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           Paths_Waziup_Servant
import           Database.MongoDB as DB hiding (value, Limit, Array, lookup, Value, Null, (!?))
import           Data.AesonBson


-- | get a token
postAuth :: AuthBody -> Waziup Token
postAuth (AuthBody username password) = do
  info "Post authentication"
  tok <- liftKeycloak' $ getUserAuthToken username password
  return tok

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsDevices :: Maybe Token -> Waziup [Perm]
getPermsDevices tok = do
  info "Get devices permissions"
  getPerms tok [DevicesUpdate,
                DevicesView,
                DevicesDelete]

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsProjects :: Maybe Token -> Waziup [Perm]
getPermsProjects tok = do
  info "Get projects permissions"
  getPerms tok [ProjectsUpdate,
                ProjectsView,
                ProjectsDelete]

-- | Get all permissions. If no token is passed, the guest token will be used.
getPermsGateways :: Maybe Token -> Waziup [Perm]
getPermsGateways tok = do
  info "Get gateways permissions"
  getPerms tok [GatewaysUpdate,
                GatewaysView,
                GatewaysDelete]

getPerms :: Maybe Token -> [W.Scope] -> Waziup [Perm]
getPerms tok scps = do
  ps <- liftKeycloak tok $ getAllPermissions (map fromScope scps)
  let getP :: KC.Permission -> Perm
      getP (KC.Permission rsname _ scopes) = Perm rsname (mapMaybe toScope scopes)
  return $ map getP ps 

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Auth" s
info  s = liftIO $ infoM    "Auth" s
warn  s = liftIO $ warningM "Auth" s
err   s = liftIO $ errorM   "Auth" s

