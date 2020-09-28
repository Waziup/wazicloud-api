{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Auth where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Monad.Extra
import           Data.String.Conversions
import           Data.Maybe
import           Data.Time
import           Data.Text hiding (map, any, filter)
import           Data.Cache as C hiding (lookup)
import           Data.Cache.Internal as CI
import qualified Data.Map as M
import           Data.Map ((!?)) 
import qualified Keycloak as KC
import           Keycloak (Token, getClaims)
import           Servant
import           System.Log.Logger
import           Waziup.Types as W
import           Waziup.Utils as U
import           Waziup.Users hiding (info, debug)

-- | get a token
postAuth :: AuthBody -> Waziup Token
postAuth (AuthBody username password) = do
  info "Post authentication"
  tok <- liftKeycloak' $ KC.getUserAuthToken username password
  return tok

-- * Permissions

-- | Throws error 403 if the scope is not premitted for this resource.
checkPermResource :: Maybe Token -> Scope -> PermResource -> Waziup ()
checkPermResource mtok scp res = do
  if isPermitted mtok res scp
    then return ()
    else throwError err403 {errBody = "Forbidden: Cannot access resource"}

-- | Check the resource against the corresponding scopes.
getPerm :: Maybe Token -> PermResource -> [Scope] -> Perm
getPerm tok res scopes = Perm (getPermResId res) (filter (isPermitted tok res) scopes)

-- | Check the resource against the corresponding scope.
isPermitted :: Maybe Token -> PermResource -> Scope -> Bool
isPermitted tok res scope = isPermitted' (getUserFromToken tok) res scope 

-- | Check the resource against the corresponding scope.
isPermitted' :: User -> PermResource -> Scope -> Bool
isPermitted' user res scope = case lookup scope allPermissions of
  Just policies -> or $ map (\p -> p user res) policies
  Nothing   -> error "Scope not found"


-- | Policies

-- An admin always have access
adminUser :: Policy
adminUser user _ = userAdmin user == Just True 

-- Access to a public resource
publicResource :: Policy 
publicResource _ (PermDevice d) = devVisibility d == Just Public
publicResource _ (PermGateway d) = gwVisibility d == Just Public
publicResource _ (PermProject d) = error "Projects doesn't have a visibility" 

-- Access if you are the owner of the resource
resourceOwner :: Policy 
resourceOwner user (PermDevice d)  = Just (userUsername user) == devOwner d 
resourceOwner user (PermGateway d) = Just (userUsername user) == gwOwner d 
resourceOwner user (PermProject d) = Just (userUsername user) == pOwner d 

-- everybody have access
allUsers :: Policy 
allUsers tok res = True

-- Policies associated to each scope
allPermissions :: [(Scope, [Policy])]
allPermissions = [(DevicesCreate,     [allUsers]),
                  (DevicesUpdate,     [adminUser, resourceOwner]),
                  (DevicesView,       [adminUser, resourceOwner, publicResource]),
                  (DevicesDelete,     [adminUser, resourceOwner]),
                  (DevicesDataCreate, [adminUser, resourceOwner, publicResource]),
                  (DevicesDataView,   [adminUser, resourceOwner, publicResource]),
                  (GatewaysCreate,    [allUsers]),
                  (GatewaysUpdate,    [adminUser, resourceOwner]),
                  (GatewaysView,      [adminUser, resourceOwner, publicResource]),
                  (GatewaysDelete,    [adminUser, resourceOwner]),
                  (ProjectsCreate,    [allUsers]),
                  (ProjectsUpdate,    [adminUser, resourceOwner]),
                  (ProjectsView,      [adminUser, resourceOwner]),
                  (ProjectsDelete,    [adminUser, resourceOwner])]

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Auth" s
info  s = liftIO $ infoM    "Auth" s
warn  s = liftIO $ warningM "Auth" s
err   s = liftIO $ errorM   "Auth" s

