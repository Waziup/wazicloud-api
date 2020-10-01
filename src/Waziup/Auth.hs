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
import           Data.Text as T hiding (map, any, filter, null)
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
  case isPermitted mtok res scp of
    Nothing -> return ()
    Just e -> case res of
      PermDevice d  -> throwError err403 {errBody = "Forbidden: Cannot access device "  <> (convertString $ unDeviceId $ devId d) <> ". Cause: " <> (convertString e)}
      PermGateway d -> throwError err403 {errBody = "Forbidden: Cannot access gateway " <> (convertString $ unGatewayId $ gwId d) <> ". Cause: " <> (convertString e)}
      PermProject d -> throwError err403 {errBody = "Forbidden: Cannot access project " <> (convertString $ pName d) <> ". Cause: " <> (convertString e)}

-- | Check the resource against the corresponding scopes.
getPerm :: Maybe Token -> PermResource -> [Scope] -> Perm
getPerm tok res scopes = Perm (getPermResId res) (filter (isNothing . isPermitted tok res) scopes)

-- | Check the resource against the corresponding scope.
isPermitted :: Maybe Token -> PermResource -> Scope -> IsPermitted
isPermitted tok res scope = isPermitted' (getUserFromToken tok) res scope 

-- | Check the resource against the corresponding scope.
isPermitted' :: User -> PermResource -> Scope -> IsPermitted
isPermitted' user res scope = case lookup scope allPermissions of
  Just policies -> case sequence $ map (\p -> p user res) policies of
                     Just as -> Just $ T.intercalate ", " as
                     Nothing -> Nothing
  Nothing   -> error "Scope not found" where

-- | Policies

-- An admin always have access
adminUser :: Policy
adminUser user _ = if userAdmin user == Just True then Nothing else Just "You are not admin"

-- Access to a public resource
publicResource :: Policy 
publicResource _ (PermDevice d)  = if not $ devVisibility d == Just Private then Nothing else Just "this device is private"
publicResource _ (PermGateway d) = if not $ gwVisibility d == Just Private then Nothing  else Just "this gateway is private"
publicResource _ (PermProject d) = error "Projects doesn't have a visibility" 

-- Access if you are the owner of the resource
resourceOwner :: Policy 
resourceOwner user (PermDevice d)  = if Just (userUsername user) == devOwner d then Nothing else Just "not owner of the device"
resourceOwner user (PermGateway d) = if Just (userUsername user) == gwOwner d then Nothing else Just "not owner of the gateway"
resourceOwner user (PermProject d) = if Just (userUsername user) == pOwner d then Nothing else Just "not owner of the project"

-- everybody have access
allUsers :: Policy 
allUsers tok res = Nothing

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

