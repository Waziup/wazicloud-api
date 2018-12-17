{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Users where

import           Waziup.Types
import           Waziup.Utils
import           System.Log.Logger
import           Control.Monad.IO.Class
import           Keycloak as KC hiding (info, warn, debug, err, Scope, User, UserId) 

getUsers :: Maybe Token -> Maybe Limit -> Maybe Offset -> Waziup [User]
getUsers tok ml mo = do
  info "Get users"
  runKeycloak $ KC.getUsers ml mo
  undefined

getUser :: Maybe Token -> UserId -> Waziup User
getUser tok uid = do
  info "Get users"
  undefined


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Users" s
info  s = liftIO $ infoM    "Users" s
warn  s = liftIO $ warningM "Users" s
err   s = liftIO $ errorM   "Users" s

