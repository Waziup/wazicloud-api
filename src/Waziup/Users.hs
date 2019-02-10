{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Users where

import           Waziup.Types
import           Waziup.Utils
import           System.Log.Logger
import           Control.Monad.IO.Class
--import           Keycloak as KC hiding (info, warn, debug, err, Scope, User, UserId) 
import qualified Keycloak as KC

getUsers :: Maybe KC.Token -> Maybe Limit -> Maybe Offset -> Waziup [User]
getUsers tok ml mo = do
  info "Get users"
  us <- liftKeycloak tok $ KC.getUsers ml mo
  return $ map toUser us

getUser :: Maybe KC.Token -> UserId -> Waziup User
getUser tok (UserId uid) = do
  info "Get users"
  u <- liftKeycloak tok $ KC.getUser (KC.UserId uid)
  return $ toUser u


toUser :: KC.User -> User
toUser (KC.User id un fn ln mail) = 
  User { userId        = maybe Nothing (Just . UserId . KC.unUserId) id 
       , userUsername  = un
       , userFirstName = fn
       , userLastName  = ln 
       , userEmail     = mail
       , userPhone     = Nothing
       , userFacebook  = Nothing
       , userTwitter   = Nothing
       }

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Users" s
info  s = liftIO $ infoM    "Users" s
warn  s = liftIO $ warningM "Users" s
err   s = liftIO $ errorM   "Users" s

