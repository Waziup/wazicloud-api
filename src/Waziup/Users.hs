{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Users where

import           Waziup.Types
import           Waziup.Utils
import           System.Log.Logger
import           Control.Monad.IO.Class
--import           Keycloak as KC hiding (info, warn, debug, err, Scope, User, UserId) 
import qualified Keycloak as KC
import           Data.Maybe
import           Data.Map hiding (map)
import           Data.String.Conversions

getUsers :: Maybe KC.Token -> Maybe Limit -> Maybe Offset -> Maybe KC.Username -> Waziup [User]
getUsers tok ml mo username = do
  info "Get users"
  debug $ "Username: " ++ (show username)
  us <- liftKeycloak tok $ KC.getUsers ml mo username
  return $ map toUser us

getUser :: Maybe KC.Token -> UserId -> Waziup User
getUser tok (UserId uid) = do
  info "Get users"
  u <- liftKeycloak tok $ KC.getUser (KC.UserId uid)
  return $ toUser u


toUser :: KC.User -> User
toUser (KC.User id un fn ln mail subs) = 
  User { userId        = maybe Nothing (Just . UserId . KC.unUserId) id 
       , userUsername  = un
       , userFirstName = fn
       , userLastName  = ln 
       , userEmail     = mail
       , userPhone     = if isJust subs then convertString.head <$> (fromJust subs) !? "phone" else Nothing
       , userFacebook  = if isJust subs then convertString.head <$> (fromJust subs) !? "facebook" else Nothing
       , userTwitter   = if isJust subs then convertString.head <$> (fromJust subs) !? "twitter" else Nothing
       }

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Users" s
info  s = liftIO $ infoM    "Users" s
warn  s = liftIO $ warningM "Users" s
err   s = liftIO $ errorM   "Users" s

