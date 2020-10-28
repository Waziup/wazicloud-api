{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Users where

import           Waziup.Types
import           Waziup.Utils
import           System.Log.Logger
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens
import qualified Keycloak as KC
import           Data.Maybe
import           Data.HashMap.Strict hiding (map)
import           Data.String.Conversions
import           Data.Text hiding (map, head)
import           Servant
import           Data.Aeson as JSON
import           Data.Aeson.Lens
import           Data.Scientific
import           Servant.Auth.Server


getUsers :: AuthUser -> Maybe Limit -> Maybe Offset -> Maybe KC.Username -> Waziup [User]
getUsers tok ml mo username = do
  info "Get users"
  debug $ "Username: " ++ (show username)
  tok <- getAdminToken
  us <- liftKeycloak $ KC.getUsers ml mo username tok
  return $ map toUser us

getUser :: AuthUser -> UserId -> Waziup User
getUser tok (UserId uid) = do
  info "Get user"
  tok <- getAdminToken
  u <- liftKeycloak $ KC.getUser (KC.UserId uid) tok
  return $ toUser u

postUser :: AuthUser -> User -> Waziup UserId
postUser tok user = do 
  info "Post users"
  tok <- getAdminToken
  uid <- liftKeycloak $ KC.createUser (fromUser user) tok
  return $ UserId $ KC.unUserId uid

putUserCredit :: AuthUser -> UserId -> Int -> Waziup NoContent
putUserCredit au uid@(UserId i)  c = do
  info "Put user credit"
  --TODO check credentials of AuthUser
  u <- getUser au uid
  let u' = u {userSmsCredit = Just c}
  tok <- getAdminToken
  liftKeycloak $ KC.updateUser (KC.UserId i) (fromUser u') tok
  return NoContent

toUser :: KC.User -> User
toUser (KC.User i un fn ln mail subs) = let obj = Object <$> subs in 
  User { userId        = UserId . KC.unUserId <$> i 
       , userUsername  = un
       , userFirstName = fn
       , userLastName  = ln 
       , userEmail     = mail
       , userPhone     = preview (_Just . at "phone" . _Just . _String) subs
       , userFacebook  = preview (_Just . at "facebook" . _Just . _String) subs
       , userTwitter   = preview (_Just . at "twitter" . _Just . _String) subs
       , userSmsCredit = preview (_Just . at "sms_credit"  . _Just . _Integral) subs
       , userAdmin     = preview (_Just . at "admin" . _Just . _Bool) subs
       }

fromUser :: User -> KC.User
fromUser u@(User i usern fn ln email ph fb tw smsc admin) =
  KC.User { KC.userId = maybe Nothing (Just . KC.UserId . unUserId) i
          , KC.userUsername = usern
          , KC.userFirstName = fn
          , KC.userLastName = ln
          , KC.userEmail = email
          , KC.userAttributes = preview _Object (toJSON u)}
          

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Users" s
info  s = liftIO $ infoM    "Users" s
warn  s = liftIO $ warningM "Users" s
err   s = liftIO $ errorM   "Users" s

