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
import           Data.Text hiding (map, head)
import           Servant

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

postUser :: Maybe KC.Token -> User -> Waziup UserId
postUser tok user = do 
  info "Post users"
  uid <- liftKeycloak tok $ KC.postUser (fromUser user)
  return $ UserId $ KC.unUserId uid

putUserCredit :: Maybe KC.Token -> UserId -> Int -> Waziup NoContent
putUserCredit tok uid@(UserId id) c = do
  info "Put user credit"
  u <- getUser tok uid 
  let u' = u {userSmsCredit = Just c}
  liftKeycloak tok $ KC.putUser (KC.UserId id) (fromUser u')
  return NoContent

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
       , userSmsCredit = if isJust subs then read.convertString.head <$> (fromJust subs) !? "sms_credit" else Nothing
       }

fromUser :: User -> KC.User
fromUser (User id usern fn ln email ph fb tw smsc) =
  KC.User { KC.userId = maybe Nothing (Just . KC.UserId . unUserId) id
          , KC.userUsername = usern
          , KC.userFirstName = fn
          , KC.userLastName = ln
          , KC.userEmail = email
          , KC.userAttributes = Just (fromList $ catMaybes [getAtt "phone" ph, 
                                                            getAtt "facebook" fb,
                                                            getAtt "twitter" tw,
                                                            getAtt "sms_credit" (convertString.show <$> smsc)])}

getAtt :: Text -> Maybe Text -> Maybe (Text, [Text])
getAtt label (Just field) = Just (label, [field])
getAtt _ Nothing = Nothing


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Users" s
info  s = liftIO $ infoM    "Users" s
warn  s = liftIO $ warningM "Users" s
err   s = liftIO $ errorM   "Users" s

