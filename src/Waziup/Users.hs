{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Users where

import           Waziup.Types
import           Waziup.Utils
import           System.Log.Logger
import           Control.Monad.IO.Class
import           Control.Monad
import qualified Keycloak as KC
import           Data.Maybe
import           Data.Map hiding (map)
import           Data.String.Conversions
import           Data.Text hiding (map, head)
import           Servant
import           Data.Aeson as JSON

getUsers :: Maybe KC.Token -> Maybe Limit -> Maybe Offset -> Maybe KC.Username -> Waziup [User]
getUsers tok ml mo username = do
  info "Get users"
  debug $ "Username: " ++ (show username)
  us <- liftKeycloak tok $ KC.getUsers ml mo username
  return $ map toUser us

getUser :: Maybe KC.Token -> UserId -> Waziup User
getUser tok (UserId uid) = do
  info "Get users"
  return $ getUserFromToken tok
  u <- liftKeycloak tok $ KC.getUser (KC.UserId uid)
  return $ toUser u

getUserFromToken :: Maybe KC.Token -> User
getUserFromToken mtok = case mtok of
    Just tok -> let claims = KC.getClaims tok in
      User
           { userId        = Nothing 
           , userUsername  = fromJust $ join $ readString <$> claims !? "preferred_username" 
           , userFirstName = join $ readString <$> claims !? "given_name"  
           , userLastName  = join $ readString <$> claims !? "family_name" 
           , userEmail     = join $ readString <$> claims !? "email" 
           , userPhone     = join $ readString <$> claims !? "phone" 
           , userFacebook  = join $ readString <$> claims !? "facebook" 
           , userTwitter   = join $ readString <$> claims !? "twitter" 
           , userSmsCredit = join $ readInt    <$> claims !? "sms_credit" 
           , userAdmin     = join $ readBool   <$> claims !? "admin"}
    Nothing -> guestUser

guestUser :: User
guestUser = User
           { userId        = Nothing 
           , userUsername  = "guest" 
           , userFirstName = Just "guest" 
           , userLastName  = Just "guest" 
           , userEmail     = Nothing 
           , userPhone     = Nothing
           , userFacebook  = Nothing
           , userTwitter   = Nothing
           , userSmsCredit = Nothing
           , userAdmin     = Just False} 

readString :: Value -> Maybe Text
readString (String a) = Just a
readString _ = Nothing

readInt :: Value -> Maybe Int
readInt (Number a) = Just $ round a
readInt _ = Nothing

readBool :: Value -> Maybe Bool
readBool (Bool a) = Just a
readBool _ = Nothing

postUser :: Maybe KC.Token -> User -> Waziup UserId
postUser tok user = do 
  info "Post users"
  uid <- liftKeycloak tok $ KC.createUser (fromUser user)
  return $ UserId $ KC.unUserId uid

putUserCredit :: Maybe KC.Token -> UserId -> Int -> Waziup NoContent
putUserCredit tok uid@(UserId i) c = do
  info "Put user credit"
  u <- getUser tok uid 
  let u' = u {userSmsCredit = Just c}
  liftKeycloak tok $ KC.updateUser (KC.UserId i) (fromUser u')
  return NoContent

toUser :: KC.User -> User
toUser (KC.User i un fn ln mail subs) = 
  User { userId        = maybe Nothing (Just . UserId . KC.unUserId) i 
       , userUsername  = un
       , userFirstName = fn
       , userLastName  = ln 
       , userEmail     = mail
       , userPhone     = if isJust subs then convertString.head <$> (fromJust subs) !? "phone" else Nothing
       , userFacebook  = if isJust subs then convertString.head <$> (fromJust subs) !? "facebook" else Nothing
       , userTwitter   = if isJust subs then convertString.head <$> (fromJust subs) !? "twitter" else Nothing
       , userSmsCredit = if isJust subs then read.convertString.head <$> (fromJust subs) !? "sms_credit" else Nothing
       , userAdmin     = if isJust subs then read.convertString.head <$> (fromJust subs) !? "admin" else Nothing
       }

fromUser :: User -> KC.User
fromUser (User i usern fn ln email ph fb tw smsc admin) =
  KC.User { KC.userId = maybe Nothing (Just . KC.UserId . unUserId) i
          , KC.userUsername = usern
          , KC.userFirstName = fn
          , KC.userLastName = ln
          , KC.userEmail = email
          , KC.userAttributes = Just (fromList $ catMaybes [getAtt "phone" ph, 
                                                            getAtt "facebook" fb,
                                                            getAtt "twitter" tw,
                                                            getAtt "sms_credit" (convertString.show <$> smsc),
                                                            getAtt "admin" (convertString.show <$> admin)
                                                            ])}

getAtt :: Text -> Maybe Text -> Maybe (Text, [Text])
getAtt label (Just field) = Just (label, [field])
getAtt _ Nothing = Nothing


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Users" s
info  s = liftIO $ infoM    "Users" s
warn  s = liftIO $ warningM "Users" s
err   s = liftIO $ errorM   "Users" s

