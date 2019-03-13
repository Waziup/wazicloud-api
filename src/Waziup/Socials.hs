{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Socials where

import           Waziup.Types as T
import           Waziup.Utils
import qualified Waziup.Users as Users
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any, find)
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Time
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value)
import           Web.Twitter.Conduit hiding (map)
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Types as TWT
import           Network.Wreq as W
import           Control.Lens hiding ((.=))
import           GHC.Generics (Generic)
import           Data.Bson as BSON
import           Servant

getSocialMessages :: Maybe Token -> Waziup [SocialMessage]
getSocialMessages tok = runMongo $ do
  let filter = ["user" =: ("test" :: Text)]
  let sel = (select filter "waziup_social_msgs") 
  cur <- find sel
  docs <- rest cur
  debug $ "Got docs:" <> (show docs)
  let res = map (fromJSON . Object . aesonify) docs
  debug $ "Got messages:" <> (show res)
  case (sequence res) of
    JSON.Success a -> return a
    JSON.Error e -> do
      err $ "Error from Mongo:" ++ (show e)
      return []
                                              
postSocialMessage :: Maybe Token -> SocialMessage -> Waziup SocialMessageId
postSocialMessage tok socMsg@(SocialMessage _ username chan msg) = do
  users <- Users.getUsers tok Nothing Nothing (Just username)
  let muser = L.find (\u -> T.userUsername u == username) users
  debug $ (show muser)
  case muser of
    Just user -> do
      case chan of
        Twitter -> do
          case (userFacebook user) of
            Just screenName -> do
              postTwitter screenName msg
              return undefined 
            Nothing -> throwError err400 {errBody = "Facebook ID not found in user profile"}
        SMS -> undefined 
        Voice -> undefined
      runMongo $ logSocialMessage socMsg
    Nothing -> throwError err400 {errBody = "User not found"}

logSocialMessage :: SocialMessage -> Action IO SocialMessageId 
logSocialMessage msg = do
  debug "Post msg to Mongo"
  let ob = case toJSON msg of
       JSON.Object o -> o
       _ -> error "Wrong object format"
  res <- insert "waziup_social_msgs" (bsonify ob)
  debug $ (show res)
  case res of 
    BSON.ObjId a -> return $ SocialMessageId $ convertString $ show a
    _ -> error "bad mongo ID"
  


postTwitter :: Text -> Text -> Waziup ()
postTwitter screenName msg = do
  debug "Posting Twitter message"
  twInfo <- view (waziupConfig.twitterConf)
  mgr <- liftIO $ newManager tlsManagerSettings
  users <- liftIO $ call twInfo mgr $ usersLookup $ ScreenNameListParam [convertString screenName]
  let usrId = TWT.userId $ L.head users
  ret <- C.try $ liftIO $ call twInfo mgr $ directMessagesNew (UserIdParam usrId) msg
  case ret of
    Right _ -> return () 
    Left (e :: SomeException)  -> return ()


postSMS :: SocialMessage -> Waziup ()
postSMS (SocialMessage _ _ _ txt) = smsPost $ PlivoSMS undefined undefined txt


postSocialMessageBatch :: Maybe Token -> SocialMessageBatch -> Waziup NoContent
postSocialMessageBatch sb = undefined


getSocialMessage :: Maybe Token -> SocialMessageId -> Waziup SocialMessage
getSocialMessage id = undefined


deleteSocialMessage :: Maybe Token -> SocialMessageId -> Waziup NoContent
deleteSocialMessage id = undefined

data PlivoSMS = PlivoSMS {
  smsSrc :: Text,
  smsDst :: Text,
  smsText :: Text} deriving (Show, Eq, Generic)

instance ToJSON PlivoSMS

smsPost :: PlivoSMS -> Waziup ()
smsPost dat = do 
  let opts = W.defaults & W.auth ?~ W.basicAuth "MAMDA5ZDJIMDM1NZVMZD" "NzRlNWJiNmU2MmFjYWJlODhlNTk3MTkyZGEzNzIy"
  let path = "https://api.plivo.com/v1/Account/" ++ "MAMDA5ZDJIMDM1NZVMZD" ++ "/Message/"
  info $ "Issuing Plivo POST " ++ (show path) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  liftIO $ W.postWith opts path (toJSON dat)
  return ()
  --case eRes of 
  --  Right res -> return ()
  --  Left err -> do
  --    warn $ "Plivo HTTP Error: " ++ (show err)
  --    throwError err404 {errBody = "Plivo error"}

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Socials" s
info  s = liftIO $ infoM    "Socials" s
warn  s = liftIO $ warningM "Socials" s
err   s = liftIO $ errorM   "Socials" s

