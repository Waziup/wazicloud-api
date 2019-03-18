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
import           Network.HTTP.Types.Status as HTS

getSocialMessages :: Maybe Token -> Waziup [SocialMessage]
getSocialMessages tok = runMongo $ do
  info "Get social messages"
  let sel = (select [] "waziup_social_msgs") 
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
postSocialMessage tok socMsg@(SocialMessage _ username chan msg _ _) = do
  info "Post social messages"
  users <- Users.getUsers tok Nothing Nothing (Just username)
  let muser = L.find (\u -> T.userUsername u == username) users
  debug $ (show muser)
  case muser of
    Just user -> do
      case chan of
        Twitter -> do
          case (userTwitter user) of
            Just screenName -> postTwitter screenName msg
            Nothing -> throwError err400 {errBody = "Twitter ID not found in user profile"}
        SMS -> case (userPhone user) of
            Just phone -> postSMS phone msg
            Nothing -> throwError err400 {errBody = "Twitter ID not found in user profile"}
        Voice -> undefined
      runMongo $ logSocialMessage socMsg
    Nothing -> throwError err400 {errBody = "User not found"}

logSocialMessage :: SocialMessage -> Action IO SocialMessageId 
logSocialMessage msg = do
  debug "Post msg to Mongo"
  time <- liftIO getCurrentTime
  let msg' = msg {socTimestamp = Just time}
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
  info "Posting Twitter message"
  twInfo <- view (waziupConfig.twitterConf)
  debug $ "Twitter details: " ++ (show twInfo)
  mgr <- liftIO $ newManager tlsManagerSettings
  res <- C.try $ do
    users <- liftIO $ call twInfo mgr $ usersLookup $ ScreenNameListParam [convertString screenName]
    let usrId = TWT.userId $ L.head users
    liftIO $ call twInfo mgr $ directMessagesNew (UserIdParam usrId) msg
  case res of
    Right _ -> return () 
    Left (FromJSONError e) -> debug $ "Twitter FromJSONError: " ++ (show e) 
    Left (TwitterErrorResponse (HTS.Status code msg) _ tms) -> do
      debug $ "Twitter Error: " ++ (show code) ++ " " ++ (show msg) ++ " " ++ (show tms)
      throwError err400 {errBody = convertString $ "Twitter error: " ++ (show tms)}

postSMS :: Text -> Text -> Waziup ()
postSMS phone txt = smsPost $ PlivoSMS "+393806412094" phone txt

postSocialMessageBatch :: Maybe Token -> SocialMessageBatch -> Waziup NoContent
postSocialMessageBatch tok b@(SocialMessageBatch uns chans msg) = do
  info $ "Post social message batch: " ++ (show b)
  forM_ uns $ \un -> do
    forM_ chans $ \chan -> do
      res <- C.try $ postSocialMessage tok (SocialMessage Nothing un chan msg Nothing Nothing)
      case res of
        Right _ -> debug "Message success"
        Left (e :: SomeException) -> warn $ "Message error: " ++ (show e)
  return NoContent 

getSocialMessage :: Maybe Token -> SocialMessageId -> Waziup SocialMessage
getSocialMessage tok soc = do
  debug $ "getting message: " ++ (show soc)
  res <- runMongo $ getSocialMessageMongo tok soc
  case res of
    Just a -> return a
    Nothing -> throwError err404 {errBody = "Could not find social message"}

getSocialMessageMongo :: Maybe Token -> SocialMessageId -> Action IO (Maybe SocialMessage)
getSocialMessageMongo tok (SocialMessageId id) = do
  let filter = ["_id" =: ObjId (read $ convertString id)]
  let sel = (select filter "waziup_social_msgs") 
  mdoc <- findOne sel
  debug $ "Got docs:" <> (show mdoc)
  case mdoc of
    Nothing -> do
      err $ "Could not find social message"
      return Nothing
    Just doc -> do
      let res = fromJSON $ Object $ aesonify doc
      debug $ "Got messages:" <> (show res)
      case res of
        JSON.Success a -> return a
        JSON.Error e -> do
          err $ "JSON error:" ++ (show e)
          return Nothing


deleteSocialMessage :: Maybe Token -> SocialMessageId -> Waziup NoContent
deleteSocialMessage tok soc = do
  res <- runMongo $ deleteSocialMessageMongo tok soc 
  case res of 
    True -> return NoContent
    False -> throwError err404 {errBody = "Could not find social message"}

deleteSocialMessageMongo :: Maybe Token -> SocialMessageId -> Action IO Bool 
deleteSocialMessageMongo tok (SocialMessageId id) = do
  let filter = ["_id" =: ObjId (read $ convertString id)]
  res <- DB.deleteMany "waziup_social_msgs" [(filter, [])]
  return $ not $ failed res 

data PlivoSMS = PlivoSMS {
  smsSrc  :: Text,
  smsDst  :: Text,
  smsText :: Text} deriving (Show, Eq, Generic)

instance ToJSON PlivoSMS where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = T.unCapitalize . L.drop 3, omitNothingFields = True}

smsPost :: PlivoSMS -> Waziup ()
smsPost dat = do
  (PlivoConfig host id token) <- view (waziupConfig.plivoConf)
  let opts = W.defaults & W.auth ?~ W.basicAuth (convertString id) (convertString token)
  let path = convertString $ host <> "/v1/Account/" <> id <> "/Message/"
  info $ "Issuing Plivo POST " ++ (show path) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  liftIO $ W.postWith opts path (toJSON dat)
  return ()

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Socials" s
info  s = liftIO $ infoM    "Socials" s
warn  s = liftIO $ warningM "Socials" s
err   s = liftIO $ errorM   "Socials" s

