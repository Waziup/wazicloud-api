{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Socials where

import           Waziup.Types as T
import           Waziup.Utils
import qualified Waziup.Users as Users
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
import           Keycloak as KC hiding (Scope) 
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value)
import           Web.Twitter.Conduit
import           Web.Twitter.Types as TWT
import           Network.Wreq as W
import           Control.Lens hiding ((.=))
import           GHC.Generics (Generic)
import           Data.Bson as BSON
import           Servant
import           Network.HTTP.Types.Status as HTS

-- | get all logged social messages
getSocialMessages :: Maybe Token -> Waziup [SocialMessage]
getSocialMessages _ = runMongo $ do
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

-- | post a social message
postSocialMessage :: Maybe Token -> SocialMessage -> Waziup SocialMessageId
postSocialMessage tok socMsg@(SocialMessage _ dest chan msg _ _) = do
  info "Post social messages"
  users <- Users.getUsers tok Nothing Nothing (Just dest)
  let muser = L.find (\u -> T.userUsername u == dest) users
  debug $ (show muser)
  case muser of
    Just user -> do
      case chan of
        Twitter -> postTwitter user msg
        SMS     -> postSMS tok user msg
        Voice   -> undefined
        None    -> return () 
      runMongo $ logSocialMessage socMsg
    Nothing -> throwError err400 {errBody = "User not found"}

logSocialMessage :: SocialMessage -> Action IO SocialMessageId 
logSocialMessage msg = do
  debug "Post msg to Mongo"
  time <- liftIO getCurrentTime
  let msg' = msg {socTimestamp = Just time}
  let ob = case toJSON msg' of
       JSON.Object o -> o
       _ -> error "Wrong object format"
  res <- insert "waziup_social_msgs" (bsonifyBound ob)
  debug $ (show res)
  case res of 
    BSON.ObjId a -> return $ SocialMessageId $ convertString $ show a
    _ -> error "bad mongo ID"
 
postTwitter :: T.User -> Text -> Waziup ()
postTwitter user msg = do
  info "Posting Twitter message"
  case (userTwitter user) of
    Nothing -> throwError err400 {errBody = "Twitter ID not found in user profile"}
    Just screenName -> do 
      twInfo <- view (waziupConfig.twitterConf)
      debug $ "Twitter details: " ++ (show twInfo)
      mgr <- liftIO $ newManager tlsManagerSettings
      res <- C.try $ do
        users <- liftIO $ call twInfo mgr $ usersLookup $ ScreenNameListParam [convertString screenName]
        let usrId = TWT.userId $ L.head users
        liftIO $ call twInfo mgr $ directMessagesNew usrId msg
      case res of
        Right _ -> return () 
        Left (FromJSONError e) -> debug $ "Twitter FromJSONError: " ++ (show e) 
        Left (TwitterErrorResponse (HTS.Status code errmsg) _ tms) -> do
          debug $ "Twitter Error: " ++ (show code) ++ " " ++ (show errmsg) ++ " " ++ (show tms)
          throwError err400 {errBody = convertString $ "Twitter error: " ++ (show tms)}
        Left e -> throwM e

postSMS :: Maybe Token -> T.User -> Text -> Waziup ()
postSMS tok user@(T.User _ _ _ _ _ (Just phone) _ _ (Just c) _) txt = do
  res <- C.try $ smsPost $ PlivoSMS "+393806412094" phone txt
  case res of 
    Right _ -> do
      debug $ "Removing one SMS credit, remaining: " ++ (show (c -1))
      void $ Users.putUserCredit tok (fromJust $ T.userId user) (c - 1)
    Left (er :: SomeException) -> throwError err400 {errBody = convertString $ "Could not send SMS: " ++ (show er)}
  return ()
postSMS _ _ _ = do
  warn "phone ID or sms credit not found in user profile: update user profile"
  throwError err400 {errBody = "phone ID or sms credit not found in user profile: update user profile"}

postSocialMessageBatch :: Maybe Token -> SocialMessageBatch -> Waziup NoContent
postSocialMessageBatch tok b@(SocialMessageBatch us chans msg) = do
  info $ "Post social message batch: " ++ (show b)
  forM_ us $ \u -> do
    forM_ chans $ \chan -> do
      res <- C.try $ postSocialMessage tok (SocialMessage Nothing u chan msg Nothing Nothing)
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
getSocialMessageMongo _ (SocialMessageId sid) = do
  let fil = ["_id" =: ObjId (read $ convertString sid)]
  let sel = (select fil "waziup_social_msgs") 
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
deleteSocialMessageMongo _ (SocialMessageId sid) = do
  let fil = ["_id" =: ObjId (read $ convertString sid)]
  res <- DB.deleteMany "waziup_social_msgs" [(fil, [])]
  return $ not $ failed res 

data PlivoSMS = PlivoSMS {
  smsSrc  :: Text,
  smsDst  :: Text,
  smsText :: Text} deriving (Show, Eq, Generic)

instance ToJSON PlivoSMS where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = T.unCapitalize . L.drop 3, omitNothingFields = True}

smsPost :: PlivoSMS -> Waziup ()
smsPost dat = do
  (PlivoConfig hos aid token) <- view (waziupConfig.plivoConf)
  let opts = W.defaults & W.auth ?~ W.basicAuth (convertString aid) (convertString token)
  let path = convertString $ hos <> "/v1/Account/" <> aid <> "/Message/"
  info $ "Issuing Plivo POST " ++ (show path) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  void $ liftIO $ W.postWith opts path (toJSON dat)

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Socials" s
info  s = liftIO $ infoM    "Socials" s
warn  s = liftIO $ warningM "Socials" s
err   s = liftIO $ errorM   "Socials" s

