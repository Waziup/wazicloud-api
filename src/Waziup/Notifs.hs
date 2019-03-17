{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Notifs where

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
import           Data.Map as M hiding (map)
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value, (!?))
import           Web.Twitter.Conduit hiding (map)
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Types as TWT
import           Network.Wreq as W
import           Control.Lens hiding ((.=))
import           GHC.Generics (Generic)
import           Data.Bson as BSON hiding ((!?))
import           Servant
import           Network.HTTP.Types.Status as HTS
import           Orion as O hiding (info, warn, debug, err)
import           Network.HTTP.Types.URI as URI

getNotifs :: Maybe Token -> Waziup [Notif]
getNotifs tok = do 
  info "Get notifs"
  subs <- liftOrion $ O.getSubs
  let notifs = map getNotifFromSub subs
  return notifs
                                              
postNotif :: Maybe Token -> Notif -> Waziup NotifId
postNotif tok not = do
  info $ "Post notif: " ++ (show not)
  let sub = getSubFromNotif not
  debug $ "sub: " ++ (convertString $ (encode sub) :: String)
  res <- liftOrion $ O.postSub sub
  return $ NotifId "" 

getNotif :: Maybe Token -> NotifId -> Waziup Notif
getNotif tok not = undefined

deleteNotif :: Maybe Token -> NotifId -> Waziup NoContent
deleteNotif tok soc = undefined


getNotifFromSub :: Subscription -> Notif
getNotifFromSub (Subscription subId subDesc subSubject subNotif subThrottling) = 
  Notif { notifId          = getNotifId <$> subId 
        , notifDescription = subDesc 
        , notifSubject     = getNotifSubject subSubject 
        , notifNotif       = getNotifNotif subNotif
        , notifThrottling  = subThrottling} 

getNotifId :: SubId -> NotifId
getNotifId (SubId sid) = NotifId sid

getNotifSubject :: SubSubject -> NotifSubject
getNotifSubject (SubSubject ents (SubCondition attrs exp)) = NotifSubject
  { notifSubjectDevices = map getDeviceId (map subEntId ents)
  , notifSubjectCond    = NotifCond (map getSensorId attrs) q } where
    q = case convertString <$> exp !? "q" of
          Just q -> q
          Nothing -> error "Cannot find q expression"

getDeviceId :: EntityId -> DeviceId
getDeviceId (EntityId id) = DeviceId id

getSensorId :: AttributeId -> SensorId
getSensorId (AttributeId id) = SensorId id

getNotifNotif :: SubNotif -> SocialMessageBatch 
getNotifNotif (SubNotif (SubHttpCustom _ payload _ _) _ _ ["waziup_notif"]) = case JSON.decode <$> convertString $ urlDecode True $ convertString payload of
  Just d -> d
  Nothing -> error "Cannot decode payload" where
getNotifNotif _ = error "not a waziup_notif" 

getSubFromNotif :: Notif -> Subscription
getSubFromNotif (Notif nid desc sub socBatch throt) = Subscription {
  subId           = getSubId <$> nid, 
  subDescription  = desc,
  subSubject      = getSubSubject sub, 
  subNotification = getSubNotif socBatch,
  subThrottling   = throt}

getSubSubject :: NotifSubject -> SubSubject
getSubSubject (NotifSubject devs (NotifCond sens expr)) = 
  SubSubject {subEntities  = map (\(DeviceId did) -> SubEntity (EntityId did) Nothing) devs,
              subCondition = SubCondition {subCondAttrs      = map (\(SensorId sid) -> AttributeId sid) sens,
                                           subCondExpression = M.singleton "q" expr}}

getSubNotif :: SocialMessageBatch -> SubNotif
getSubNotif smb = SubNotif 
  {subHttpCustom = SubHttpCustom {subUrl = "http://localhost:3000/api/v2/socials/batch",
                                  subPayload = convertString $ URI.urlEncode True $ convertString $ JSON.encode smb,
                                  subMethod = "POST",
                                  subHeaders = fromList [("Content-Type", "application/json"), ("accept", "application/js")]},
   subAttrs = [],
   subAttrsFormat = "normalized",
   subMetadata = ["waziup_notif"]}

getSubId :: NotifId -> SubId
getSubId (NotifId id) = SubId id

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Notifs" s
info  s = liftIO $ infoM    "Notifs" s
warn  s = liftIO $ warningM "Notifs" s
err   s = liftIO $ errorM   "Notifs" s

