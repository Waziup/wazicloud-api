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
  sub <- getSubFromNotif not
  debug $ "sub: " ++ (convertString $ (encode sub) :: String)
  (SubId res) <- liftOrion $ O.postSub sub
  return $ NotifId res 

getNotif :: Maybe Token -> NotifId -> Waziup Notif
getNotif tok (NotifId id) = do
  info "Get notif"
  sub <- liftOrion $ O.getSub (SubId id) 
  let notif = getNotifFromSub sub
  return notif

deleteNotif :: Maybe Token -> NotifId -> Waziup NoContent
deleteNotif tok (NotifId id) = do
  info "Delete notif"
  liftOrion $ O.deleteSub (SubId id)
  return NoContent


getNotifFromSub :: Subscription -> Notif
getNotifFromSub (Subscription subId subDesc subSubject subNotif subThrottling subStat) = 
  Notif { notifId          = getNotifId <$> subId 
        , notifDescription = subDesc 
        , notifSubject     = getNotifSubject subSubject 
        , notifNotif       = getNotifNotif subNotif
        , notifThrottling  = subThrottling
        , notifStatus      = subStat} 

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

getNotifNotif :: SubNotif -> NotifNotif 
getNotifNotif (SubNotif (SubHttpCustom _ payload _ _) _ _ ["waziup_notif"] ts ln ls lf) = case JSON.decode <$> convertString $ urlDecode True $ convertString payload of
  Just d -> NotifNotif d ts ln ls lf
  Nothing -> error "Cannot decode payload" where
getNotifNotif _ = error "not a waziup_notif" 

getSubFromNotif :: Notif -> Waziup Subscription
getSubFromNotif (Notif nid desc sub not throt stat) = do
  subNot <- getSubNotif not
  return Subscription {
  subId           = getSubId <$> nid, 
  subDescription  = desc,
  subSubject      = getSubSubject sub, 
  subNotification = subNot,
  subThrottling   = throt,
  subStatus       = stat}

getSubSubject :: NotifSubject -> SubSubject
getSubSubject (NotifSubject devs (NotifCond sens expr)) = 
  SubSubject {subEntities  = map (\(DeviceId did) -> SubEntity (EntityId did) Nothing) devs,
              subCondition = SubCondition {subCondAttrs      = map (\(SensorId sid) -> AttributeId sid) sens,
                                           subCondExpression = M.singleton "q" expr}}

getSubNotif :: NotifNotif -> Waziup SubNotif
getSubNotif (NotifNotif smb ts ln ls lf) = do
  host <- view (waziupConfig.serverConf.serverHost)
  return SubNotif  {subHttpCustom = SubHttpCustom {subUrl = convertString $ host <> "/api/v2/socials/batch",
                                                   subPayload = convertString $ URI.urlEncode True $ convertString $ JSON.encode smb,
                                                   subMethod = "POST",
                                                   subHeaders = fromList [("Content-Type", "application/json"), ("accept", "application/json")]},
                    subAttrs            = [],
                    subAttrsFormat      = "normalized",
                    subMetadata         = ["waziup_notif"],
                    subTimesSent        = ts, 
                    subLastNotification = ln,
                    subLastSuccess      = ls,
                    subLastFailure      = lf}

getSubId :: NotifId -> SubId
getSubId (NotifId id) = SubId id

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Notifs" s
info  s = liftIO $ infoM    "Notifs" s
warn  s = liftIO $ warningM "Notifs" s
err   s = liftIO $ errorM   "Notifs" s

