{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Notifs where

import           Waziup.Types as T
import           Waziup.Utils
import           Waziup.Auth hiding (info, warn, debug, err)
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
import           Safe

-- * Notif API

getNotifs :: Maybe Token -> Waziup [Notif]
getNotifs tok = do 
  info "Get notifs"
  subs <- liftOrion $ O.getSubs
  let notifs = catMaybes $ map getNotifFromSub subs
  ps <- getPermsDevices tok
  debug $ "Perms" ++ (show ps)
  let notifs2 = L.filter (isPermitted ps) notifs
  return notifs2 where
  isPermitted :: [Perm] -> Notif -> Bool
  isPermitted ps notif = case (notifDevices $ notifCondition notif) of
    [] -> False
    a:as -> checkPermResource' DevicesView ps (PermDeviceId a) --Only the first device is used to check permission 
                                              
postNotif :: Maybe Token -> Notif -> Waziup NotifId
postNotif tok not = do
  info $ "Post notif: " ++ (show not)
  notifMinInterval <- view (waziupConfig.serverConf.notifMinInterval)
  let not2 = not {notifThrottling = max notifMinInterval (notifThrottling not)}
  host <- view (waziupConfig.serverConf.serverHost)
  sub <- getSubFromNotif not2 host
  debug $ "sub: " ++ (convertString $ (encode sub) :: String)
  (SubId res) <- liftOrion $ O.postSub sub
  return $ NotifId res 

getNotif :: Maybe Token -> NotifId -> Waziup Notif
getNotif tok (NotifId id) = do
  info "Get notif"
  sub <- liftOrion $ O.getSub (SubId id) 
  let mnotif = getNotifFromSub sub
  case mnotif of
    Just n -> return n
    Nothing -> do
      warn "Not a Waziup notification"
      throwError err400 {errBody = "Not a Waziup notification"}

patchNotif :: Maybe Token -> NotifId -> Notif -> Waziup NoContent
patchNotif tok (NotifId id) not = do
  info $ "Patch notif: " ++ (show not)
  notifMinInterval <- view (waziupConfig.serverConf.notifMinInterval)
  let not2 = not {notifThrottling = max notifMinInterval (notifThrottling not)}
  host <- view (waziupConfig.serverConf.serverHost)
  sub <- getSubFromNotif not2 host
  debug $ "sub: " ++ (convertString $ (encode sub) :: String)
  liftOrion $ O.patchSub (SubId id) sub
  return NoContent

deleteNotif :: Maybe Token -> NotifId -> Waziup NoContent
deleteNotif tok (NotifId id) = do
  info "Delete notif"
  liftOrion $ O.deleteSub (SubId id)
  return NoContent

putNotifStatus :: Maybe Token -> NotifId -> SubStatus -> Waziup NoContent
putNotifStatus tok nid status = do
  info "Put notif status"
  notif <- getNotif tok nid
  let not2 = notif {notifStatus = Just status}
  host <- view (waziupConfig.serverConf.serverHost)
  sub <- getSubFromNotif not2 host
  liftOrion $ O.patchSub (SubId $ unNotifId nid) sub 
  return NoContent

-- * Helpers

getNotifFromSub :: Subscription -> Maybe Notif
getNotifFromSub (Subscription subId subDesc subSubject subNotif subThrottling subStat subExp) = 
  case getNotifAction subNotif of
    Just action -> Just $ Notif { notifId                = getNotifId <$> subId 
                                , notifDescription       = subDesc 
                                , notifCondition         = getNotifCondition subSubject 
                                , notifAction            = action 
                                , notifThrottling        = subThrottling
                                , notifStatus            = subStat 
                                , notifTimesSent         = subTimesSent subNotif 
                                , notifLastNotif         = subLastNotification subNotif
                                , notifLastSuccess       = subLastSuccess subNotif
                                , notifLastSuccessCode   = subLastSuccessCode subNotif
                                , notifLastFailure       = subLastFailure subNotif
                                , notifLastFailureReason = subLastFailureReason subNotif
                                , notifExpires           = subExp}
    Nothing -> Nothing

getNotifId :: SubId -> NotifId
getNotifId (SubId sid) = NotifId sid

getNotifCondition :: SubSubject -> NotifCondition
getNotifCondition (SubSubject ents (SubCondition attrs exp)) = NotifCondition
  { notifDevices    = map getDeviceId (map subEntId ents)
  , notifSensors    = map getSensorId attrs
  , notifExpression = q} where
    q = case convertString <$> exp !? "q" of
          Just q -> q
          Nothing -> error "Cannot find q expression"

getDeviceId :: EntityId -> DeviceId
getDeviceId (EntityId id) = DeviceId id

getSensorId :: AttributeId -> SensorId
getSensorId (AttributeId id) = SensorId id

getNotifAction :: SubNotif -> Maybe SocialMessageBatch
getNotifAction subNot = 
  if subMetadata subNot == ["waziup_notif"]
    then JSON.decode <$> convertString $ urlDecode True $ convertString $ subPayload $ subHttpCustom subNot
    else Nothing

getSubFromNotif :: Notif -> Text -> Waziup Subscription
getSubFromNotif (Notif nid desc sub not throt stat ts ln ls lsc lf lfr exp) host = do
  return Subscription {
  subId           = getSubId <$> nid, 
  subDescription  = desc,
  subSubject      = getSubSubject sub, 
  subNotification = getSubNotif not ts ln ls lsc lf lfr host,
  subThrottling   = throt,
  subStatus       = stat,
  subExpires      = exp}

getSubSubject :: NotifCondition -> SubSubject
getSubSubject (NotifCondition devs sens expr) = 
  SubSubject {subEntities  = map (\(DeviceId did) -> SubEntity (EntityId did) Nothing) devs,
              subCondition = SubCondition {subCondAttrs      = map (\(SensorId sid) -> AttributeId sid) sens,
                                           subCondExpression = M.singleton "q" expr}}

getSubNotif :: SocialMessageBatch -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe UTCTime -> Maybe String -> Text -> SubNotif
getSubNotif smb ts ln ls lsc lf lfr host = do
  SubNotif  {subHttpCustom = SubHttpCustom {subUrl = convertString $ host <> "/api/v2/socials/batch",
                                            subPayload = convertString $ URI.urlEncode True $ convertString $ JSON.encode smb,
                                            subMethod = "POST",
                                            subHeaders = fromList [("Content-Type", "application/json"), ("accept", "application/json")]},
             subAttrs             = [],
             subAttrsFormat       = "normalized",
             subMetadata          = ["waziup_notif"],
             subTimesSent         = ts, 
             subLastNotification  = ln,
             subLastSuccess       = ls,
             subLastSuccessCode   = lsc,
             subLastFailure       = lf,
             subLastFailureReason = lfr}

getSubId :: NotifId -> SubId
getSubId (NotifId id) = SubId id

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Notifs" s
info  s = liftIO $ infoM    "Notifs" s
warn  s = liftIO $ warningM "Notifs" s
err   s = liftIO $ errorM   "Notifs" s

