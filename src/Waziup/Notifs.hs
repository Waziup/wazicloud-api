{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Notifs where

import           Waziup.Types as T
import           Waziup.Utils hiding ((!?))
import           Waziup.Auth hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any, find)
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Aeson as JSON
import           Data.Time
import           Data.Map as M hiding (map)
import           Servant
import           Keycloak as KC hiding (Scope) 
import           System.Log.Logger
import           Control.Lens hiding ((.=))
import           Orion as O hiding (info, warn, debug, err)
import           Network.HTTP.Types.URI as URI

-- * Notif API

getNotifs :: AuthUser -> Waziup [Notif]
getNotifs tok = do 
  info "Get notifs"
  subs <- liftOrion $ O.getSubs
  let notifs = catMaybes $ map getNotifFromSub subs
  let notifs2 = L.filter (const True) notifs
  return notifs where
  --isPermitted :: [Perm] -> Notif -> Bool
  --isPermitted ps notif = case (notifDevices $ notifCondition notif) of
  --  [] -> False
  --  a:_ -> True -- isPermitted tok DevicesView (PermDeviceId a) --Only the first device is used to check permission 
                                              
postNotif :: AuthUser -> Notif -> Waziup NotifId
postNotif _ notif = do
  info $ "Post notif: " ++ (show notif)
  interval <- view (waziupConfig.serverConf.notifMinInterval)
  let not2 = notif {notifThrottling = max interval (notifThrottling notif)}
  host <- view (waziupConfig.serverConf.serverHost)
  sub <- getSubFromNotif not2 host
  debug $ "sub: " ++ (convertString $ (encode sub) :: String)
  (SubId res) <- liftOrion $ O.postSub sub
  return $ NotifId res 

getNotif :: AuthUser -> NotifId -> Waziup Notif
getNotif _ (NotifId nid) = do
  info "Get notif"
  sub <- liftOrion $ O.getSub (SubId nid) 
  let mnotif = getNotifFromSub sub
  case mnotif of
    Just n -> return n
    Nothing -> do
      warn "Not a Waziup notification"
      throwError err400 {errBody = "Not a Waziup notification"}

patchNotif :: AuthUser -> NotifId -> Notif -> Waziup NoContent
patchNotif _ (NotifId nid) notif = do
  info $ "Patch notif: " ++ (show notif)
  interval <- view (waziupConfig.serverConf.notifMinInterval)
  let not2 = notif {notifThrottling = max interval (notifThrottling notif)}
  host <- view (waziupConfig.serverConf.serverHost)
  sub <- getSubFromNotif not2 host
  debug $ "sub: " ++ (convertString $ (encode sub) :: String)
  liftOrion $ O.patchSub (SubId nid) sub
  return NoContent

deleteNotif :: AuthUser -> NotifId -> Waziup NoContent
deleteNotif _ (NotifId nid) = do
  info "Delete notif"
  liftOrion $ O.deleteSub (SubId nid)
  return NoContent

putNotifStatus :: AuthUser -> NotifId -> SubStatus -> Waziup NoContent
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
getNotifFromSub (Subscription sid desc subj notif throt stat expir) = 
  case getNotifAction notif of
    Just (Right action) -> Just $ Notif { notifId                = getNotifId <$> sid 
                                        , notifDescription       = desc 
                                        , notifCondition         = getNotifCondition subj 
                                        , notifAction            = Just action 
                                        , notifActuationAction   = Nothing 
                                        , notifThrottling        = throt
                                        , notifStatus            = stat 
                                        , notifTimesSent         = subTimesSent notif 
                                        , notifLastNotif         = subLastNotification notif
                                        , notifLastSuccess       = subLastSuccess notif
                                        , notifLastSuccessCode   = subLastSuccessCode notif
                                        , notifLastFailure       = subLastFailure notif
                                        , notifLastFailureReason = subLastFailureReason notif
                                        , notifExpires           = expir}
    Just (Left actuationAction) -> Just $ Notif { notifId                = getNotifId <$> sid 
                                        , notifDescription       = desc 
                                        , notifCondition         = getNotifCondition subj 
                                        , notifAction            = Nothing
                                        , notifActuationAction   = Just actuationAction 
                                        , notifThrottling        = throt
                                        , notifStatus            = stat 
                                        , notifTimesSent         = subTimesSent notif 
                                        , notifLastNotif         = subLastNotification notif
                                        , notifLastSuccess       = subLastSuccess notif
                                        , notifLastSuccessCode   = subLastSuccessCode notif
                                        , notifLastFailure       = subLastFailure notif
                                        , notifLastFailureReason = subLastFailureReason notif
                                        , notifExpires           = expir}
    Nothing -> Nothing

getNotifId :: SubId -> NotifId
getNotifId (SubId sid) = NotifId sid

getNotifCondition :: SubSubject -> NotifCondition
getNotifCondition (SubSubject ents (SubCondition attrs expir)) = NotifCondition
  { notifDevices    = map getDeviceId (map subEntId ents)
  , notifSensors    = map getSensorId attrs
  , notifExpression = qExpr} where
    qExpr = case convertString <$> expir !? "q" of
          Just q -> q
          Nothing -> error "Cannot find q expression"

getDeviceId :: EntityId -> DeviceId
getDeviceId (EntityId eid) = DeviceId eid

getSensorId :: AttributeId -> SensorId
getSensorId (AttributeId aid) = SensorId aid

getNotifAction :: SubNotif -> Maybe (Either ActuationValue SocialMessageBatch)
getNotifAction subNot = 
  case subMetadata subNot of
    ["waziup_notif"] -> Right <$> getNotifSocial (subHttpCustom subNot)
    ["waziup_actuation_notif"] -> Left <$> getNotifActuation (subHttpCustom subNot)
    _ -> Nothing where

getNotifSocial :: SubHttpCustom -> Maybe SocialMessageBatch
getNotifSocial (SubHttpCustom _ payload _ _) = JSON.decode <$> convertString $ urlDecode True $ convertString $ payload

getNotifActuation :: SubHttpCustom -> Maybe ActuationValue
getNotifActuation (SubHttpCustom url payload _ _) = case decodePathSegments $ extractPath $ convertString url of
  ("api":"v2":"devices":devId:"actuators":actId:_) -> case JSON.decode $ convertString $ urlDecode True $ convertString $ payload of
    Just val -> Just $ ActuationValue (DeviceId devId) (ActuatorId actId) val
    Nothing -> Nothing
  _ -> Nothing


getSubFromNotif :: Notif -> Text -> Waziup Subscription
getSubFromNotif (Notif nid desc sub soc act throt stat ts ln ls lsc lf lfr expi) host = do
  return Subscription {
  subId           = getSubId <$> nid, 
  subDescription  = desc,
  subSubject      = getSubSubject sub, 
  subNotification = getSubNotif soc act ts ln ls lsc lf lfr host,
  subThrottling   = throt,
  subStatus       = stat,
  subExpires      = expi}

getSubSubject :: NotifCondition -> SubSubject
getSubSubject (NotifCondition devs sens expr) = 
  SubSubject {subEntities  = map (\(DeviceId did) -> SubEntity (EntityId did) Nothing) devs,
              subCondition = SubCondition {subCondAttrs      = map (\(SensorId sid) -> AttributeId sid) sens,
                                           subCondExpression = M.singleton "q" expr}}

getSubNotif :: Maybe SocialMessageBatch -> Maybe ActuationValue -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe UTCTime -> Maybe String -> Text -> SubNotif
getSubNotif (Just smb) Nothing ts ln ls lsc lf lfr host = do
  SubNotif  {subHttpCustom = SubHttpCustom {subUrl = convertString $ host <> "/api/v2/socials/batch",
                                            subPayload = convertString $ urlEncodeForbiddens $ convertString $ JSON.encode smb,
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
getSubNotif Nothing (Just (ActuationValue (DeviceId devId) (ActuatorId actId) val)) ts ln ls lsc lf lfr host = do
  SubNotif  {subHttpCustom = SubHttpCustom {subUrl = convertString $ host <> "/api/v2/devices/" <> devId <> "/actuators/" <> actId <> "/value",
                                            subPayload = convertString $ URI.urlEncode True $ convertString $ JSON.encode val,
                                            subMethod = "PUT",
                                            subHeaders = fromList [("Content-Type", "application/json"), ("accept", "application/json")]},
             subAttrs             = [],
             subAttrsFormat       = "normalized",
             subMetadata          = ["waziup_actuation_notif"],
             subTimesSent         = ts, 
             subLastNotification  = ln,
             subLastSuccess       = ls,
             subLastSuccessCode   = lsc,
             subLastFailure       = lf,
             subLastFailureReason = lfr}

getSubId :: NotifId -> SubId
getSubId (NotifId nid) = SubId nid



-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Notifs" s
info  s = liftIO $ infoM    "Notifs" s
warn  s = liftIO $ warningM "Notifs" s
err   s = liftIO $ errorM   "Notifs" s

