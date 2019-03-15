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

getNotifs :: Maybe Token -> Waziup [Notif]
getNotifs tok = do 
  info "Get notifs"
  undefined
  --subs <- liftOrion $ O.getSubscritions
  --let devices = catMaybes $ map getDeviceFromEntity entities
  --ps <- getPerms tok
  --let devices2 = filter (checkPermDevice DevicesView ps . devId) devices -- TODO limits
  --return devices2
                                              
postNotif :: Maybe Token -> Notif -> Waziup NotifId
postNotif tok not = undefined

getNotif :: Maybe Token -> NotifId -> Waziup Notif
getNotif tok not = undefined

deleteNotif :: Maybe Token -> NotifId -> Waziup NoContent
deleteNotif tok soc = undefined

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Notifs" s
info  s = liftIO $ infoM    "Notifs" s
warn  s = liftIO $ warningM "Notifs" s
err   s = liftIO $ errorM   "Notifs" s

