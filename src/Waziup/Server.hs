{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waziup.Server where

import           Waziup.Types
import           Waziup.API
import           Waziup.Devices
import           Waziup.Ontologies
import           Waziup.Projects
import           Waziup.Sensors
import           Waziup.SensorData
import           Waziup.Actuators
import           Waziup.Users
import           Waziup.Socials
import           Waziup.Notifs
import           Waziup.Gateways
import           Waziup.Auth
import qualified Keycloak.Types as KC
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens hiding ((.=))
import           Data.Proxy (Proxy(..))
import qualified Data.Swagger as S
import qualified Data.ByteString.Lazy as BL
import           Data.Set
import           Data.String.Conversions
import           Servant
import           Servant.Server
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Log.Logger

-- * Building the server

server :: ServerT API Waziup
server = serverWaziup
    :<|> serverDocs
    :<|> redir

-- redirect root queries to docs
redir :: ServerT Redir Waziup
redir = do
  wi <- ask
  let host = _serverHost $ _serverConf $ _waziupConfig wi
  throwError $ err301 { errHeaders = [("Location", convertString $ host <> "/docs")] }

-- All API servers
serverWaziup :: ServerT WaziupAPI Waziup
serverWaziup = authServer
          :<|> devicesServer
          :<|> sensorsServer
          :<|> actuatorsServer
          :<|> sensorDataServer
          :<|> gatewaysServer
          :<|> projectsServer
          :<|> usersServer
          :<|> socialsServer
          :<|> notifsServer
          :<|> ontologiesServer

-- Docs server
serverDocs :: ServerT WaziupDocs Waziup
serverDocs = hoistDocs $ swaggerSchemaUIServer swaggerDoc

-- Authentication server
authServer :: ServerT AuthAPI Waziup
authServer = getPermsDevices
        :<|> getPermsProjects
        :<|> getPermsGateways
        :<|> postAuth

-- Devices servers
devicesServer :: ServerT DevicesAPI Waziup
devicesServer = getDevices
           :<|> postDevice
           :<|> getDevice
           :<|> deleteDevice
           :<|> putDeviceName
           :<|> putDeviceLocation
           :<|> putDeviceGatewayId
           :<|> putDeviceVisibility
           :<|> putDeviceDeployed
           :<|> putDeviceOwner

-- Sensors server
sensorsServer :: ServerT SensorsAPI Waziup
sensorsServer = getSensors
           :<|> postSensor
           :<|> getSensor
           :<|> deleteSensor
           :<|> putSensorName
           :<|> putSensorSensorKind
           :<|> putSensorQuantityKind
           :<|> putSensorUnit
           :<|> putSensorCalib
           :<|> putSensorValue
           :<|> putSensorValues

-- Sensor data server
sensorDataServer :: ServerT SensorDataAPI Waziup
sensorDataServer = getDatapoints

-- Gateways server
gatewaysServer :: ServerT GatewaysAPI Waziup
gatewaysServer = getGateways
            :<|> postGateway
            :<|> getGateway
            :<|> deleteGateway
            :<|> putHeartbeat
            :<|> putGatewayName
            :<|> putGatewayOwner
            :<|> putGatewayLocation

-- Actuators server
actuatorsServer :: ServerT ActuatorsAPI Waziup
actuatorsServer = getActuators
             :<|> postActuator
             :<|> getActuator
             :<|> deleteActuator
             :<|> putActuatorName
             :<|> putActActuatorKind
             :<|> putActuatorValueType
             :<|> putActuatorValue

-- users server
usersServer :: ServerT UsersAPI Waziup
usersServer = getUsers
         :<|> postUser
         :<|> getUser
         :<|> putUserCredit

-- socials server
socialsServer :: ServerT SocialsAPI Waziup
socialsServer = getSocialMessages
           :<|> postSocialMessage
           :<|> postSocialMessageBatch
           :<|> getSocialMessage
           :<|> deleteSocialMessage

--notifs server
notifsServer :: ServerT NotifsAPI Waziup
notifsServer = getNotifs
          :<|> postNotif
          :<|> getNotif
          :<|> patchNotif
          :<|> deleteNotif
          :<|> putNotifStatus

-- projects server
projectsServer :: ServerT ProjectsAPI Waziup
projectsServer = getProjects
            :<|> postProject
            :<|> getProject
            :<|> deleteProject
            :<|> putProjectDevices
            :<|> putProjectGateways
            :<|> putProjectName

-- ontologies server
ontologiesServer :: ServerT OntologiesAPI Waziup
ontologiesServer = getSensorKinds
              :<|> getActuatorKinds
              :<|> getQuantityKinds
              :<|> getUnits

-- final server
waziupServer :: WaziupInfo -> Application
waziupServer conf = serve waziupAPI $ Servant.Server.hoistServer waziupAPI (getHandler conf) server

-- Swagger docs
swaggerDoc :: S.Swagger
swaggerDoc = toSwagger (Proxy :: Proxy WaziupAPI)
  & S.info . S.title       .~ "Waziup API"
  & S.info . S.version     .~ "v2.0.0"
  & S.info . S.description ?~ "This API allows you to access all Waziup services.\n\
                              \In order to access protected services, first get a token with POST /auth/token.\n\
                              \Then insert this token in the authorization key, specifying “Bearer” in front. For example \"Bearer eyJhbGc…\"."
  & S.basePath ?~ "/api/v2"
  & S.applyTagsFor devicesOps ["Devices"]
  & S.applyTagsFor sensorOps  ["Sensors"]
  & S.applyTagsFor actuatOps  ["Actuators"]
  & S.applyTagsFor dataOps    ["Sensor Data"]
  & S.applyTagsFor authOps    ["Auth"]
  & S.applyTagsFor projectOps ["Projects"]
  & S.applyTagsFor userOps    ["Users"]
  & S.applyTagsFor ontoOps    ["Ontologies"]
  & S.applyTagsFor gwsOps     ["Gateways"]
  & S.applyTagsFor socialsOps ["Socials"]
  & S.applyTagsFor notifsOps  ["Notifs"]
  & S.tags .~ (fromList [])
  where
    devicesOps, sensorOps, actuatOps, dataOps, authOps, projectOps, userOps, ontoOps, gwsOps, socialsOps, notifsOps :: Traversal' S.Swagger S.Operation
    devicesOps = subOperations (Proxy :: Proxy DevicesAPI)    (Proxy :: Proxy WaziupAPI)
    sensorOps  = subOperations (Proxy :: Proxy SensorsAPI)    (Proxy :: Proxy WaziupAPI)
    actuatOps  = subOperations (Proxy :: Proxy ActuatorsAPI)  (Proxy :: Proxy WaziupAPI)
    dataOps    = subOperations (Proxy :: Proxy SensorDataAPI) (Proxy :: Proxy WaziupAPI)
    authOps    = subOperations (Proxy :: Proxy AuthAPI)       (Proxy :: Proxy WaziupAPI)
    projectOps = subOperations (Proxy :: Proxy ProjectsAPI)   (Proxy :: Proxy WaziupAPI)
    userOps    = subOperations (Proxy :: Proxy UsersAPI)      (Proxy :: Proxy WaziupAPI)
    ontoOps    = subOperations (Proxy :: Proxy OntologiesAPI) (Proxy :: Proxy WaziupAPI)
    gwsOps     = subOperations (Proxy :: Proxy GatewaysAPI)   (Proxy :: Proxy WaziupAPI)
    socialsOps = subOperations (Proxy :: Proxy SocialsAPI)    (Proxy :: Proxy WaziupAPI)
    notifsOps  = subOperations (Proxy :: Proxy NotifsAPI)     (Proxy :: Proxy WaziupAPI)

-- * helpers

waziupAPI :: Proxy API
waziupAPI = Proxy

getHandler :: WaziupInfo -> Waziup a -> Servant.Handler a
getHandler s x = runReaderT x s

hoistDocs :: ServerT WaziupDocs Servant.Handler -> ServerT WaziupDocs Waziup
hoistDocs s = Servant.Server.hoistServer (Proxy :: Proxy WaziupDocs) lift s


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info  s = liftIO $ infoM "API" s
warn  s = liftIO $ warningM "API" s
err   s = liftIO $ errorM "API" s

instance MimeRender PlainText KC.Token where
  mimeRender _ (KC.Token tok) = BL.fromStrict tok
