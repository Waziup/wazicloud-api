{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Servant.Swagger.Tags
import           Servant.Auth.Server
import           Servant.Auth.Swagger
import           Servant.API.Flatten
import           System.Log.Logger

-- * Building the server

-- complete server, including Waziup APIs, documentation and redirection
server :: ServerT API Waziup
server = waziupServer 
    :<|> docsServer
    :<|> redir

-- Waziup server with all resources and authentication token
waziupServer :: ServerT WaziupAPI Waziup
waziupServer = resourcesServer
          :<|> postAuth

-- redirect root queries to docs
redir :: ServerT Redir Waziup
redir = throwError $ err301 { errHeaders = [("Location", "/docs")] }

-- All API servers
resourcesServer :: AuthResult User -> ServerT ResourcesAPI Waziup
resourcesServer u = authServer u
         :<|> devicesServer u
         :<|> sensorsServer u
         :<|> actuatorsServer u
         :<|> sensorDataServer u
         :<|> gatewaysServer u
         :<|> projectsServer u
         :<|> usersServer u
         :<|> socialsServer u
         :<|> notifsServer u
         :<|> ontologiesServer 

-- Docs server
docsServer :: ServerT DocsAPI Waziup
docsServer = hoistDocs $ swaggerSchemaUIServer swaggerDoc

-- Authorization server
authServer :: AuthResult User -> ServerT AuthAPI Waziup
authServer u = getPermsDevices u
          :<|> getPermsProjects u
          :<|> getPermsGateways u

-- Devices servers
devicesServer :: AuthResult User -> ServerT DevicesAPI Waziup
devicesServer u = getDevices u
           :<|> postDevice u
           :<|> getDevice u
           :<|> deleteDevice u
           :<|> putDeviceName u
           :<|> putDeviceLocation u
           :<|> putDeviceGatewayId u
           :<|> putDeviceVisibility u
           :<|> putDeviceDeployed u
           :<|> putDeviceOwner u

-- Sensors server
sensorsServer :: AuthResult User -> ServerT SensorsAPI Waziup
sensorsServer u = getSensors u
           :<|> postSensor u
           :<|> getSensor u
           :<|> deleteSensor u
           :<|> putSensorName u
           :<|> putSensorSensorKind u
           :<|> putSensorQuantityKind u
           :<|> putSensorUnit u
           :<|> putSensorCalib u
           :<|> putSensorValue u
           :<|> putSensorValues u
           :<|> getSensorValues u

-- Sensor data server
sensorDataServer :: AuthResult User -> ServerT SensorDataAPI Waziup
sensorDataServer u = getDatapoints u

-- Gateways server
gatewaysServer :: AuthResult User -> ServerT GatewaysAPI Waziup
gatewaysServer u = getGateways u
            :<|> postGateway u
            :<|> getGateway u
            :<|> deleteGateway u
            :<|> putHeartbeat u
            :<|> putGatewayName u
            :<|> putGatewayOwner u
            :<|> putGatewayLocation u

-- Actuators server
actuatorsServer :: AuthResult User -> ServerT ActuatorsAPI Waziup
actuatorsServer u = getActuators u
             :<|> postActuator u
             :<|> getActuator u
             :<|> deleteActuator u
             :<|> putActuatorName u
             :<|> putActActuatorKind u
             :<|> putActuatorValueType u
             :<|> putActuatorValue u

-- users server
usersServer :: AuthResult User -> ServerT UsersAPI Waziup
usersServer u = 
                getUsers u
           :<|> postUser u
           :<|> getUsersMe u
           :<|> getUser u
           :<|> putUserCredit u

-- socials server
socialsServer :: AuthResult User -> ServerT SocialsAPI Waziup
socialsServer u = getSocialMessages u
           :<|> postSocialMessage u
           :<|> postSocialMessageBatch u
           :<|> getSocialMessage u
           :<|> deleteSocialMessage u

--notifs server
notifsServer :: AuthResult User -> ServerT NotifsAPI Waziup
notifsServer u = getNotifs u
          :<|> postNotif u
          :<|> getNotif u
          :<|> patchNotif u
          :<|> deleteNotif u
          :<|> putNotifStatus u 

-- projects server
projectsServer :: AuthResult User -> ServerT ProjectsAPI Waziup
projectsServer u = getProjects u
            :<|> postProject u
            :<|> getProject u
            :<|> deleteProject u
            :<|> putProjectDevices u
            :<|> putProjectGateways u
            :<|> putProjectName u

-- ontologies server
ontologiesServer :: ServerT OntologiesAPI Waziup
ontologiesServer = getSensorKinds
              :<|> getActuatorKinds
              :<|> getQuantityKinds
              :<|> getUnits


-- Swagger docs
swaggerDoc :: S.Swagger
swaggerDoc = toSwagger ((Proxy :: Proxy WaziupAPI))
  & S.basePath ?~ "/api/v2"
  & S.info . S.title       .~ "Waziup API"
  & S.info . S.version     .~ "v2.0.0"
  & S.info . S.description ?~ "This API allows you to access all Waziup services.\n\
                              \In order to access protected services, first get a token with POST /auth/token.\n\
                              \Then insert this token in the authorization key, specifying “Bearer” in front. For example \"Bearer eyJhbGc…\"."

-- * helpers

getHandler :: WaziupInfo -> Waziup a -> Servant.Handler a
getHandler s x = runReaderT x s

hoistDocs :: ServerT DocsAPI Servant.Handler -> ServerT DocsAPI Waziup
hoistDocs s = Servant.Server.hoistServer (Proxy :: Proxy DocsAPI) lift s


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info  s = liftIO $ infoM "API" s
warn  s = liftIO $ warningM "API" s
err   s = liftIO $ errorM "API" s
