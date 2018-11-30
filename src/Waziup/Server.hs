{-# LANGUAGE OverloadedStrings #-}

module Waziup.Server where

import           Waziup.Types
import           Waziup.API
import           Waziup.Sensors
import           Waziup.Ontologies
import           Waziup.Projects
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens hiding ((.=))
import           Data.Proxy (Proxy(..))
import qualified Data.Swagger as S
import           Servant
import           Servant.Server
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Log.Logger

server :: ServerT API Waziup
server = serverWaziup :<|> serverDocs

serverWaziup :: ServerT WaziupAPI Waziup
serverWaziup = authServer :<|> sensorsServer :<|> projectsServer :<|> ontologiesServer

serverDocs :: ServerT WaziupDocs Waziup
serverDocs = hoistDocs $ swaggerSchemaUIServer swaggerDoc

swaggerDoc :: S.Swagger
swaggerDoc = toSwagger (Proxy :: Proxy WaziupAPI)
  & S.info . S.title       .~ "Waziup API"
  & S.info . S.version     .~ "v2.0.0"
  & S.info . S.description ?~ "This is the API of Waziup"
  & S.basePath ?~ "/api/v1"
  & S.applyTagsFor sensorsOps ["Sensors"]
  & S.applyTagsFor authOps    ["Auth"]
  & S.applyTagsFor projectOps ["Projects"]
  & S.applyTagsFor ontoOps    ["Ontologies"]
  where
    sensorsOps, authOps, projectOps, ontoOps :: Traversal' S.Swagger S.Operation
    sensorsOps = subOperations (Proxy :: Proxy SensorsAPI)    (Proxy :: Proxy WaziupAPI)
    authOps    = subOperations (Proxy :: Proxy AuthAPI)       (Proxy :: Proxy WaziupAPI)
    projectOps = subOperations (Proxy :: Proxy ProjectsAPI)    (Proxy :: Proxy WaziupAPI)
    ontoOps    = subOperations (Proxy :: Proxy OntologiesAPI) (Proxy :: Proxy WaziupAPI)

authServer :: ServerT AuthAPI Waziup
authServer = getPerms :<|> postAuth

sensorsServer :: ServerT SensorsAPI Waziup
sensorsServer = getSensors :<|> postSensor :<|> getSensor :<|> deleteSensor

projectsServer :: ServerT ProjectsAPI Waziup
projectsServer = getProjects :<|> postProject :<|> getProject :<|> deleteProject :<|> putProjectDevices :<|> putProjectGateways

ontologiesServer :: ServerT OntologiesAPI Waziup
ontologiesServer = getSensingDevices :<|> getQuantityKinds :<|> getUnits


-- * Server

waziupAPI :: Proxy API
waziupAPI = Proxy

getHandler :: WaziupInfo -> Waziup a -> Servant.Handler a
getHandler s x = runReaderT x s

waziupServer :: WaziupInfo -> Application
waziupServer conf = serve waziupAPI $ Servant.Server.hoistServer waziupAPI (getHandler conf) server

hoistDocs :: ServerT WaziupDocs Servant.Handler -> ServerT WaziupDocs Waziup
hoistDocs s = Servant.Server.hoistServer (Proxy :: Proxy WaziupDocs) lift s


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info  s = liftIO $ infoM "API" s
warn  s = liftIO $ warningM "API" s
err   s = liftIO $ errorM "API" s

