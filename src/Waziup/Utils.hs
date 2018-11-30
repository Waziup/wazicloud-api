{-# LANGUAGE OverloadedStrings #-}

module Waziup.Utils where

import           Waziup.Types
import qualified Orion as O
import           Keycloak as KC
import           Control.Monad.Except (throwError, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.String.Conversions
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types.Status as HTS
import           Network.HTTP.Client as HC
import           Servant
import           Database.MongoDB as DB

-- * Lifting
runOrion :: O.Orion a -> Waziup a
runOrion orion = do
 (WaziupInfo _ (WaziupConfig _ _ _ conf) _) <- ask
 e <- liftIO $ runExceptT $ runReaderT orion conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromOrionError err

runKeycloak :: KC.Keycloak a -> Waziup a
runKeycloak kc = do
 (WaziupInfo _ (WaziupConfig _ _ conf _) _) <- ask
 e <- liftIO $ runExceptT $ runReaderT kc conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromKCError err

runMongo :: Action IO a -> Waziup a
runMongo dbAction = do
  (WaziupInfo pipe _ _) <- ask
  liftIO $ access pipe DB.master "projects" dbAction

-- * error convertions
fromOrionError :: O.OrionError -> ServantErr
fromOrionError (O.HTTPError (HttpExceptionRequest _ (StatusCodeException r m))) 
  = ServantErr { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                 errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                 errBody         = BL.fromStrict ("Error from Orion: " <> m),
                 errHeaders      = []}
fromOrionError (O.HTTPError (HttpExceptionRequest _ (HC.ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Orion: " ++ show a} 
fromOrionError (O.HTTPError s) = err500 {errBody = encode $ show s} 
fromOrionError (O.ParseError s) = err500 {errBody = encode s} 
fromOrionError O.EmptyError = err500 {errBody = "EmptyError"}

fromKCError :: KC.KCError -> ServantErr
fromKCError (KC.HTTPError (HttpExceptionRequest _ (StatusCodeException r m)))
  = ServantErr { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                 errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                 errBody         = BL.fromStrict ("Error from Keycloak: " <> m),
                 errHeaders      = []}
fromKCError (KC.HTTPError (HttpExceptionRequest _ (HC.ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Keycloak: " ++ show a} 
fromKCError (KC.HTTPError s) = err500 {errBody = encode $ show s} 
fromKCError (KC.ParseError s) = err500 {errBody = encode s} 
fromKCError KC.EmptyError = err500 {errBody = "EmptyError"}


