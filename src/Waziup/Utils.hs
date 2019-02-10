{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Waziup.Utils where

import           Waziup.Types
import qualified Orion as O
import           Keycloak as KC
import           Control.Monad.Except (throwError, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens
import           Data.String.Conversions
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types.Status as HTS
import           Network.HTTP.Client as HC
import           Servant
import           Database.MongoDB as DB

-- * Lifting

-- Run Orion functions
runOrion :: O.Orion a -> Waziup a
runOrion orion = do
 (WaziupInfo _ (WaziupConfig _ _ _ conf) _) <- ask
 e <- liftIO $ runExceptT $ runReaderT orion conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromOrionError err

-- * Run a Keycloak function with default guest token
liftKeycloak :: Maybe KC.Token -> (Token -> KC.Keycloak a) -> Waziup a
liftKeycloak mtok kc = do
 guestId   <- view (waziupConfig.serverConf.guestLogin)
 guestPass <- view (waziupConfig.serverConf.guestPassword)
 tok <- case mtok of
   Just tok -> return tok
   Nothing -> liftKeycloak' (getUserAuthToken guestId guestPass)
 liftKeycloak' (kc tok)

-- * Run Keycloak function
liftKeycloak' :: KC.Keycloak a -> Waziup a
liftKeycloak' kc = do
 conf <- view $ waziupConfig.keycloakConf
 e <- liftIO $ runKeycloak kc conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromKCError err

-- * run Mongo function
runMongo :: Action IO a -> Waziup a
runMongo dbAction = do
  (WaziupInfo pipe _ _) <- ask
  liftIO $ access pipe DB.master "waziup" dbAction

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


-- Lens setter for Maybe values
(.~?) :: Setter s t b b -> Maybe b -> s -> t
(.~?) x y =  x %~ (\v -> fromMaybe v y)


