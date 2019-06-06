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
import           Control.Exception.Lifted (throwIO)
import           Data.String.Conversions
import           Data.Aeson
import           Data.Maybe
import           Data.Pool
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types.Status as HTS
import           Network.HTTP.Client as HC
import           Servant
import           Database.MongoDB as DB

-- * Lifting

-- Run Orion functions
liftOrion :: O.Orion a -> Waziup a
liftOrion orion = do
 conf <- view (waziupConfig.orionConf)
 e <- liftIO $ O.runOrion orion conf
 case e of
   Right res -> return res
   Left err -> throwError $ fromOrionError err

-- * Run a Keycloak function with default guest token
liftKeycloak :: Maybe KC.Token -> (Token -> KC.Keycloak a) -> Waziup a
liftKeycloak mtok kc = do
 tok <- case mtok of
   Just tok2 -> return tok2
   Nothing -> do
     guestId   <- view (waziupConfig.serverConf.guestLogin)
     guestPass <- view (waziupConfig.serverConf.guestPassword)
     -- retrieve guest token
     liftKeycloak' (getUserAuthToken guestId guestPass)
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
  (WaziupInfo dBPool conf _) <- ask
  dBPool <- view dbPool 
  muser  <- view $ waziupConfig.mongoConf.mongoUser
  mpass  <- view $ waziupConfig.mongoConf.mongoPass
  liftIO $ withResource dBPool $ \p -> DB.access p DB.master "waziup" $ do
    case (muser, mpass) of
      (Just user, Just pass) -> do 
        res <- DB.auth user pass
        if res 
          then dbAction
          else throwIO $ DB.ConnectionFailure $ userError "auth failed."
      _ -> dbAction

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

maybeToList' :: Maybe [a] -> [a]
maybeToList' (Just a) = a
maybeToList' Nothing = []
