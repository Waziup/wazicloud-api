{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Waziup.Utils where

import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text.Encoding
import Network.HTTP.Types.Status as HTS
import Servant
import Keycloak as KC hiding (info, warn, debug, Scope) 
import qualified Orion as O
import Control.Monad.Reader
import System.Log.Logger
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client as HC
import Data.Monoid

-- * Lifting
runOrion :: O.Orion a -> ExceptT ServantErr IO a
runOrion orion = withExceptT fromOrionError (runReaderT orion O.defaultOrionConfig)

runKeycloak :: KC.Keycloak a -> ExceptT ServantErr IO a
runKeycloak kc = withExceptT fromKCError (runReaderT kc defaultConfig)

-- * error convertions
fromOrionError :: O.OrionError -> ServantErr
fromOrionError (O.HTTPError (HttpExceptionRequest _ (StatusCodeException r m))) 
  = ServantErr { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                 errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                 errBody         = BL.fromStrict ("Error from Orion: " <> m),
                 errHeaders      = HC.responseHeaders r}
fromOrionError (O.HTTPError (HttpExceptionRequest _ (ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Orion: " ++ show a} 
fromOrionError (O.HTTPError s) = err500 {errBody = encode $ show s} 
fromOrionError (O.ParseError s) = err500 {errBody = encode s} 
fromOrionError O.EmptyError = err500 {errBody = "EmptyError"}

fromKCError :: KC.KCError -> ServantErr
fromKCError (KC.HTTPError (HttpExceptionRequest _ (StatusCodeException r m)))
  = ServantErr { errHTTPCode     = HTS.statusCode $ HC.responseStatus r, 
                 errReasonPhrase = show $ HTS.statusMessage $ HC.responseStatus r, 
                 errBody         = BL.fromStrict ("Error from Keycloak: " <> m),
                 errHeaders      = HC.responseHeaders r}
fromKCError (KC.HTTPError (HttpExceptionRequest _ (ConnectionFailure a))) = err500 {errBody = encode $ "Failed to connect to Keycloak: " ++ show a} 
fromKCError (KC.HTTPError s) = err500 {errBody = encode $ show s} 
fromKCError (KC.ParseError s) = err500 {errBody = encode s} 
fromKCError KC.EmptyError = err500 {errBody = "EmptyError"}


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "API" s
info s = liftIO $ infoM "API" s
warn s = liftIO $ warningM "API" s
err s = liftIO $ errorM "API" s

instance MimeRender PlainText Token where
  mimeRender _ (Token tok) = BL.fromStrict $ encodeUtf8 tok

