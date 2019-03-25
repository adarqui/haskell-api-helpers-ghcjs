{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskell.Api.Helpers (
  SpecificApiOptions (..),
  defaultSpecificApiOptions,
  handleError,
  getAt,
  postAt,
  putAt,
  deleteAt
) where



import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (FromJSON, ToJSON, eitherDecode,
                                                encode)
import           Data.ByteString.Lazy.Char8    (ByteString)
import           Data.Default                  (Default, def)
import           Data.Monoid                   ((<>))
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Debug.Trace
import           Haskell.Api.Helpers.Shared

import           Data.JSString.Text
import           JavaScript.Web.XMLHttpRequest
import           Network.HTTP.Types            (Status (..))



data SpecificApiOptions = SpecificApiOptions

instance Default SpecificApiOptions where
  def = defaultSpecificApiOptions

defaultSpecificApiOptions :: SpecificApiOptions
defaultSpecificApiOptions = SpecificApiOptions



_eitherDecode :: (FromJSON a, Default a) => ByteString -> a
_eitherDecode bs =
  case eitherDecode bs of
    Left _  -> def
    Right v -> v



handleError :: (FromJSON a, FromJSON b, Default b) => RawApiResult -> Either (ApiError b) a
handleError (Left (status, body)) = Left $ ServerError status (_eitherDecode body)
handleError (Right bs)    =
  let _ = trace "handleError" () in
  case eitherDecode (cs bs) of
    Left err -> Left $ DecodeError $ cs err
    Right a  -> Right a



internalAction
  :: (MonadIO m, ToJSON body)
  => Method         -- ^ method
  -> Text           -- ^ url
  -> Maybe body     -- ^ optional request body to encode
  -> m RawApiResult -- ^ result
internalAction method url m_body = do

  liftIO $ print ["internalAction", show method, show url]

  -- v <- liftIO (sendRequest method url (fmap (cs . encode) m_body) (Just "application/json") >>= properResponse)
  let req = Request{
    reqMethod = method,
    reqURI = textToJSString url,
    reqData = case m_body of
                             Just body -> StringData (textToJSString $ cs $ encode $ body)
                             Nothing   -> NoData,
    reqHeaders = [("Content-Type", "application/json")],
    reqLogin = Nothing,
    reqWithCredentials = False
  }

  v <- liftIO (xhrText req) >>= properResponse

  liftIO $ print $ show v

  pure v



properResponse :: (Monad m) => Response Text -> m RawApiResult
properResponse Response{..} =
   case status of
     200 -> pure $ Right $ maybe "" cs contents
     _   -> pure $ Left (Status {statusCode=status, statusMessage=""}, maybe "" cs contents)
-- properResponse _ = pure $ Left (Status {statusCode=0, statusMessage=""}, "")



getAt :: (QueryParam qp)  => [qp] -> [Text] -> ApiEff SpecificApiOptions RawApiResult
getAt params' paths = do

  liftIO $ print ("getAt 1" :: String)

  url <- urlFromReader

  liftIO $ print ("getAt 2" :: String)

  let url' = routeQueryBy url paths params'

  liftIO $ print ("getAt 3" :: String)

  runDebug (apiLog ("getAt: " <> url'))

  liftIO $ print ("getAt 4" :: String)

  internalAction GET url' (Nothing :: Maybe ())



postAt :: (QueryParam qp, ToJSON body) => [qp] -> [Text] -> body -> ApiEff SpecificApiOptions RawApiResult
postAt params' paths body = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("postAt: " <> url'))
  internalAction POST url' (Just body)



putAt :: (QueryParam qp, ToJSON body) => [qp] -> [Text] -> body -> ApiEff SpecificApiOptions RawApiResult
putAt params' paths body = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("putAt: " <> url'))
  internalAction PUT url' (Just body)



deleteAt :: QueryParam qp => [qp] -> [Text] -> ApiEff SpecificApiOptions RawApiResult
deleteAt params' paths = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("deleteAt: " <> url'))
  internalAction DELETE url' (Nothing :: Maybe ())
