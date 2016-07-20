{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskell.Api.Helpers.GHCJS (
  handleError,
  getAt,
  postAt,
  putAt,
  deleteAt
) where



import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             encode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default               (Default, def)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import           Haskell.Api.Helpers.Shared
import           JavaScript.Ajax            (AjaxResponse (..), StdMethod (..),
                                             sendRequest)
import           Network.HTTP.Types         (Status (..))



data UselessOptions = UselessOptions

instance Default UselessOptions where
  def = UselessOptions



_eitherDecode :: (FromJSON a, Default a) => ByteString -> a
_eitherDecode bs =
  case eitherDecode bs of
    Left _  -> def
    Right v -> v



handleError :: (FromJSON a, FromJSON b, Default b) => RawApiResult -> Either (ApiError b) a
handleError (Left (status, body)) = Left $ ServerError status (_eitherDecode body)
handleError (Right bs)    =
  case eitherDecode (cs bs) of
    Left err -> Left $ DecodeError $ cs err
    Right a  -> Right a



internalAction
  :: (MonadIO m, ToJSON body)
  => StdMethod      -- ^ method
  -> Text           -- ^ url
  -> Maybe body     -- ^ optional request body to encode
  -> m RawApiResult -- ^ result
internalAction method url m_body =
  liftIO (sendRequest method url (fmap (cs . encode) m_body) (Just "application/json") >>= properResponse)



-- properResponse :: (Monad m, FromJSON body) => AjaxResponse -> m RawApiResult
properResponse :: (Monad m) => AjaxResponse -> m RawApiResult
properResponse AjaxResponse{..} =
  case ar_status of
    (Status 200 _) -> pure $ Right $ cs ar_body
    _              -> pure $ Left (ar_status, cs ar_body)



getAt :: (QueryParam qp)  => [qp] -> [Text] -> ApiEff UselessOptions RawApiResult
getAt params' paths = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("getAt: " <> url'))
  internalAction GET url' (Nothing :: Maybe ())



postAt :: (QueryParam qp, ToJSON body) => [qp] -> [Text] -> body -> ApiEff UselessOptions RawApiResult
postAt params' paths body = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("postAt: " <> url'))
  internalAction POST url' (Just body)



putAt :: (QueryParam qp, ToJSON body) => [qp] -> [Text] -> body -> ApiEff UselessOptions RawApiResult
putAt params' paths body = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("putAt: " <> url'))
  internalAction PUT url' (Just body)



deleteAt :: QueryParam qp => [qp] -> [Text] -> ApiEff UselessOptions RawApiResult
deleteAt params' paths = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (apiLog ("deleteAt: " <> url'))
  internalAction DELETE url' (Nothing :: Maybe ())
