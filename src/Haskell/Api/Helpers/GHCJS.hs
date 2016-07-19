{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskell.Api.Helpers.GHCJS (
  ApiOptions (..),
  ApiError (..),
  ApiEff,
  RawApiResult,
  QueryParam (..),
  defaultApiOptions,
  route,
  flattenParams,
  mkQueryString,
  routeQueryBy,
  runDebug,
  urlFromReader,
  handleError,
  getAt,
  postAt,
  putAt,
  deleteAt,
  runDefault,
  rD,
  runWith,
  rW
) where



import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             encode)
import           Data.Default               (Default, def)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (dropWhileEnd, intercalate)
import qualified Data.Text.IO               as TextIO (putStrLn)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           JavaScript.Ajax            (AjaxResponse (..), StdMethod (..),
                                             sendRequest)
import           Network.HTTP.Types         (Status (..))
import           Prelude                    hiding (log)




type ApiEff       = ReaderT ApiOptions IO


-- | Raw API Result, which can include an Error + Message, or the Response Body
-- (Status, ByteString) is redundant because Status contains a statusMessage (ByteString).
-- However, we are potentially pulling the response body from the content of the header: X-jSON-ERROR.
-- This is because we don't have access to the body of the message in an exception.
--
type RawApiResult = Either (Status, Text) Text



data ApiOptions = ApiOptions {
  apiUrl    :: Text,
  apiPrefix :: Text,
  apiDebug  :: Bool
} deriving (Show, Generic, Typeable)



data ApiError b
  = ServerError Status b
  | DecodeError Text
  deriving (Show)



class QueryParam a where
  qp :: a -> (Text, Text)



instance QueryParam (Text, Text) where
  qp (t,t') = (t, t')



route :: Text -> [Text] -> Text
route url paths = Text.intercalate "/" (url : paths)



flattenParams :: QueryParam qp => [qp] -> [Text]
flattenParams [] = []
flattenParams params' = map (\par -> let (k,v) = qp par in k <> "=" <> v) params'



mkQueryString :: [Text] -> Text
mkQueryString [] = ""
mkQueryString params' = "?" <> Text.intercalate "&" params'



routeQueryBy :: QueryParam qp => Text -> [Text] -> [qp] -> Text
routeQueryBy url paths params' = route url paths <> mkQueryString (flattenParams params')



runDebug :: ApiEff () -> ApiEff ()
runDebug fn = do
  debug <- asks apiDebug
  if debug
     then do
       fn
       pure ()
     else pure ()



urlFromReader :: ApiEff Text
urlFromReader = do
  ApiOptions{..} <- ask
  let
    apiUrl'    = Text.dropWhileEnd (=='/') apiUrl
    apiPrefix' = Text.dropWhileEnd (=='/') apiPrefix
  pure $ apiUrl' <> "/" <> apiPrefix'



defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions {
  apiUrl         = "https://github.com",
  apiPrefix      = "api",
  apiDebug       = True
}



runDefault :: ReaderT ApiOptions m a -> m a
runDefault actions = runReaderT actions defaultApiOptions



rD :: ReaderT ApiOptions m a -> m a
rD = runDefault



runWith :: ReaderT ApiOptions m a -> ApiOptions -> m a
runWith = runReaderT



rW :: ReaderT ApiOptions m a -> ApiOptions -> m a
rW = runWith



_eitherDecode :: (FromJSON a, Default a) => Text -> a
_eitherDecode text =
  case eitherDecode $ cs text of
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



getAt :: (QueryParam qp)  => [qp] -> [Text] -> ApiEff RawApiResult
getAt params' paths = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("getAt: " <> url'))
  internalAction GET url' (Nothing :: Maybe ())



postAt :: (QueryParam qp, ToJSON body) => [qp] -> [Text] -> body -> ApiEff RawApiResult
postAt params' paths body = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("postAt: " <> url'))
  internalAction POST url' (Just body)



putAt :: (QueryParam qp, ToJSON body) => [qp] -> [Text] -> body -> ApiEff RawApiResult
putAt params' paths body = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("putAt: " <> url'))
  internalAction PUT url' (Just body)



deleteAt :: QueryParam qp => [qp] -> [Text] -> ApiEff RawApiResult
deleteAt params' paths = do

  url <- urlFromReader

  let url' = routeQueryBy url paths params'
  runDebug (log ("deleteAt: " <> url'))
  internalAction DELETE url' (Nothing :: Maybe ())



log :: MonadIO m => Text -> m ()
log s = liftIO $ TextIO.putStrLn s
