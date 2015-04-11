{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Nero.Wai where

import Nero.Prelude
import Control.Monad ((<=<))
import Control.Arrow (first, second)
import Data.Maybe (fromMaybe, maybeToList)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T (fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Map (Map)
import Network.HTTP.Types as Wai
  ( ok200
  , notFound404
  , movedPermanently301
  , queryToQueryText
  )
import qualified Network.Wai as Wai
import qualified Nero
import Nero.Url as Nero(Url(..), Scheme(..), Location(..))
import Nero.Response as Nero(_Ok, _MovedPermanently, _NotFound)
import qualified Nero.Param (fromList)
import qualified Nero.Binary as Nero (render)
import Control.Lens.Extras (is)

type Headers = Map ByteString ByteString

waify :: Nero.Application -> Wai.Application
waify neroApp = waifyResponse . (neroApp <=< fromWaiRequest)

fromWaiRequest :: Wai.Request -> IO Nero.Request
fromWaiRequest waiRequest =
    case Wai.requestMethod waiRequest of
        "GET" -> pure $ Nero.get (urlFromWaiRequest waiRequest)
        _     -> error "fromWaiRequest: Not implemented yet"

urlFromWaiRequest :: Wai.Request -> Nero.Url
urlFromWaiRequest = Nero.Url
    <$> (\req -> if Wai.isSecure req
                    then Nero.Https
                    else Nero.Http)
    <*> L.fromStrict . fromMaybe mempty . Wai.requestHeaderHost
    -- TODO: Does WAI server urldecode?
    <*> decodeUtf8 . L.fromStrict . Wai.rawPathInfo
    <*> Nero.Param.fromList . fmap ( first T.fromStrict
                                   . second (maybeToList . fmap T.fromStrict)
                                   )
                            . Wai.queryToQueryText
                            . Wai.queryString
    -- TODO: Wai URL fragments?

toWaiResponse :: Nero.Response -> Wai.Response
toWaiResponse neroResponse
    | is Nero._Ok neroResponse       = go Wai.ok200
    | is Nero._NotFound neroResponse = go Wai.notFound404
    | is Nero._MovedPermanently neroResponse =
        -- TODO: URL decode?
        Wai.responseLBS Wai.movedPermanently301
          [("Location", L.toStrict . Nero.render $ neroResponse ^?! location)]
          mempty
    | otherwise = error "toWaiResponse: Not implemented yet"
  where
    go st = Wai.responseLBS st [] (Nero.body neroResponse)

waifyResponse :: IO Nero.Response
              -> (Wai.Response -> IO Wai.ResponseReceived)
              -> IO Wai.ResponseReceived
waifyResponse ioNeroResponse respond =
    respond =<< toWaiResponse <$> ioNeroResponse
