{-# LANGUAGE OverloadedStrings #-}

module YaWai where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (ResponseHeaders, Status, internalServerError500, status200, status302, hLocation)
import Network.Wai (Request, Response, queryString, responseLBS)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)


type Params = M.Map Text [Text]

data RequestData = RequestData
    { waiReq :: Request
    , queryParams :: Params
    , postParams  :: Params
    }

data ResponseState = ResponseState
    { resStatus :: Status
    , resHeaders :: ResponseHeaders
    , content :: BL.ByteString
    }

data HandlerResult = Redirect ByteString     -- Redirect to a URL
                   | ResponseComplete        -- Send the response
                   | HandlerError ByteString -- Send an internal error response
                     deriving (Show, Eq)

type Handler a = EitherT HandlerResult (ReaderT RequestData (StateT ResponseState IO)) a

type Router = [Text] -> Handler ()

runHandler :: Request -> Handler () -> IO Response
runHandler req h  = do
    (pParams, _) <- parseRequestBody lbsBackEnd req
    let initRes = ResponseState status200 [] ""
        rd = RequestData
              { waiReq      = req
              , queryParams = toMap $ fmap (\(n, v) -> (n, fromMaybe "" v)) $ queryString req
              , postParams  = toMap pParams
              }

    (result, res) <- runStateT (runReaderT (runEitherT h) rd) initRes
    let hdrs = resHeaders res
    return $ case result of
        Left ResponseComplete   -> responseLBS (resStatus res) hdrs (content res)
        Left (Redirect url)     -> responseLBS status302 ((hLocation, url) : hdrs) ""
        Left (HandlerError msg) -> responseLBS internalServerError500 hdrs (BL.fromStrict msg)
        Right _ -> error "Not handled"

toMap :: [(ByteString, ByteString)] -> Params
toMap = M.unionsWith (++) . map (\(x, y) -> M.singleton (TE.decodeUtf8 x) [TE.decodeUtf8 y])

