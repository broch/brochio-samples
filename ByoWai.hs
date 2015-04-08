{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Arrow (second)
import Control.Exception (SomeException, catch)
import Control.Monad.Error (throwError)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (HeaderName, ResponseHeaders, Status, internalServerError500, notFound404, status200, status302, hLocation)
import Network.Wai (Application, Request, Response, queryString, responseLBS, strictRequestBody, pathInfo)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.Wai.Handler.Warp (run)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)


-- App

main :: IO ()
main = run 3000 (routerToApplication myAppRouter)

myAppRouter :: Router
myAppRouter path = case path of
  []          -> redirect "/home" -- root
  ["home"]    -> text "Hello, this is the home page!"
  ["eek"]     -> error "eeek!"
  ["user", u] -> html (userPage u)
  _           -> status notFound404 >> text "Not Found"


userPage :: Text -> H.Html
userPage u = H.docTypeHtml $ H.html $ do
    H.head $
        H.title "User"
    H.body $ do
        H.h2 "User Information"
        H.p . H.toHtml $ T.concat ["Hello ", u]

-- "Framework" Stuff

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

routerToApplication :: Router -> Application
routerToApplication route req respond = do
  response <- runHandler req (route $ pathInfo req)
      `catch` \(_ :: SomeException) -> return $ responseLBS internalServerError500 [] "Internal error"
  respond response

runHandler :: Request -> Handler () -> IO Response
runHandler req h  = do
    (pParams, _) <- parseRequestBody lbsBackEnd req
    let initRes = ResponseState status200 [] ""
        rd = RequestData
              { waiReq      = req
              , queryParams = toMap $ fmap (second $ fromMaybe "") (queryString req)
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

-- Read request data
postParam :: Text -> Handler Text
postParam name = asks postParams >>= lookupParam name

queryParam :: Text -> Handler Text
queryParam name = asks queryParams >>= lookupParam name

lookupParam :: Text -> Params -> Handler Text
lookupParam name params = case M.lookup name params of
    Just [v] -> return v
    _        -> throwError $ HandlerError $ B.concat ["Missing or duplicate parameter", TE.encodeUtf8 name]

body :: Handler BL.ByteString
body = asks waiReq >>= liftIO . strictRequestBody

-- Reponse writing
redirect :: ByteString -> Handler a
redirect = throwError . Redirect

status :: Status -> Handler ()
status s = modify $ \rs -> rs { resStatus = s }

text :: Text -> Handler ()
text t = setContentType "text/plain; charset=utf-8" >> (rawBytes . BL.fromStrict $ TE.encodeUtf8 t)

json :: ToJSON a => a -> Handler ()
json j = setContentType "application/json" >> rawBytes (encode j)

html :: H.Html -> Handler ()
html h = setContentType "text/html; charset=utf-8" >> rawBytes (renderHtml h)

rawBytes :: BL.ByteString -> Handler ()
rawBytes b = modify (\rs -> rs { content = b }) >> throwError ResponseComplete

setHeader :: HeaderName -> ByteString -> Handler ()
setHeader name value = modify $ \rs -> rs { resHeaders = (name, value) : resHeaders rs }

setContentType :: ByteString -> Handler ()
setContentType = setHeader "Content-Type"
