{-# LANGUAGE OverloadedStrings #-}

module YaWai where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either (EitherT)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (ResponseHeaders, Status)
import Network.Wai (Request)


type Params = Map Text [Text]

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

