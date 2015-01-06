{-# LANGUAGE OverloadedStrings #-}

module YaWai where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Text (Text)
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

type Handler a = ReaderT RequestData (StateT ResponseState IO) a

