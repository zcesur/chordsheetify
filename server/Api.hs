{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Data.Swagger
import           GHC.Generics
import           Servant
import           Servant.Swagger.UI
import qualified Elm.Derive

type Api = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> BasicApi
type BasicApi = "api" :> SheetApi

-- brittany-disable-next-binding
type SheetApi =
  "sheets" :> Get '[JSON] [Sheet] :<|>
  "sheets" :> Capture "id" String :> Get '[JSON] Sheet

api :: Proxy Api
api = Proxy

basicApi :: Proxy BasicApi
basicApi = Proxy

newtype SheetId = SheetId String deriving (Show, Generic)
data Sheet = Sheet { id :: SheetId, name :: String, content :: String } deriving (Show, Generic)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Sheet
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''SheetId

instance ToParamSchema SheetId
instance ToSchema SheetId
instance ToSchema Sheet
