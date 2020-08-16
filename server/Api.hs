{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Data.Swagger
import           Database.PostgreSQL.Simple     ( FromRow )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           GHC.Generics
import           Servant
import           Servant.Swagger.UI
import qualified Elm.Derive

type Api = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> BasicApi
type BasicApi = "api" :> SheetApi

-- brittany-disable-next-binding
type SheetApi =
  "sheets" :> Get '[JSON] [Sheet] :<|>
  "sheets" :> Capture "id" Int :> Get '[JSON] Sheet

api :: Proxy Api
api = Proxy

basicApi :: Proxy BasicApi
basicApi = Proxy

newtype SheetId = SheetId Int deriving (Show, Generic)
instance FromField SheetId where
  fromField field mbs = SheetId <$> fromField field mbs

data Sheet = Sheet { id :: SheetId, name :: String, content :: String } deriving (Show, Generic, FromRow)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Sheet
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''SheetId

instance ToParamSchema SheetId
instance ToSchema SheetId
instance ToSchema Sheet
