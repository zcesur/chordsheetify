{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Servant
import qualified Elm.Derive



-- API


type Api =
  "api" :> SheetApi

type SheetApi =
  "sheets" :> Get '[JSON] [Sheet] :<|>
  "sheets" :> Capture "id" String :> Get '[JSON] Sheet

api :: Proxy Api
api = Proxy



-- MODEL


newtype SheetId = SheetId String deriving (Show, Eq)

data Sheet = Sheet { id :: SheetId, name :: String, content :: String } deriving (Eq, Show)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Sheet
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''SheetId
