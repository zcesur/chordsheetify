{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module App where

import           Control.Lens
import           Data.Swagger
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

import           Api

mkApp :: IO Application
mkApp = return $ simpleCors $ serve api server

-- brittany-disable-next-binding
apiSwagger :: Swagger
apiSwagger = toSwagger basicApi
  & info.title   .~ "Chordsheetify API"
  & info.version .~ "1.0.0"
  & host         ?~ "chordsheetify.herokuapp.com"

server :: Server Api
server = swaggerSchemaUIServer apiSwagger :<|> getSheets :<|> getSheetById

getSheets :: Handler [Sheet]
getSheets = return [rise]

getSheetById :: String -> Handler Sheet
getSheetById = \case
  "37635f03-bc33-432f-9514-ea44c89148e5" -> return rise
  _ -> throwError err404

rise :: Sheet
rise = Sheet
  (SheetId "37635f03-bc33-432f-9514-ea44c89148e5")
  "Eddie Vedder - Rise"
  "[G]               [Gsus2]  [G]             [G]     [Gsus2]  [G]   [C]    [C]    [C]    [C]\nSuch is the way of     the world, You can ne  -   ver know\n\n[G]                 [Gsus2]  [G]              [G]   [Gsus2]  [G]   [C]     [C]    [C]    [C]\nJust where to put all    your faith And how will   it  grow\n\n      [D]         [G]                           [C]        [Cadd9]  [C]  [Cadd9]\nGonna rise up, Bringing back holes in dark memories\n\n      [D]         [G]                [C]        [C]    [C]   [C6]\nGonna rise up, Turning mistakes into gold\n"
