{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           Data.Maybe (fromMaybe)
import           Text.Read (readMaybe)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           System.Environment (lookupEnv)



-- API


type SheetApi =
  "sheet" :> Get '[JSON] [Sheet] :<|>
  "sheet" :> Capture "sheetId" Integer :> Get '[JSON] Sheet

sheetApi :: Proxy SheetApi
sheetApi = Proxy



-- SERVER


run :: IO ()
run = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe 3000 (portEnv >>= readMaybe)
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve sheetApi server

server :: Server SheetApi
server =
  getSheets :<|>
  getSheetById

getSheets :: Handler [Sheet]
getSheets = return [exampleSheet]

getSheetById :: Integer -> Handler Sheet
getSheetById = \ case
  0 -> return exampleSheet
  _ -> throwError err404



-- MODEL


data Sheet = Sheet { id :: Integer, content :: String } deriving (Eq, Show, Generic)

instance ToJSON Sheet
instance FromJSON Sheet

exampleSheet :: Sheet
exampleSheet = Sheet 0 $ unlines
  [ "[G]               [Gsus2]  [G]             [G]     [Gsus2]  [G]   [C]    [C]    [C]    [C]"
  , "Such is the way of     the world, You can ne  -   ver know"
  , ""
  , "[G]                 [Gsus2]  [G]              [G]   [Gsus2]  [G]   [C]     [C]    [C]    [C]"
  , "Just where to put all    your faith And how will   it  grow"
  , ""
  , "      [D]         [G]                           [C]        [Cadd9]  [C]  [Cadd9]"
  , "Gonna rise up, Bringing back holes in dark memories"
  , ""
  , "      [D]         [G]                [C]        [C]    [C]   [C6]"
  , "Gonna rise up, Turning mistakes into gold"
  ]
