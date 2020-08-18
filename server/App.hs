{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Pool
import           Data.Swagger
import           Database.PostgreSQL.Simple
import           Network.Wai
import           Network.Wai.Application.Static
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           WaiAppStatic.Types

import           Api

mkApp :: Pool Connection -> IO Application
mkApp conns = return $ serve api $ server conns

server :: Pool Connection -> Server Api
server conns =
  swaggerSchemaUIServer apiSwagger
    :<|> sheetServer conns
    :<|> serveDirectoryWith ss
 where
  ss = ds { ssLookupFile = lookupFile }
  ds = defaultFileServerSettings "client/dist"
  lookupFile ps = ssLookupFile ds $ case ps of
    [] -> [unsafeToPiece "index.html"]
    _  -> ps

sheetServer :: Pool Connection -> Server SheetApi
sheetServer conns = getSheets :<|> getSheetById
 where
  getSheets :: Handler [Sheet]
  getSheets =
    liftIO $ withResource conns $ \conn -> query_ conn "SELECT * FROM sheets"

  getSheetById :: Int -> Handler Sheet
  getSheetById sheetId = do
    sheets <- liftIO $ withResource conns $ \conn ->
      query conn "SELECT * FROM sheets WHERE id = ?" [sheetId]
    case sheets of
      (s : _) -> return s
      []      -> throwError err404

-- brittany-disable-next-binding
apiSwagger :: Swagger
apiSwagger = toSwagger restApi
  & info.title   .~ "Chordsheetify API"
  & info.version .~ "1.0.0"
