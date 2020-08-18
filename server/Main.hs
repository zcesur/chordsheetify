module Main where

import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )
import qualified Data.ByteString.Char8         as B

import           App

type DBConnectionString = ByteString

main :: IO ()
main = do
  port    <- fromMaybe defaultPort . (readMaybe =<<) <$> lookupEnv "PORT"
  connStr <- B.pack . fromMaybe defaultConnStr <$> lookupEnv "DATABASE_URL"
  app     <- mkApp <$> initConnectionPool connStr
  runSettings (setPort port defaultSettings) app


initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr) close 2 60 10

defaultPort :: Port
defaultPort = 3000

defaultConnStr :: String
defaultConnStr = "postgresql://chordsheetify:chordsheetify@localhost"
