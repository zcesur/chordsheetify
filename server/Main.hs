module Main where

import           Data.ByteString                ( ByteString )
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           System.Environment             ( lookupEnv )
import           System.IO
import           Text.Read                      ( readMaybe )
import qualified Data.ByteString.Char8         as B

import           App

type DBConnectionString = ByteString

main :: IO ()
main = do
  dbEnv   <- lookupEnv "DATABASE_URL"
  portEnv <- lookupEnv "PORT"
  let connStr  = fromMaybe defaultConnStr dbEnv
      port     = fromMaybe defaultPort (portEnv >>= readMaybe)
      settings = setPort port $ setBeforeMainLoop
        (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  pool <- initConnectionPool (B.pack connStr)
  runSettings settings =<< mkApp pool

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr) close 2 60 10

defaultPort :: Port
defaultPort = 3000

defaultConnStr :: String
defaultConnStr = "postgresql://chordsheetify:chordsheetify@localhost"
