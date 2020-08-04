module Main where

import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )
import           Network.Wai.Handler.Warp
import           System.IO
import           System.Environment             ( lookupEnv )

import           App

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port     = fromMaybe 3000 (portEnv >>= readMaybe)
      settings = setPort port $ setBeforeMainLoop
        (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp
