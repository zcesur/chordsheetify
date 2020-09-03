{-# LANGUAGE OverloadedStrings #-}

import           Api
import           Servant.Elm (DefineElm (DefineElm), Proxy (Proxy),
                              UrlPrefix (Static), defElmImports, defElmOptions,
                              generateElmModuleWith)

main :: IO ()
main = generateElmModuleWith
  defElmOptions
  ["Api"]
  defElmImports
  "client/src"
  [DefineElm (Proxy :: Proxy Sheet), DefineElm (Proxy :: Proxy SheetId)]
  (Proxy :: Proxy RestApi)
