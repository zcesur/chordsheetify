{-# LANGUAGE OverloadedStrings #-}

import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , UrlPrefix(Static)
                                                , defElmImports
                                                , defElmOptions
                                                , generateElmModuleWith
                                                )
import           Api

main :: IO ()
main = generateElmModuleWith
  defElmOptions
  ["Api"]
  defElmImports
  "client/src"
  [DefineElm (Proxy :: Proxy Sheet), DefineElm (Proxy :: Proxy SheetId)]
  (Proxy :: Proxy RestApi)
