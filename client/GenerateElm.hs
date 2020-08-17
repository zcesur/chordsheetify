{-# LANGUAGE OverloadedStrings #-}

import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , UrlPrefix(Static)
                                                , defElmImports
                                                , defElmOptions
                                                , generateElmModuleWith
                                                , urlPrefix
                                                )
import           Api

main :: IO ()
main = generateElmModuleWith
  (defElmOptions { urlPrefix = Static "https://chordsheetify.herokuapp.com" })
  ["Api"]
  defElmImports
  "client/src"
  [DefineElm (Proxy :: Proxy Sheet), DefineElm (Proxy :: Proxy SheetId)]
  (Proxy :: Proxy RestApi)
