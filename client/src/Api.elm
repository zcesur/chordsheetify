module Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Sheet  =
   { id: SheetId
   , name: String
   , content: String
   }

jsonDecSheet : Json.Decode.Decoder ( Sheet )
jsonDecSheet =
   Json.Decode.succeed (\pid pname pcontent -> {id = pid, name = pname, content = pcontent})
   |> required "id" (jsonDecSheetId)
   |> required "name" (Json.Decode.string)
   |> required "content" (Json.Decode.string)

jsonEncSheet : Sheet -> Value
jsonEncSheet  val =
   Json.Encode.object
   [ ("id", jsonEncSheetId val.id)
   , ("name", Json.Encode.string val.name)
   , ("content", Json.Encode.string val.content)
   ]



type alias SheetId  = Int

jsonDecSheetId : Json.Decode.Decoder ( SheetId )
jsonDecSheetId =
    Json.Decode.int

jsonEncSheetId : SheetId -> Value
jsonEncSheetId  val = Json.Encode.int val


getApiSheets : (Result Http.Error  ((List Sheet))  -> msg) -> Cmd msg
getApiSheets toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "https://chordsheetify.herokuapp.com"
                    [ "api"
                    , "sheets"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecSheet))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiSheetsById : Int -> (Result Http.Error  (Sheet)  -> msg) -> Cmd msg
getApiSheetsById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "https://chordsheetify.herokuapp.com"
                    [ "api"
                    , "sheets"
                    , (capture_id |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecSheet
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
