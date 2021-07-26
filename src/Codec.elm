module Codec exposing (..)

import Json.Encode as E


encodeForPDF : String -> String -> String -> List String -> E.Value
encodeForPDF id title content urlList =
    E.object
        [ ( "id", E.string id )
        , ( "content", E.string content )
        , ( "urlList", E.list E.string urlList )
        ]
