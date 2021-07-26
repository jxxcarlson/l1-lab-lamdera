module L1.Parser.Check exposing (make, reduceList, reduceList2, reduces)

import Dict exposing (Dict)
import L1.Library.Console as Console
import List.Extra


make : String -> List String
make str =
    String.split "" str


pairDict : Dict String String
pairDict =
    Dict.fromList [ ( "[", "]" ), ( "$", "$" ) ]


beginSymbols =
    [ "[" ]


endSymbols =
    [ "]" ]


symbolValue : String -> Int
symbolValue str =
    if List.member str beginSymbols then
        1

    else if List.member str endSymbols then
        -1

    else
        0


{-|

    > reduces ["[","[","]","[","]","]"] -- "balanced" list
      True

    > reduces ["[","[","]","[","]"] -- "unbalanced"
      False

-}
reduces : List String -> Bool
reduces symbolList =
    case reduceAux (Just (List.filter (\s -> not (List.member s [ "$", "`", "\"" ])) symbolList)) of
        Nothing ->
            False

        Just [] ->
            True

        _ ->
            False


reduceList : List String -> List String
reduceList list =
    case list of
        [] ->
            []

        first :: rest ->
            case List.Extra.unconsLast rest of
                Nothing ->
                    list

                Just ( last, rest2 ) ->
                    if matches first last then
                        reduceList rest2

                    else
                        list


reduceList2 : List String -> List String
reduceList2 list =
    case list of
        [] ->
            []

        first :: rest ->
            case List.Extra.unconsLast rest of
                Nothing ->
                    list

                Just ( last, rest2 ) ->
                    if matches first last then
                        reduceList rest2

                    else
                        list


matches first last =
    if first == "[" && last == "]" then
        True

    else if first == "$" && last == "$" then
        True

    else if first == "`" && last == "`" then
        True

    else if first == "\"" && last == "\"" then
        True

    else
        False


reduceAux : Maybe (List String) -> Maybe (List String)
reduceAux symbolList =
    case symbolList of
        Nothing ->
            Nothing

        Just [] ->
            Just []

        Just symbols ->
            reduceAux (reduceOnce symbols)


reduceOnce : List String -> Maybe (List String)
reduceOnce symbolList =
    let
        levels =
            getLevels symbolList
    in
    case List.Extra.elemIndex 0 levels of
        Nothing ->
            Nothing

        Just i ->
            Just (symbolList |> List.Extra.removeAt i |> List.drop 1)


getLevels : List String -> List Int
getLevels symbols =
    getLevels_ ( 0, symbols, [] ) |> (\( a, b, c ) -> c) |> List.reverse


getLevels_ : ( Int, List String, List Int ) -> ( Int, List String, List Int )
getLevels_ ( k, symbols, indices ) =
    case List.Extra.uncons symbols of
        Nothing ->
            ( k, symbols, indices )

        Just ( first, rest ) ->
            let
                newK =
                    k + symbolValue first
            in
            getLevels_ ( newK, List.drop 1 symbols, newK :: indices )
