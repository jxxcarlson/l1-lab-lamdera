module Data exposing (
     aboutCayatex
    , docsNotFound, foo, notSignedIn, DataDict
    , DataFile
    , DataId
    , Datum
    , filter
    , fixUrls
    , insertDatum
    , make
    , remove
    , setupUser
    )

import Document exposing (empty)
import Dict exposing(Dict)
import Time

notSignedIn =
    { empty
        | title = "Not signed in"
        , author = "System"
        , username = "system"
        , content = notSigneInText
        , id = "sys0002"
    }


notSigneInText =
    """

[title Welcome!]

[blue [i Please sign in to create or work with your documents]]

[violet [i To sign out, click the left-most button above: the one with your username.]]

[fontRGB |100, 100, 100|  [b Note.] Export to LaTeX and create PDF features have been added.  This is a bleeding edge experimental feature which needs quite a bit of attention before it can be used for real work. Please do bear with us!]

[fontRGB |100, 100, 100| As an example, this document can be successfully converted to PDF, but the image apppears on the next page.]

[image |width: 400, placement: center|https://i.pinimg.com/originals/d4/07/a4/d407a45bcf3ade18468ac7ba633244b9.jpg]

"""


docsNotFound =
    { empty
        | title = "Oops!"
        , author = "System"
        , username = "system"
        , content = docsNotFoundText
        , id = "sys0001"
    }


docsNotFoundText =
    """
[title Oops!]

[i  Sorry, could not find your documents]

[i To create a document, press the [b New] button above, on left.]
"""


aboutCayatex =
    { empty
        | title = "Announcing CaYaTeX"
        , author = "James Carlson"
        , username = "jxxcarlson"
        , content = "Whatever!"
        , id = "aboutCYT"
    }


foo =
    { empty
        | title = "Foo"
        , author = "James Carlson"
        , username = "jxxcarlson"
        , content = "This is a test"
        , id = "foo222"
    }

----XXXX----



type alias Username =
    String


type alias DataId =
    String


type alias Datum =
    { id : String
    , title : String
    , username : Username
    , content : String
    , tags : List String
    , creationData : Time.Posix
    , modificationData : Time.Posix
    }


make : Username -> Time.Posix -> String -> String -> Datum
make username currentTime id content =
    { id = id
    , title = content |> String.lines |> List.head |> Maybe.withDefault "TITLE"
    , username = username
    , content = fixUrls content
    , tags = []
    , creationData = currentTime
    , modificationData = currentTime
    }


type alias DataFile =
    { data : List Datum
    , username : Username
    , creationData : Time.Posix
    , modificationData : Time.Posix
    }


type alias DataDict =
    Dict Username DataFile


filter : String -> List Datum -> List Datum
filter filterString data =
    let
        filterString_ =
            String.toLower filterString
    in
    List.filter (\datum -> String.contains filterString_ (String.toLower datum.content)) data


setupUser : Time.Posix -> Username -> DataDict -> DataDict
setupUser currentTime username dataDict =
    let
        newDataFile =
            { data = []
            , username = username
            , creationData = currentTime
            , modificationData = currentTime
            }
    in
    Dict.insert username newDataFile dataDict


insertDatum : Username -> Datum -> DataDict -> DataDict
insertDatum username datum dataDict =
    case Dict.get username dataDict of
        Nothing ->
            dataDict

        Just dataFile ->
            Dict.insert username { dataFile | data = datum :: dataFile.data } dataDict


remove : Username -> DataId -> DataDict -> DataDict
remove username id dataDict =
    case Dict.get username dataDict of
        Nothing ->
            dataDict

        Just dataFile ->
            let
                newData =
                    List.filter (\datum -> datum.id /= id) dataFile.data

                newDataFile =
                    { dataFile | data = newData }
            in
            Dict.insert username newDataFile dataDict


getUrls : String -> List String
getUrls str =
    str |> String.words |> List.filter isUrl


getLinkLabel : String -> String
getLinkLabel str =
    if String.left 7 str == "http://" then
        String.replace "http://" "" str

    else
        String.replace "https://" "" str


fixUrl : String -> String -> String
fixUrl url str =
    let
        label =
            getLinkLabel url

        link =
            " [" ++ label ++ "](" ++ url ++ ")"
    in
     String.replace url link str


fixUrls : String -> String
fixUrls str =
    let
        urls =
            getUrls str

        fixers =
            List.map fixUrl urls
    in
    List.foldl (\fixer str_ -> fixer str_) str fixers


isUrl : String -> Bool
isUrl str =
    String.left 4 str == "http"
