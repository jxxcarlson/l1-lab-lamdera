module Data exposing
    ( DataDict
    , DataFile
    , DataId
    , Datum
    , aboutCayatex
    , docsNotFound
    , filter
    , fixUrls
    , foo
    , insertDatum
    , make
    , notSignedIn
    , remove
    , setupUser
    )

import Dict exposing (Dict)
import Document exposing (empty)
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
    """[title Welcome to L1]

[i L1 is an experiment in the design of markup languages and fault-tolerant parsers. ]

[b Guest Access]

::: [blue [i Click on the [b [violet Fetch] ]button above to see more documents.]]

::: As a guest, you can create and edit documents.  However, your documents will not be saved.  You can also edit an existing public document.  Those edits will not be saved either.

[b User Account]

::: To make a user account, first make sure that you are signed out.  If the leftmost button above says [quote guest], click it.  Then you will see a button [b Sign In | Sign Up].  That's the one to click.

::: With a user account, you can create and save documents.  Please note, however, that the  L1 language is currently an experiment and so can change at any time.


[b Fech and Filter]

::: The [b Fetch] button retrieves documents from storage.

::: If you only want to see some documents, put a keyword in the search box. Thus, if you only want to see documents pertaning to L1, put [quote l1] in the search box. Filtering is not case-sensitive. The special keywords [blue :public] and [blue :me] do what they say.

:::  Filter the documents already fetched by putting a (different) keyword in the search box.



[b Comments]

::: You can reach me, Jim Carlson, at jxxcarlson@gmail.com or on the Elm Slack as jxxcarlson.

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
