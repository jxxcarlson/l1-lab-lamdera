module Document exposing
    ( Access(..)
    , Document
    , dateResidue
    , deletedMessage
    , empty
    , handleSearchCommand
    , makeHomePage
    , makeSlug
    , search
    , setAccess
    , wordCount
    )

import Time
import User exposing (User)


makeSlug : Time.Posix -> Document -> String
makeSlug t doc =
    doc.username
        ++ "-"
        ++ doc.title
        ++ "-"
        ++ dateResidue t
        |> String.toLower
        |> String.replace " " "-"


dateResidue : Time.Posix -> String
dateResidue t =
    let
        y =
            Time.toYear Time.utc t

        m =
            Time.toMonth Time.utc t |> monthAsInt

        d =
            Time.toDay Time.utc t
    in
    [ String.fromInt y, String.fromInt m |> String.padLeft 2 '0', String.fromInt d |> String.padLeft 2 '0' ] |> String.join "-"


monthAsInt : Time.Month -> Int
monthAsInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


type alias Document =
    { title : String
    , author : String
    , username : String
    , id : String
    , created : Time.Posix
    , modified : Time.Posix
    , tags : List String
    , content : String
    , access : Access
    , slug : Maybe String
    }


type alias Username =
    String


type Access
    = Public
    | Private
    | Shared { canRead : List Username, canWrite : List Username }


empty =
    { title = "Empty Doc"
    , author = "No Name"
    , username = "noname123"
    , id = "1"
    , created = Time.millisToPosix 0
    , modified = Time.millisToPosix 0
    , tags = []
    , content = ""
    , access = Private
    , slug = Nothing
    }


deletedMessage : String -> Document
deletedMessage title =
    { empty
        | title = "Document deleted: " ++ title
        , content = ""
        , id = "sys-doc-deleted"
    }


makeHomePage : String -> Document
makeHomePage username =
    { empty | title = username ++ ": home", id = username, username = username, content = "[title " ++ username ++ ": home]", access = Public }


setAccess access document =
    { document | access = access }


wordCount : Document -> Int
wordCount doc =
    doc.content |> String.words |> List.length


search : Maybe User -> String -> List Document -> List Document
search currentUser key docs =
    case currentUser of
        Nothing ->
            docs

        Just user ->
            if key == "" then
                docs

            else if String.left 1 key == ":" then
                handleSearchCommand user.username (String.dropLeft 1 key) docs

            else
                List.filter (\doc -> String.contains (String.toLower key) (String.toLower doc.title)) docs


handleSearchCommand : String -> String -> List Document -> List Document
handleSearchCommand username key docs =
    if key == "me" then
        List.filter (\doc -> doc.username == username) docs

    else if key == "public" then
        List.filter (\doc -> doc.access == Public) docs

    else if String.left 1 key == "t" then
        let
            key_ =
                String.dropLeft 1 key |> String.trim
        in
        if String.length key_ < 3 then
            docs

        else
            List.filter (\doc -> String.contains key_ doc.content) docs

    else if String.left 1 key == "u" then
        let
            key_ =
                String.dropLeft 1 key |> String.trim
        in
        if String.length key_ < 2 then
            docs

        else
            List.filter (\doc -> String.contains (String.toLower key_) (String.toLower doc.username)) docs

    else if String.left 1 key == "h" then
        let
            key_ =
                String.dropLeft 1 key |> String.trim
        in
        if String.length key_ < 1 then
            List.filter (\doc -> doc.id == doc.username) docs

        else
            List.filter (\doc -> String.contains key_ doc.id) docs

    else
        docs
