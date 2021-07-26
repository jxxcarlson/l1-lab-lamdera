module Document exposing
    ( Access(..)
    , Document
    , deletedMessage
    , empty
    , handleSearchCommand
    , makeHomePage
    , search
    , setAccess
    , wordCount
    )

import Time
import User exposing (User)


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
