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
    """# Welcome to L1

[blue [i An experimental markup language.  Can change at any time!]]

James Carlson (jxxcarlson@gmail.com)

## About the App

As guest, click [b Fetch] to fetch documents.  Guests have access to public documents.  Put [quote :public]
in the search field to fetch all public documents.  Put [quote atom] to search for documents about atoms.  Etc.

[blue [i Create an account to experiment with documents. Just put in a username and password and click [b Sign in].]]

[violet [i To sign out, click the left-most button above: the one with your username.]]




## About L1

[b L1] is an experiment in markup language design and the construction of fault-tolerant parsers for them.  L1 documents are a mixture of ordinary text and elements:

|| codeblock
This is [b bold text.]
And this is [i talic text.]

The elements are the pieces of the form `[f x y ...]`, where `f` is the [i name] of the element and `x`, `y`, etc. are the elements of its [i body].  You can think of the body as a list whose items are either [quote words] or other elements, e.g., `[i italic [b bold-italic]]`, which is rendered as

| indent
[i italic [b bold-italic]]

There are a few other bits of grammar.  First, there is in-line code and in-line math:

|| codeblock
`a[i] = b[i] + 1`
$a^2 + b^2 = c^2$

for code and math.  These render as `a[i] = b[i] + 1` and $a^2 + b^2 = c^2$.  Second, there are [i blocks], .e.g,

|| codeblock
|| mathblock
\\int_0^1 x^n
  =
\\frac{1}{n+1}

which is rendered as

|| mathblock
\\int_0^1 x^n
  =
\\frac{1}{n+1}

There are also blocks for code:


|| codeblock
|| codeblock
import sys
"""
        ++ String.fromChar '\u{00A0}'
        ++ """
capital = float(sys.argv[1])
...

which is rendered as

|| codeblock
import sys
"""
        ++ String.fromChar '\u{00A0}'
        ++ """
capital = float(sys.argv[1])
rate = float(sys.argv[2])/100.0
years = int(sys.argv[3])
"""
        ++ String.fromChar '\u{00A0}'
        ++ """
for i in range(0, years):
  capital = (1 + rate)*capital
  print "%3d %8.0f" % (i, capital)


Blocks must have at least one empy line above and below.  [quote blank] lines inside a block are permitted, but they cannot be [i empty]: put at least one space.  For more information about L1, see the articles [i The L1 Markup Language] and [i Fault-tolerant Parsing].

For more information about L1, see [ilink "this document" "/s/jxxcarlson-the-l1-markup-language-2021-07-27"]


## Export

At the moment there is an imperfect export-to-markdown feature.
This feature will always be [quote lossy], since L1 is more expressive than Markdown.
We plan to have export to LaTeX, and with it export to PDF.

## Using L1

For now, L1 should be used only for experimentation.  It is an experiment itself,
and so will change as I discover mistakes in its design or discover a better design.

## Design Philosophy

Nonetheless, there is one overriding principle of design:
[i the language shoud remain very simple, but with features that make it expressive and powerful.]
Simplicity makes a language easy to learn and to use.  It also makes it easier to develop and maintain.

## The L1 Mascot

[image "width:400" "placement:center" "https://i.pinimg.com/originals/d4/07/a4/d407a45bcf3ade18468ac7ba633244b9.jpg"]


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
