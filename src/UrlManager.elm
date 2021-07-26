module UrlManager exposing (handleDocId)

import Lamdera exposing (sendToBackend)
import Parser exposing (..)
import Types exposing (FrontendMsg(..), ToBackend(..))
import Url exposing (Url)
import Url.Builder


type DocUrl
    = DocUrl String
    | HomePage String
    | NoDocUrl


handleDocId : Url -> Cmd FrontendMsg
handleDocId url =
    case parseDocUrl url of
        NoDocUrl ->
            Cmd.none

        HomePage _ ->
            Cmd.none

        DocUrl id ->
            sendToBackend (GetDocumentById id)



-- PARSE


getInternalRef : String -> Maybe String
getInternalRef str =
    case run parseInternalRef str of
        Ok str_ ->
            Just str_

        Err _ ->
            Nothing


parseInternalRef : Parser String
parseInternalRef =
    succeed identity
        |. int
        |. symbol "#"
        |= parseRefString


parseRefString : Parser String
parseRefString =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseDocUrl : Url -> DocUrl
parseDocUrl url =
    case run docUrlParser url.path of
        Ok docUrl ->
            docUrl

        Err _ ->
            NoDocUrl


docUrlParser : Parser DocUrl
docUrlParser =
    oneOf [ parseHomePage, docUrlUParser_ ]



--guestDocUrlParser : Parser DocUrl
--guestDocUrlParser =
--    succeed (\k -> DocUrlForGuest k)
--        |. symbol "/g/"
--        |= oneOf [ uuidParser, intParser ]
--identifierParser =
--    oneOf [ uuidParser, intParser ]
--
--docUrlParserForIndex : Parser DocUrl
--docUrlParserForIndex =
--    succeed (\k -> DocUrlForIndex k)
--        |. symbol "/i/"
--        |= oneOf [ uuidParser, intParser ]


docUrlUParser_ : Parser DocUrl
docUrlUParser_ =
    succeed (\k -> DocUrl k)
        |. symbol "/"
        |= oneOf [ uuidParser ]



--intParser : Parser Identifier
--intParser =
--    int |> map N


uuidParser : Parser String
uuidParser =
    succeed identity
        |. symbol "uuid:"
        |= parseUuid



--


parseUuid : Parser String
parseUuid =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c || c == '-')


parseAlphaNum : Parser String
parseAlphaNum =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c)


parseHomePage : Parser DocUrl
parseHomePage =
    succeed HomePage
        |. symbol "/h/"
        |= parseAlphaNum
