module L1.Parser.TextCursor exposing
    ( TextCursor, init
    , ProtoStackItem(..), ScannerType(..), add, advance, advanceNormal, commit, pop, push
    )

{-| TextCursor is the data structure used by L1.Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import L1.Library.Console as Console
import L1.Library.ParserTools as ParserTools
import L1.Library.Utility as Utility exposing (debug, debug2)
import L1.Parser.AST as AST exposing (Element(..), Name(..))
import L1.Parser.Config as Config exposing (Configuration, EType(..), Expectation)
import L1.Parser.Configuration as Configuration
import L1.Parser.MetaData as MetaData exposing (MetaData)
import L1.Parser.Stack as Stack exposing (StackItem(..))
import List.Extra
import Parser.Advanced


{-| TODO: give an account of what these fields do
-}
type alias TextCursor =
    { count : Int
    , generation : Int

    ---
    , verbatimPrefix : Maybe String
    , scannerType : ScannerType
    , scanPoint : Int
    , sourceLength : Int

    ---
    , source : String
    , parsed : List Element -- might be incorporated later
    , complete : List Element -- no changes will be made
    , stack : List StackItem -- a stack of unclosed elements

    ---
    , message : String
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


type ProtoStackItem
    = Expect_ Expectation
    | EndMark_ String


{-| initialize with source text
-}
init : Int -> String -> TextCursor
init generation source =
    { count = 0
    , generation = generation

    --
    , scanPoint = 0
    , sourceLength = String.length source
    , scannerType = NormalScan
    , verbatimPrefix = Nothing

    --
    , source = source
    , parsed = []
    , complete = []
    , stack = []

    --
    , message = "START"
    }


advance : TextCursor -> String -> ParserTools.StringData
advance cursor textToProcess =
    case cursor.scannerType of
        NormalScan ->
            advanceNormal Configuration.configuration
                cursor.scanPoint
                textToProcess

        VerbatimScan c ->
            advanceVerbatim c textToProcess


{-| Return the longest prefix of str that does not contain a delimiter.
The delimiter sets used depend upon position. One set for position = 0,
another for position /= 0.
-}
advanceNormal : Configuration -> Int -> String -> ParserTools.StringData
advanceNormal config position str =
    let
        delimiterTypes =
            if List.member (String.slice 0 1 str) [ "|", ":" ] then
                Config.AllDelimiters

            else
                Config.InteriorDelimiters
    in
    case Parser.Advanced.run (ParserTools.text (Config.notDelimiter Configuration.configuration delimiterTypes) (Config.notDelimiter Configuration.configuration delimiterTypes)) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


{-| Advance, but according to different criteria, because the
scanner type has been set to 'VerbatimScan c'
-}
advanceVerbatim : Char -> String -> ParserTools.StringData
advanceVerbatim verbatimChar str =
    let
        predicate =
            \c -> c /= verbatimChar

        -- && c /= ']'
    in
    case Parser.Advanced.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


add : (String -> Element) -> String -> TextCursor -> TextCursor
add parse_ str tc =
    if tc.stack == [] then
        { tc
            | count = tc.count + 1
            , scanPoint = tc.scanPoint + String.length str
            , complete = parse_ str :: tc.parsed ++ tc.complete
            , parsed = []
        }

    else
        { tc
            | count = tc.count + 1
            , stack = TextItem { content = str, position = { start = 0, end = String.length str } } :: tc.stack
            , scanPoint = tc.scanPoint + String.length str
            , parsed = []
        }


{-| A
-}
push : { prefix : String, isMatch : Bool } -> ProtoStackItem -> TextCursor -> TextCursor
push ({ prefix, isMatch } as prefixData) proto tc =
    let
        newText : ParserTools.StringData
        newText =
            if isMatch then
                -- TODO: think about scanPoint
                { start = 0, finish = 0, content = "" }

            else
                advance tc (String.dropLeft (tc.scanPoint + String.length prefix) tc.source)

        newContent =
            newText.content

        scanPointIncrement =
            case proto of
                Expect_ _ ->
                    String.length prefix + newText.finish - newText.start

                EndMark_ _ ->
                    if newContent == "" then
                        String.length prefix

                    else
                        String.length prefix + newText.finish - newText.start

        ( verbatimPrefix, scannerType ) =
            case ( Config.isVerbatimSymbol prefix, Just prefix == tc.verbatimPrefix ) of
                -- TURN ON VERBATIM SCANNING: Fresh verbatim prefix
                ( True, False ) ->
                    case String.uncons prefix of
                        Nothing ->
                            ( Nothing, NormalScan )

                        Just ( ch, _ ) ->
                            ( Just prefix, VerbatimScan ch )

                -- TURN OFF VERBATIM SCANNING: second occurrence of a verbatim prefix
                ( True, True ) ->
                    ( Nothing, NormalScan )

                _ ->
                    ( tc.verbatimPrefix, tc.scannerType )

        newStack =
            case proto of
                Expect_ expectation ->
                    Stack.Expect
                        { expect = expectation
                        , content = newContent
                        , count = tc.count
                        , scanPoint = tc.scanPoint + scanPointIncrement
                        , position = { start = tc.scanPoint, end = tc.scanPoint + scanPointIncrement }
                        }
                        :: tc.stack

                EndMark_ prefix_ ->
                    if newContent == "" then
                        -- TODO: need real position
                        Stack.EndMark { content = prefix_, position = { start = tc.scanPoint, end = tc.scanPoint + scanPointIncrement } } :: tc.stack

                    else
                        Stack.TextItem { content = newContent, position = { start = tc.scanPoint + 1, end = tc.scanPoint + String.length newContent } } :: Stack.EndMark { content = prefix_, position = { start = tc.scanPoint, end = tc.scanPoint + 1 } } :: tc.stack
    in
    { tc
        | count = tc.count + 1
        , verbatimPrefix = verbatimPrefix
        , scannerType = scannerType
        , scanPoint = tc.scanPoint + scanPointIncrement
        , stack = newStack
    }


pop : (String -> Element) -> String -> TextCursor -> TextCursor
pop parse prefix cursor =
    -- The cursors' scanPoint is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.Parser.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1, scannerType = NormalScan }

        Just stackTop_ ->
            let
                data =
                    Stack.showStack (List.reverse cursor.stack) ++ prefix
            in
            { cursor
                | stack = []
                , parsed = parse data :: cursor.parsed
                , count = cursor.count + 1
                , scanPoint = cursor.scanPoint + 1
            }


commit : (String -> Element) -> TextCursor -> TextCursor
commit parse tc =
    if tc.stack == [] && tc.scanPoint >= tc.sourceLength then
        finishUp tc

    else if Stack.isStrictlyReducible tc.stack && tc.scanPoint >= tc.sourceLength then
        finishUpWithReducibleStack parse tc

    else
        -- CAN NOW ASSUME THAT THE STACK IS NOT REDUCIBLE
        resolveError tc


finishUp : TextCursor -> TextCursor
finishUp tc =
    { tc | parsed = [] }


finishUpWithReducibleStack parse tc =
    let
        stackData =
            tc.stack |> List.reverse |> List.map Stack.show |> String.join ""
    in
    { tc | complete = parse stackData :: tc.complete }



-- LANGUAGE HANDLERS


resolveError : TextCursor -> TextCursor
resolveError tc =
    let
        scanPointString =
            Utility.characterAt tc.scanPoint tc.source

        ( errorPosition, badStackItemSymbol ) =
            if scanPointString == "]" then
                ( tc.scanPoint, scanPointString )

            else
                let
                    maybeBottomOfStack =
                        List.Extra.unconsLast tc.stack
                            |> Maybe.map Tuple.first

                    errorPosition_ =
                        maybeBottomOfStack
                            |> Maybe.map Stack.startPosition
                            -- TODO: DANGER! THE BELOW IS A REALLY BAD IDEA
                            |> Maybe.withDefault 0

                    badStackItemSymbol_ =
                        maybeBottomOfStack
                            |> Maybe.map Stack.beginSymbol
                            |> Maybe.withDefault "??"
                in
                ( errorPosition_, badStackItemSymbol_ )

        -- TODO: the above is hacky and DANGEROUS
        errorElement =
            Element (Name "error") (Text (" unmatched '" ++ badStackItemSymbol ++ "'") MetaData.dummy) MetaData.dummy
    in
    { tc
        | count = 1 + tc.count
        , stack = []
        , parsed = []
        , scanPoint = errorPosition + String.length badStackItemSymbol
        , scannerType = NormalScan
        , complete = tc.parsed ++ errorElement :: tc.complete
    }
