module L1.Parser.Loop exposing (nextCursor, parseLoop)

import L1.Library.Console as Console
import L1.Library.ParserTools as ParserTools
import L1.Parser.AST as AST exposing (Element(..), Name(..))
import L1.Parser.Branch as Branch exposing (Operation(..), branch)
import L1.Parser.Config as Config exposing (Configuration, EType(..))
import L1.Parser.Configuration as Configuration
import L1.Parser.Error exposing (Context, Problem)
import L1.Parser.Handle as Handle
import L1.Parser.Print
import L1.Parser.TextCursor as TextCursor exposing (ScannerType(..), TextCursor)
import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser a =
    Parser.Parser Context Problem a


{-| parseLoop scans the source text from right to left, update the TextCursor
on each pass. See module Parser.TextCursor for definitions. The TextCursor
is initialized with source text. When parseLoop concludes, it also carries
the AST of the processed source.
-}
parseLoop : (String -> Element) -> Int -> String -> TextCursor
parseLoop parser generation str =
    let
        result =
            ParserTools.loop (TextCursor.init generation str) (nextCursor parser)
                |> (\tc_ -> { tc_ | complete = List.reverse tc_.complete })

        --_ =
        --    Debug.log (L1.Parser.Print.print result) "-"
    in
    result


{-| nextCursor operates by advancing from one syntactic mark to another, e.g.,
'[' or ']' in the case of language L1. On each move it updates the cursor
with one of four TextCursor functions: `add`, `push`, `pop`, 'push'.

The scanPoint field of the text cursor points the character in source
field that is currently being scanned. As a convenience, tc.remaining
holds the source text from the scanPoint onwards.

The scanPoint must be incremented by at least one unit on each pass so
that parseLoop is guaranteed to terminate. The program terminates
when the scanPoint comes to the end of the source.

-}
nextCursor : (String -> Element) -> TextCursor -> ParserTools.Step TextCursor TextCursor
nextCursor parser cursor =
    if cursor.count > 300 then
        exit parser cursor "EMERGENCY STOP AT COUNT 300"

    else
        let
            --_ =
            --    Debug.log (L1.Parser.Print.print cursor) ""
            textToProcess =
                String.dropLeft cursor.scanPoint cursor.source

            chompedText =
                TextCursor.advance cursor textToProcess

            maybeFirstChar =
                String.uncons textToProcess |> Maybe.map Tuple.first

            maybePrefix =
                Maybe.map ((\c -> ParserTools.prefixWith c textToProcess) >> .content) maybeFirstChar
        in
        case ( maybeFirstChar, maybePrefix, cursor.stack ) of
            ( Nothing, _, [] ) ->
                -- NORMAL LOOP TERMINATION: at end of input (Nothing), stack is empty
                ParserTools.Done { cursor | complete = cursor.parsed ++ cursor.complete, message = "COMM0" }

            ( Nothing, _, _ ) ->
                -- NEED TO RESOLVE ERROR: at end of input (Nothing), stack is NOT empty
                ParserTools.Loop (TextCursor.commit parser { cursor | message = "COMM2", count = cursor.count + 1 })

            ( _, Nothing, _ ) ->
                -- WHAT THE HECK?  MAYBE WE SHOULD JUST BAIL OUT
                ParserTools.Loop (TextCursor.commit parser { cursor | message = "COMM3", count = cursor.count + 1 })

            ( Just firstChar, Just prefixx, _ ) ->
                -- CONTINUE NORMAL PROCESSING
                case branch Configuration.configuration cursor firstChar prefixx of
                    ADD ->
                        add parser cursor chompedText

                    PUSH data ->
                        push cursor data

                    POP ->
                        pop parser prefixx cursor

                    SHORTCIRCUIT ->
                        shortcircuit prefixx cursor

                    COMMIT ->
                        ParserTools.Loop (TextCursor.commit parser { cursor | message = "COMMIT" })


exit parser cursor message =
    ParserTools.Done { cursor | message = message }


add parser cursor chompedText =
    ParserTools.Loop <| TextCursor.add parser chompedText.content { cursor | message = "ADD" }


pop parser prefix cursor =
    ParserTools.Loop <| TextCursor.pop parser prefix { cursor | message = "POP", scannerType = NormalScan }


push : TextCursor -> { prefix : String, isMatch : Bool } -> ParserTools.Step TextCursor TextCursor
push cursor ({ prefix, isMatch } as prefixData) =
    case Config.lookup Configuration.configuration prefix of
        Nothing ->
            ParserTools.Loop <|
                TextCursor.push prefixData
                    (TextCursor.EndMark_ prefix)
                    { cursor | message = "PUSH E" }

        Just expectation ->
            let
                scannerType =
                    -- Set the scanner type
                    if List.member expectation.etype [ CodeType, InlineMathType, QuotedType ] then
                        VerbatimScan (expectation.beginSymbol |> Config.firstChar |> Maybe.withDefault '0')
                        -- TODO: fix this (prefix)

                    else
                        NormalScan
            in
            ParserTools.Loop <|
                TextCursor.push prefixData (TextCursor.Expect_ expectation) { cursor | message = "PUSH", scannerType = scannerType }


shortcircuit prefix cursor =
    if List.member prefix [ "#", "##", "###", "####" ] then
        ParserTools.Done <| Handle.heading2 cursor

    else if prefix == ":" then
        ParserTools.Done <| Handle.item cursor

    else if prefix == "|" then
        ParserTools.Done <| Handle.pipe cursor

    else if prefix == "||" then
        ParserTools.Done <| Handle.doublePipe cursor

    else
        ParserTools.Done cursor


error cursor =
    ParserTools.Done { cursor | message = "Unexpected error: no corresponding expectation" }
