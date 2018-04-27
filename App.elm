module App exposing (main)

{-| In this series, we’ll look at how so-called “applicative parsers” work. In order to understand something, there’s nothing like building it for yourself, and so we’ll create a basic parser library from scratch, and then some useful “parser combinators”, and then finish off by building a complete JSON parser.

Now terms like “applicative parsers” and “parser combinators” can make this approach seem complicated, but rather than attempting to explain these concepts up front, we’ll just dive in and start coding.

We’ll build up to the complex stuff incrementally via a series of implementations, where each implementation is only slightly different from the previous one. By using this approach, I hope that at each stage the design and concepts will be easy to understand, and so by the end of this series, parser combinators will have become completely demystified.

There will be four posts in this series:

In this, the first post, we’ll look at the basic concepts of parser combinators and build the core of the library.
In the second post, we’ll build up a useful library of combinators.
In the third post, we’ll work on providing helpful error messages.
In the last post, we’ll build a JSON parser using this parser library.


# Basics

@doc pchar, andThen, orElse, choice, anyOf

-}

import Html exposing (Html)


main : Program Never number msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Implementation 1. Parsing a hard-coded character
For our first implementation, let’s create something that just parses a single, hard-coded, character, in this case, the letter “A”. You can’t get much simpler than that!

Here is how it works:

The input to a parser is a stream of characters. We could use something complicated, but for now we’ll just use a string.
If the stream is empty, then return a pair consisting of false and an empty string.
If the first character in the stream is an A, then return a pair consisting of true and the remaining stream of characters.
If the first character in the stream is not an A, then return false and the (unchanged) original stream of characters.
Here’s the code:

-}
pcharA : String -> ( Bool, String )
pcharA input =
    -- check for the character A in the input
    case String.uncons input of
        Just ( 'A', remaining ) ->
            ( True, remaining )

        _ ->
            ( False, input )


type ParserResult a
    = Success a
    | Failure String


type Parser a
    = Parser (String -> ParserResult a)


{-| Implementation 2. Parsing a specified character
Let’s refactor so that we can pass in the character we want to match, rather than having it be hard coded.

And this time, rather than returning true or false, we’ll return a message indicating what happened.

We’ll call the function pchar for “parse char”. Here’s the code:

-}
pchar : Char -> Parser ( Char, String )
pchar charToMatch =
    let
        -- inner function
        innerFn : String -> ParserResult ( Char, String )
        innerFn input =
            case String.uncons input of
                Just ( charFound, remaining ) ->
                    case charFound == charToMatch of
                        True ->
                            Success ( charToMatch, remaining )

                        False ->
                            Failure <| "Expecting " ++ String.fromChar charToMatch ++ ". Got " ++ String.fromChar charFound

                Nothing ->
                    Failure "No more input"
    in
        Parser innerFn


{-| This is the documentation for andThen
-}
andThen : Parser ( a, String ) -> Parser ( b, c ) -> Parser ( ( a, b ), c )
andThen parser1 parser2 =
    let
        innerFn : String -> ParserResult ( ( a, b ), c )
        innerFn input =
            let
                result1 =
                    run parser1 input
            in
                case result1 of
                    Failure err ->
                        Failure err

                    Success ( value1, remaining1 ) ->
                        let
                            result2 =
                                run parser2 remaining1
                        in
                            case result2 of
                                Failure err ->
                                    Failure err

                                Success ( value2, remaining2 ) ->
                                    let
                                        combinedValue =
                                            ( value1, value2 )
                                    in
                                        Success ( combinedValue, remaining2 )
    in
        Parser innerFn


{-| This is the documentation for infix .>>. operator for andThen
-}
(.>>.) : Parser ( a, String ) -> Parser ( b, c ) -> Parser ( ( a, b ), c )
(.>>.) =
    andThen


{-| This is the documentation for orElse
-}
orElse : Parser a -> Parser a -> Parser a
orElse parser1 parser2 =
    let
        innerFn : String -> ParserResult a
        innerFn input =
            let
                -- run parser1 with the input
                result1 =
                    run parser1 input
            in
                -- test the result for Success/Failure
                case result1 of
                    Success result ->
                        -- successful, therefore return the original result
                        Success result

                    Failure _ ->
                        -- failure, run parser2 and return its result
                        run parser2 input
    in
        -- return the inner function
        Parser innerFn


{-| This is the documentation for the infix operator <|>, for orElse
-}
(<|>) : Parser a -> Parser a -> Parser a
(<|>) =
    orElse


choice : List (Parser ( Char, String )) -> Parser ( Char, String )
choice listOfParsers =
    let
        innerFn _ =
            Failure "At least one parser must be defined"

        parserFail =
            Parser innerFn

        firstParser =
            case List.head listOfParsers of
                Just firstInList ->
                    firstInList

                Nothing ->
                    parserFail

        restOfParsers =
            case List.tail listOfParsers of
                Just restOfTheList ->
                    restOfTheList

                Nothing ->
                    listOfParsers
    in
        List.foldl orElse firstParser restOfParsers


anyOf : List Char -> Parser ( Char, String )
anyOf listOfChars =
    listOfChars
        |> List.map pchar
        |> choice


mapP : (a -> b) -> Parser ( a, c ) -> Parser ( b, c )
mapP f parser =
    let
        innerFn : String -> ParserResult ( b, c )
        innerFn input =
            let
                result =
                    run parser input
            in
                case result of
                    Success ( value, remaining ) ->
                        let
                            newValue =
                                f value
                        in
                            Success ( newValue, remaining )

                    Failure err ->
                        Failure err
    in
        Parser innerFn


(<!>) : (a -> b) -> Parser ( a, c ) -> Parser ( b, c )
(<!>) =
    mapP


(|>>) : Parser ( a, b ) -> (a -> c) -> Parser ( c, b )
(|>>) x f =
    mapP f x


{-| This is the documentation for returnP
-}
returnP : a -> Parser ( a, String )
returnP x =
    let
        innerFn : String -> ParserResult ( a, String )
        innerFn input =
            Success ( x, input )
    in
        Parser innerFn


{-| This is the documentation for applyP
-}
applyP : Parser ( a -> b, String ) -> Parser ( a, c ) -> Parser ( b, c )
applyP fp xP =
    -- create a Parser containing a pair (f, x)
    (fp .>>. xP)
        -- map the pair by applying f to x
        |> mapP (\( f, x ) -> f x)


{-| This is the documentation for infix <*> for applyP
-}
(<*>) : Parser ( a -> b, String ) -> Parser ( a, c ) -> Parser ( b, c )
(<*>) =
    applyP


lift2 : (a -> a1 -> b) -> Parser ( a, String ) -> Parser ( a1, c ) -> Parser ( b, c )
lift2 f xP yP =
    returnP f <*> xP <*> yP


addP : Parser ( number, String ) -> Parser ( number, c ) -> Parser ( number, c )
addP =
    lift2 (+)


startsWithP : Parser ( String, String ) -> Parser ( String, c ) -> Parser ( Bool, c )
startsWithP =
    lift2 String.startsWith


sequence : List (Parser ( a, String )) -> Parser ( List a, String )
sequence parserList =
    let
        cons head tail =
            head :: tail

        consP =
            lift2 cons
    in
        case parserList of
            [] ->
                returnP []

            head :: tail ->
                consP head (sequence tail)


pstring : String -> Parser ( String, String )
pstring str =
    str
        |> String.toList
        |> List.map pchar
        |> sequence
        |> mapP String.fromList


run : Parser a -> String -> ParserResult a
run parser input =
    let
        (Parser innerFn) =
            parser
    in
        innerFn input


init : ( number, Cmd msg )
init =
    ( 0, Cmd.none )


view : a -> Html msg
view model =
    let
        simpleParseA =
            pcharA "ABCDEFGH"

        resultSimpleA =
            toString <| simpleParseA

        parseA : Parser ( Char, String )
        parseA =
            pchar 'A'

        input =
            "ABCDEFGH"

        resultA : String
        resultA =
            toString <| run parseA input

        parseB =
            pchar 'B'

        parseAThenB =
            parseA .>>. parseB

        {- andThen parseA parseB -}
        resultAThenB =
            toString <| run parseAThenB input

        parseAOrElseB =
            parseA <|> parseB

        {- orElse parseA parseB -}
        resultAOrElseB_1 =
            toString <| run parseAOrElseB "AZZ"

        resultAOrElseB_2 =
            toString <| run parseAOrElseB "BZZ"

        resultAOrElseB_3 =
            toString <| run parseAOrElseB "CZZ"

        parseC =
            pchar 'C'

        bOrElseC =
            orElse parseB parseC

        aAndThenBorC =
            andThen parseA bOrElseC

        resultaAndThenBorC_1 =
            toString <| run aAndThenBorC "ABZ"

        resultaAndThenBorC_2 =
            toString <| run aAndThenBorC "ACZ"

        resultaAndThenBorC_3 =
            toString <| run aAndThenBorC "QBZ"

        resultaAndThenBorC_4 =
            toString <| run aAndThenBorC "AQZ"

        parseLowercase =
            anyOf [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm' ]

        resultParseLowercase =
            toString <| run parseLowercase "mbcdefghijkM"

        parseDigit =
            anyOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

        resultParseDigit =
            toString <| run parseDigit "1ABC"

        parseThreeDigitsAsStr =
            let
                -- create a parser that returns a tuple
                tupleParser =
                    parseDigit .>>. parseDigit .>>. parseDigit

                -- create a function that turns the tuple into a string
                transformTuple ( ( c1, c2 ), c3 ) =
                    String.fromChar c1 ++ String.fromChar c2 ++ String.fromChar c3
            in
                -- use "map" to combine them
                mapP transformTuple tupleParser

        {-
           Compact version of parseThreeDigitsAsStr

           parseThreeDigitsAsStr =
               ( parseDigit .>>. parseDigit .>>. parseDigit)
                   |>> \( ( c1, c2 ), c3) -> String.fromChar c1 ++ String.fromChar c2 ++ String.fromChar c3
        -}
        resultParseThreeDigitsAsStr =
            toString <| run parseThreeDigitsAsStr "123A"

        parseThreeDigitsAsInt : Parser ( Result String Int, String )
        parseThreeDigitsAsInt =
            mapP String.toInt parseThreeDigitsAsStr

        resultParseThreeDigitsAsInt =
            toString <| run parseThreeDigitsAsInt "123A"

        parsers =
            [ pchar 'A', pchar 'B', pchar 'C' ]

        combined =
            sequence parsers

        resultCombined =
            toString <| run combined "ABCD"

        parseABC =
            pstring "ABC"

        resultParseABC1 =
            toString <| run parseABC "ABCDE"

        resultParseABC2 =
            toString <| run parseABC "A|CDE"

        resultParseABC3 =
            toString <| run parseABC "AB|DE"
    in
        Html.div []
            [ Html.p [] [ Html.text "Hello, World! " ]
            , Html.p [] [ Html.text resultSimpleA ]
            , Html.p [] [ Html.text resultA ]
            , Html.p [] [ Html.text resultAThenB ]
            , Html.p [] [ Html.text resultAOrElseB_1 ]
            , Html.p [] [ Html.text resultAOrElseB_2 ]
            , Html.p [] [ Html.text resultAOrElseB_3 ]
            , Html.p [] [ Html.text resultaAndThenBorC_1 ]
            , Html.p [] [ Html.text resultaAndThenBorC_2 ]
            , Html.p [] [ Html.text resultaAndThenBorC_3 ]
            , Html.p [] [ Html.text resultaAndThenBorC_4 ]
            , Html.p [] [ Html.text resultParseLowercase ]
            , Html.p [] [ Html.text resultParseDigit ]
            , Html.p [] [ Html.text resultParseThreeDigitsAsStr ]
            , Html.p [] [ Html.text resultParseThreeDigitsAsInt ]
            , Html.p [] [ Html.text "run combined \"ABCD\"", Html.text resultCombined ]
            , Html.p [] [ Html.text "run parseABC \"ABCDE\"", Html.text resultParseABC1 ]
            , Html.p [] [ Html.text "run parseABC \"A|CDE\"", Html.text resultParseABC2 ]
            , Html.p [] [ Html.text "run parseABC \"AB|DE\"", Html.text resultParseABC3 ]
            ]


update : a -> b -> ( b, Cmd msg )
update msg model =
    ( model, Cmd.none )


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none
