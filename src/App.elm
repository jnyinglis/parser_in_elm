module App exposing (main)

{-| Hello
@docs main
-}

import Html exposing (Html)
import ElmParsec.ElmParsec as ElmParsec exposing (..)


{-| -}
main : Program Never number msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| -}
init : ( number, Cmd msg )
init =
    ( 0, Cmd.none )


{-| -}
view : a -> Html msg
view model =
    let
        simpleParseA =
            ElmParsec.charA "ABCDEFGH"

        resultSimpleA =
            toString <| simpleParseA

        parseA : ElmParsec.Parser ( Char, String )
        parseA =
            ElmParsec.char 'A'

        input =
            "ABCDEFGH"

        resultA : String
        resultA =
            toString <| ElmParsec.run parseA input

        parseB =
            ElmParsec.char 'B'

        parseAThenB =
            parseA .>>. parseB

        {- andThen parseA parseB -}
        resultAThenB =
            toString <| ElmParsec.run parseAThenB input

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
            ElmParsec.char 'C'

        bOrElseC =
            ElmParsec.orElse parseB parseC

        aAndThenBorC =
            ElmParsec.andThen parseA bOrElseC

        resultaAndThenBorC_1 =
            toString <| run aAndThenBorC "ABZ"

        resultaAndThenBorC_2 =
            toString <| run aAndThenBorC "ACZ"

        resultaAndThenBorC_3 =
            toString <| run aAndThenBorC "QBZ"

        resultaAndThenBorC_4 =
            toString <| run aAndThenBorC "AQZ"

        parseLowercase =
            ElmParsec.anyOf [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm' ]

        resultParseLowercase =
            toString <| run parseLowercase "mbcdefghijkM"

        parseDigit =
            ElmParsec.anyOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

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
                ElmParsec.map transformTuple tupleParser

        {-
           Compact version of parseThreeDigitsAsStr

           parseThreeDigitsAsStr =
               ( parseDigit .>>. parseDigit .>>. parseDigit)
                   |>> \( ( c1, c2 ), c3) -> String.fromChar c1 ++ String.fromChar c2 ++ String.fromChar c3
        -}
        resultParseThreeDigitsAsStr =
            toString <| ElmParsec.run parseThreeDigitsAsStr "123A"

        parseThreeDigitsAsInt : ElmParsec.Parser ( Result String Int, String )
        parseThreeDigitsAsInt =
            ElmParsec.map String.toInt parseThreeDigitsAsStr

        resultParseThreeDigitsAsInt =
            toString <| run parseThreeDigitsAsInt "123A"

        parsers =
            [ char 'A', char 'B', char 'C' ]

        combined =
            ElmParsec.sequence parsers

        resultCombined =
            toString <| ElmParsec.run combined "ABCD"

        parseABC =
            string "ABC"

        resultParseABC1 =
            toString <| ElmParsec.run parseABC "ABCDE"

        resultParseABC2 =
            toString <| ElmParsec.run parseABC "A|CDE"

        resultParseABC3 =
            toString <| ElmParsec.run parseABC "AB|DE"

        -- Now let’s test many:
        manyA =
            ElmParsec.many (char 'A')

        -- test some success cases
        resultManyA1 =
            toString <| ElmParsec.run manyA "ABCD"

        -- Success (['A'], "BCD")
        resultManyA2 =
            toString <| ElmParsec.run manyA "AACD"

        -- Success (['A'; 'A'], "CD")
        resultManyA3 =
            toString <| ElmParsec.run manyA "AAAD"

        -- Success (['A'; 'A'; 'A'], "D")
        -- test a case with no matches
        resultManyA4 =
            toString <| ElmParsec.run manyA "|BCD"

        -- Success ([], "|BCD")
        -- now let’s test many1:
        -- define parser for one digit
        digit =
            ElmParsec.anyOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

        -- define parser for one or more digits
        digits =
            ElmParsec.many1 digit

        resultManydigits1 =
            toString <| ElmParsec.run digits "1ABC"

        -- Success (['1'], "ABC")
        resultManydigits2 =
            toString <| ElmParsec.run digits "12BC"

        -- Success (['1'; '2'], "BC")
        resultManydigits3 =
            toString <| ElmParsec.run digits "123C"

        -- Success (['1'; '2'; '3'], "C")
        resultManydigits4 =
            toString <| ElmParsec.run digits "1234"

        -- Success (['1'; '2'; '3'; '4'], "")
        resultManydigits5 =
            toString <| ElmParsec.run digits "ABC"

        -- Failure "Expecting '9'. Got 'A'"
        resultPint1 =
            toString <| ElmParsec.run ElmParsec.int "1ABC"

        -- Success (1, "ABC")
        resultPint2 =
            toString <| ElmParsec.run ElmParsec.int "12BC"

        -- Success (12, "BC")
        resultPint3 =
            toString <| ElmParsec.run ElmParsec.int "123C"

        -- Success (123, "C")
        resultPint4 =
            toString <| ElmParsec.run ElmParsec.int "-1234"

        -- Success (1234, "")
        resultPint5 =
            toString <| ElmParsec.run ElmParsec.int "ABC"

        -- Failure "Expecting '9'. Got 'A'"
        digitThenSemicolon =
            digit .>>. ElmParsec.opt (ElmParsec.char ';')

        resultOpt1 =
            toString <| ElmParsec.run digitThenSemicolon "1;"

        -- Success (('1', Some ';'), "")
        resultOpt2 =
            toString <| ElmParsec.run digitThenSemicolon "1"

        pdoublequote =
            char '"'

        quotedInteger =
            ElmParsec.between pdoublequote int pdoublequote

        resultBetween =
            toString <| ElmParsec.run quotedInteger "\"1234\""

        comma =
            ElmParsec.char ','

        zeroOrMoreDigitList =
            ElmParsec.sepBy digit comma

        oneOrMoreDigitList =
            ElmParsec.sepBy1 digit comma

        resultOneOrMore1 =
            toString <| ElmParsec.run oneOrMoreDigitList "1;"

        -- Success (['1'], ";")
        resultOneOrMore2 =
            toString <| ElmParsec.run oneOrMoreDigitList "1,2;"

        -- Success (['1'; '2'], ";")
        resultOneOrMore3 =
            toString <| ElmParsec.run oneOrMoreDigitList "1,2,3;"

        -- Success (['1'; '2'; '3'], ";")
        resultOneOrMore =
            toString <| ElmParsec.run oneOrMoreDigitList "Z;"

        resultZeroOrMore1 =
            toString <| ElmParsec.run zeroOrMoreDigitList "1;"

        -- Success (['1'], ";")
        resultZeroOrMore2 =
            toString <| ElmParsec.run zeroOrMoreDigitList "1,2;"

        -- Success (['1'; '2'], ";")
        resultZeroOrMore3 =
            toString <| ElmParsec.run zeroOrMoreDigitList "1,2,3;"

        -- Success (['1'; '2'; '3'], ";")
        resultZeroOrMore4 =
            toString <| ElmParsec.run zeroOrMoreDigitList "Z;"

        -- Success ([], "Z;")
        -- Failure "Expecting '9'. Got 'Z'"
        digitThenSemicolon2 =
            digit .>> ElmParsec.opt (char ';')

        resultDigitSemicolon2_1 =
            toString <| ElmParsec.run digitThenSemicolon2 "1;"

        resultDigitSemicolon2_2 =
            toString <| ElmParsec.run digitThenSemicolon2 "1"
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
            , Html.p [] [ Html.text "run combined \"ABCD\" ", Html.text resultCombined ]
            , Html.p [] [ Html.text "run parseABC \"ABCDE\" ", Html.text resultParseABC1 ]
            , Html.p [] [ Html.text "run parseABC \"A|CDE\" ", Html.text resultParseABC2 ]
            , Html.p [] [ Html.text "run parseABC \"AB|DE\" ", Html.text resultParseABC3 ]
            , Html.p [] [ Html.text "run manyA \"ABCD\" ", Html.text resultManyA1 ]
            , Html.p [] [ Html.text "run manyA \"AACD\" ", Html.text resultManyA2 ]
            , Html.p [] [ Html.text "run manyA \"AAAD\" ", Html.text resultManyA3 ]
            , Html.p [] [ Html.text "run manyA \"|BCD\" ", Html.text resultManyA4 ]
            , Html.p [] [ Html.text "run digits \"1ABC\" ", Html.text resultManydigits1 ]
            , Html.p [] [ Html.text "run digits \"12BC\" ", Html.text resultManydigits2 ]
            , Html.p [] [ Html.text "run digits \"123C\" ", Html.text resultManydigits3 ]
            , Html.p [] [ Html.text "run digits \"1234\" ", Html.text resultManydigits4 ]
            , Html.p [] [ Html.text "run digits \"ABC\" ", Html.text resultManydigits5 ]
            , Html.p [] [ Html.text "run int \"1ABC\" ", Html.text resultPint1 ]
            , Html.p [] [ Html.text "run int \"12BC\" ", Html.text resultPint2 ]
            , Html.p [] [ Html.text "run int \"123C\" ", Html.text resultPint3 ]
            , Html.p [] [ Html.text "run int \"-1234\" ", Html.text resultPint4 ]
            , Html.p [] [ Html.text "run int \"ABC\" ", Html.text resultPint5 ]
            , Html.p [] [ Html.text "run opt \"1;\" ", Html.text resultOpt1 ]
            , Html.p [] [ Html.text "run opt \"1\" ", Html.text resultOpt2 ]
            , Html.p [] [ Html.text "run between \"1234\" ", Html.text resultBetween ]
            , Html.p [] [ Html.text "run oneOrMoreDigitList \"1;\" ", Html.text resultOneOrMore1 ]
            , Html.p [] [ Html.text "run oneOrMoreDigitList \"1,2;\" ", Html.text resultOneOrMore2 ]
            , Html.p [] [ Html.text "run oneOrMoreDigitList \"1,2,3;\" ", Html.text resultOneOrMore3 ]
            , Html.p [] [ Html.text "run zeroOrMoreDigitList \"1;\" ", Html.text resultZeroOrMore1 ]
            , Html.p [] [ Html.text "run zeroOrMoreDigitList \"1,2;\" ", Html.text resultZeroOrMore2 ]
            , Html.p [] [ Html.text "run zeroOrMoreDigitList \"1,2,3;\" ", Html.text resultZeroOrMore3 ]
            , Html.p [] [ Html.text "run zeroOrMoreDigitList \"Z;\" ", Html.text resultZeroOrMore4 ]
            , Html.p [] [ Html.text "run opt \"1;\" ", Html.text resultDigitSemicolon2_1 ]
            , Html.p [] [ Html.text "run opt \"1\" ", Html.text resultDigitSemicolon2_2 ]
            ]


{-| -}
update : a -> b -> ( b, Cmd msg )
update msg model =
    ( model, Cmd.none )


{-| -}
subscriptions : a -> Sub msg
subscriptions model =
    Sub.none
