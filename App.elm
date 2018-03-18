module App exposing (main)

import Html exposing (Html)


main : Program Never number msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


pcharA : String -> ( Bool, String )
pcharA input =
    case String.uncons input of
        Just ( 'A', remaining ) ->
            ( True, remaining )

        _ ->
            ( False, input )


type Result a
    = Success a
    | Failure String


type Parser a
    = Parser (String -> Result a)


pchar : Char -> Parser ( Char, String )
pchar charToMatch =
    let
        innerFn input =
            case String.uncons input of
                Just ( charMatched, remaining ) ->
                    case charMatched == charToMatch of
                        True ->
                            Success ( charMatched, remaining )

                        False ->
                            Failure <| "Expecting " ++ String.fromChar charToMatch ++ ". Got " ++ String.fromChar charMatched

                Nothing ->
                    Failure "No more input"
    in
        Parser innerFn


andThen : Parser ( a, String ) -> Parser ( b, c ) -> Parser ( ( a, b ), c )
andThen parser1 parser2 =
    let
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


(.>>.) : Parser ( a, String ) -> Parser ( b, c ) -> Parser ( ( a, b ), c )
(.>>.) =
    andThen


identity : a -> a
identity a =
    a


orElse : Parser a -> Parser a -> Parser a
orElse parser1 parser2 =
    let
        innerFn input =
            let
                result1 =
                    run parser1 input
            in
                case result1 of
                    Success result ->
                        result1

                    Failure err ->
                        run parser2 input
    in
        Parser innerFn


(<|>) : Parser a -> Parser a -> Parser a
(<|>) =
    orElse


choice : List (Parser ( Char, String )) -> Parser ( Char, String )
choice listOfParsers =
    let
        innerFn input =
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
        List.foldl (orElse) firstParser restOfParsers


anyOf : List Char -> Parser ( Char, String )
anyOf listOfChars =
    listOfChars
        |> List.map pchar
        |> choice


run : Parser a -> String -> Result a
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
        parseA =
            pchar 'A'

        input =
            "ABCDEFGH"

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
            toString <| run parseLowercase "AbcdefghijkM"

        parseDigit =
            anyOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

        resultParseDigit =
            toString <| run parseDigit "1ABC"
    in
        Html.div []
            [ Html.p [] [ Html.text "Hello, World! " ]
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
            ]


update : a -> b -> ( b, Cmd msg )
update msg model =
    ( model, Cmd.none )


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none
