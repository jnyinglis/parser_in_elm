module ElmParsec.ElmParsec
    exposing
        ( charA
        , char
        , andThen
        , orElse
        , choice
        , anyOf
        , string
        , sequence
        , map
        , Parser
        , (.>>.)
        , (<|>)
        , many
        , many1
        , int
        , opt
        , (.>>)
        , (>>.)
        , between
        , sepBy1
        , sepBy
        , run
        )

{-| In this series, we’ll look at how so-called "applicative parsers" work. In order to understand something, there’s nothing like building it for yourself, and so we’ll create a basic parser library from scratch, and then some useful parser combinators, and then finish off by building a complete JSON parser.

Now terms like “applicative parsers” and “parser combinators” can make this approach seem complicated, but rather than attempting to explain these concepts up front, we’ll just dive in and start coding.

We’ll build up to the complex stuff incrementally via a series of implementations, where each implementation is only slightly different from the previous one. By using this approach, I hope that at each stage the design and concepts will be easy to understand, and so by the end of this series, parser combinators will have become completely demystified.

There will be four posts in this series:

In this, the first post, we’ll look at the basic concepts of parser combinators and build the core of the library.
In the second post, we’ll build up a useful library of combinators.
In the third post, we’ll work on providing helpful error messages.
In the last post, we’ll build a JSON parser using this parser library.
And in this post we created the following additional combinators:

  - bind chains the result of a parser to another parser-producing function.
  - map transforms the result of a parser.
  - return lifts an normal value into the world of parsers.
  - apply allows us to lift multi-parameter functions into functions that work on Parsers.
  - lift2 uses apply to lift two-parameter functions into Parser World.
  - sequence converts a list of Parsers into a Parser containing a list.
  - many matches zero or more occurences of the specified parser.
  - many1 matches one or more occurences of the specified parser.
  - opt matches an optional occurrence of the specified parser.
  - .>> keeps only the result of the left side parser.
  - > > . keeps only the result of the right side parser.
  - between keeps only the result of the middle parser.
  - sepBy parses zero or more occurrences of a parser with a separator.
  - sepBy1 parses one or more occurrences of a parser with a separator.


# Entry

@docs main


## Basics

@docs char, andThen, orElse, choice, anyOf

-}


{-| Implementation 3. Returning a Success/Failure
We want to be able to tell the difference between a successful match and a failure, and returning a stringly-typed message is not very helpful, so let’s define a special “choice” type to indicate the difference. I’ll call it Result:

type Result<'a> =
| Success of 'a
| Failure of string
The Success case is generic and can contain any value. The Failure case contains an error message.

Note: for more on using this Success/Failure approach, see my talk on functional error handling.

We can now rewrite the parser to return one of the Result cases, like this:

-}
type ParserResult a
    = Success a
    | Failure String


type Parser a
    = Parser (String -> ParserResult a)


{-| "bind" takes a parser-producing function f, and a parser p
and passes the output of p into f, to create a new parser
-}
bind : (a -> Parser b) -> Parser ( a, String ) -> Parser b
bind f p =
    let
        innerFn input =
            let
                result1 =
                    run p input
            in
                case result1 of
                    Failure err ->
                        -- return error from parser1
                        Failure err

                    Success ( value1, remainingInput ) ->
                        -- apply f to get a new parser
                        let
                            p2 =
                                f value1
                        in
                            -- run parser with remaining input
                            run p2 remainingInput
    in
        Parser innerFn


(>>=) : Parser ( a, String ) -> (a -> Parser b) -> Parser b
(>>=) p f =
    bind f p


{-| -}
charA : String -> ( Bool, String )
charA input =
    -- check for the character A in the input
    case String.uncons input of
        Just ( 'A', remaining ) ->
            ( True, remaining )

        _ ->
            ( False, input )


{-| Implementation 2. Parsing a specified character
Let’s refactor so that we can pass in the character we want to match, rather than having it be hard coded.

And this time, rather than returning true or false, we’ll return a message indicating what happened.

We’ll call the function pchar for “parse char”. Here’s the code:

-}
char : Char -> Parser ( Char, String )
char charToMatch =
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


{-| Combining two parsers in sequence: the “and then” combinator
That last implementation is good enough for basic parsing logic. We’ll revisit it later, but now let’s move up a level and develop some ways of combining parsers together – the “parser combinators” mentioned at the beginning.

We’ll start with combining two parsers in sequence. For example, say that we want a parser that matches “A” and then “B”. We could try writing something like this:

let parseA = pchar 'A'
let parseB = pchar 'B'

let parseAThenB = parseA >> parseB
but that gives us a compiler error, as the output of parseA does not match the input of parseB, and so they cannot be composed like that.

If you are familiar with functional programming patterns, the need to chain a sequence of wrapped types together like this happens frequently, and the solution is a bind function.

However, in this case, I won’t implement bind but will instead go straight to an andThen implemention.

The implementation logic will be as follows:

Run the first parser.
If there is a failure, return.
Otherwise, run the second parser with the remaining input.
If there is a failure, return.
If both parsers succeed, return a pair (tuple) that contains both parsed values.
Here’s the code for andThen:

-}
andThen : Parser ( a, String ) -> Parser ( b, String ) -> Parser ( ( a, b ), String )
andThen parser1 parser2 =
    parser1
        >>= (\p1Result ->
                parser2
                    >>= (\p2Result ->
                            return ( p1Result, p2Result )
                        )
            )


{-| This is the documentation for infix .>>. operator for andThen
-}
(.>>.) : Parser ( a, String ) -> Parser ( b, String ) -> Parser ( ( a, b ), String )
(.>>.) =
    andThen


{-| Choosing between two parsers: the “or else” combinator
Let’s look at another important way of combining parsers – the “or else” combinator.

For example, say that we want a parser that matches “A” or “B”. How could we combine them?

The implementation logic would be:

Run the first parser.
On success, return the parsed value, along with the remaining input.
Otherwise, on failure, run the second parser with the original input…
…and in this case, return the result (success or failure) from the second parser.
Here’s the code for orElse:

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


{-| Choosing from a list of parsers: “choice” and “anyOf”
This is where where the power of combinators starts kicking in, because with orElse in our toolbox, we can use it to build even more combinators.

For example, let’s say that we want choose from a list of parsers, rather than just two.

Well, that’s easy. If we have a pairwise way of combining things, we can extend that to combining an entire list using reduce (for more on working with reduce, see this post on monoids ).

/// Choose any of a list of parsers
let choice listOfParsers =
List.reduce ( <|> ) listOfParsers
Note that this will fail if the input list is empty, but we will ignore that for now.

The signature of choice is:

val choice :
Parser<'a> list -> Parser<'a>
which shows us that, as expected, the input is a list of parsers, and the output is a single parser.

-}
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


{-| With choice available, we can create an anyOf parser that matches any character in a list, using the following logic:

The input is a list of characters
Each char in the list is transformed into a parser for that char using pchar
Finally, all the parsers are combined using choice
Here’s the code:

-}
anyOf : List Char -> Parser ( Char, String )
anyOf listOfChars =
    listOfChars
        |> List.map char
        |> choice


{-| apply a function to the value inside a parser

The logic is:

Inside the innerFn, run the parser to get the result.
If the result was a success, apply the specified function to the success value to get a new, transformed value, and…
…return the new, mapped, value instead of the original value.

-}
map : (a -> a1) -> Parser ( a, String ) -> Parser ( a1, String )
map f =
    bind (f >> return)


{-| infix version of map
-}
(<!>) : (a -> a1) -> Parser ( a, String ) -> Parser ( a1, String )
(<!>) =
    map


{-| "piping" version of map
-}
(|>>) : Parser ( a, String ) -> (a -> a1) -> Parser ( a1, String )
(|>>) x f =
    map f x


{-| Lift a value to a Parser
apply and return – lifting functions to the world of Parsers
To achieve our goal of creating a parser that matches a list of characters, we need two more helper functions which I will call return and apply.

return simply transforms a normal value into a value in Parser World

-}
return : a -> Parser ( a, String )
return x =
    let
        innerFn : String -> ParserResult ( a, String )
        innerFn input =
            Success ( x, input )
    in
        Parser innerFn


{-| apply a wrapped function to a wrapped value
apply transforms a Parser containing a function (Parser< 'a->'b >) into a function in Parser World (Parser<'a> -> Parser<'b >)
-}
apply : Parser ( a -> b, String ) -> Parser ( a, String ) -> Parser ( b, String )
apply fP xP =
    -- create a Parser containing a pair (f, x)
    fP
        >>= (\f ->
                xP
                    >>= (\x ->
                            return (f x)
                        )
            )


{-| infix <*> for apply
-}
(<*>) : Parser ( a -> b, String ) -> Parser ( a, String ) -> Parser ( b, String )
(<*>) =
    apply


{-| lift a two parameter function to Parser World
-}
lift2 : (a -> a1 -> b) -> Parser ( a, String ) -> Parser ( a1, String ) -> Parser ( b, String )
lift2 f xP yP =
    return f <*> xP <*> yP


{-| Let’s see some examples of using lift2 in practice. First, lifting integer addition to addition of Parsers
The signature is:

addP : Parser ( number, String ) -> Parser ( number, c ) -> Parser ( number, c )
which shows that addP does indeed take two Parser ( number, c) parameters and returns another Parser (number, c)

-}
addP : Parser ( number, String ) -> Parser ( number, String ) -> Parser ( number, String )
addP =
    lift2 (+)


{-| And here’s the startsWith function being lifted to Parser World
Again, the signature of startsWithP is parallel to the signature of startsWith, but lifted to the world of Parsers.
-}
startsWithP : Parser ( String, String ) -> Parser ( String, String ) -> Parser ( Bool, String )
startsWithP =
    lift2 String.startsWith


{-| sequence – transforming a list of Parsers into a single Parser
We now have the tools we need to implement our sequencing combinator! The logic will be:

Start with the list “cons” operator. This is the two-parameter function that prepends a “head” element onto a “tail” of elements to make a new list.
Lift cons into the world of Parsers using lift2.
We now have a a function that prepends a head Parser to a tail list of Parsers to make a new list of Parsers, where:
The head Parser is the first element in the list of parsers that has been passed in.
The tail is generated by calling the same function recursively with the next parser in the list.
When the input list is empty, just return a Parser containing an empty list.
Here’s the implementation:

-}
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
                return []

            head :: tail ->
                consP head (sequence tail)


{-| Implementing the string parser

At last, we can implement the parser that matches a string, which we’ll call string.

The logic is:

Convert the string into a list of characters.
Convert each character into a Parser<char>.
Use sequence to convert the list of Parser<char> into a single Parser<char list>.
And finally, use map to convert the Parser<char list> into a Parser<string>.
Here’s the code:

-}
string : String -> Parser ( String, String )
string str =
    str
        |> String.toList
        |> List.map char
        |> sequence
        |> map String.fromList


{-|

1.  many and many1 – matching a parser multiple times
    Another common need is to match a particular parser as many times as you can. For example:

When matching an integer, you want to match as many digit characters as you can.
When matching a run of whitespace, you want to match as many whitespace characters as you can.
There are slightly different requirements for these two cases.

When matching whitespace, it is often optional, so we want a “zero or more” matcher, which we’ll call many.
On the other hand, when matching digits for an integer, you want to match at least one digit, so we want a “one or more” matcher, which we’ll call many1.
Before creating these, we’ll define a helper function which matches a parser zero or more times. The logic is:

Run the parser.
If the parser returns Failure (and this is key) just return an empty list. That is, this function can never fail!
If the parser succeeds:
Call the function recursively to get the remaining values (which could also be an empty list).
Then combine the first value and the remaining values.
Here’s the code:

-}
parseZeroOrMore : Parser ( a, String ) -> String -> ( List a, String )
parseZeroOrMore parser input =
    let
        -- run parser with the input
        firstResult =
            run parser input
    in
        -- test the result for Failure/Success
        case firstResult of
            Failure err ->
                -- if parse fails, return empty list
                ( [], input )

            Success ( firstValue, inputAfterFirstParse ) ->
                -- if parse succeeds, call recursively
                -- to get the subsequent values
                let
                    ( subsequentValues, remainingInput ) =
                        parseZeroOrMore parser inputAfterFirstParse

                    values =
                        firstValue :: subsequentValues
                in
                    ( values, remainingInput )



{- With this helper function, we can easily define many now – it’s just a wrapper over parseZeroOrMore:
   match zero or more occurences of the specified parser
-}


many : Parser ( a, String ) -> Parser ( List a, String )
many parser =
    let
        innerFn input =
            -- parse the input -- wrap in Success as it always succeeds
            Success (parseZeroOrMore parser input)
    in
        Parser innerFn


{-| many1 - the “one or more” combinator many1, using the following logic:

Run the parser.
If it fails, return the failure.
If it succeeds:
Call the helper function parseZeroOrMore to get the remaining values.
Then combine the first value and the remaining values.
/// match one or more occurences of the specified parser

-}
many1 : Parser ( a, String ) -> Parser ( List a, String )
many1 parser =
    parser
        >>= (\head ->
                many parser
                    >>= (\tail ->
                            return (head :: tail)
                        )
            )


{-| int - Parsing an integer

Using many1, we can create a parser for an integer. The implementation logic is:

Create a parser for a digit.
Use many1 to get a list of digits.
Using map, transform the result (a list of digits) into a string and then into an int.
Here’s the code:

-}
int : Parser ( Int, String )
int =
    -- helper
    let
        resultToInt ( sign, charList ) =
            -- ignore int overflow for now
            let
                i =
                    case String.fromList charList |> String.toInt of
                        Ok i ->
                            i

                        Err message ->
                            0
            in
                case sign of
                    Just ch ->
                        -1 * i

                    Nothing ->
                        i

        -- define parser for one digit
        digit =
            anyOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

        -- define parser for one or more digits
        digits =
            many1 digit
    in
        opt (char '-')
            .>>. digits
            |>> resultToInt


{-| opt – matching a parser zero or one time
Sometimes we only want to match a parser zero or one time. For example, the int parser above does not handle negative values. To correct this, we need to be able to handle an optional minus sign.

We can define an opt combinator easily:

Change the result of a specified parser to an option by mapping the result to Some.
Create another parser that always returns None.
Use <|> to choose the sec?NDond (“None”) parser if the first fails.
Here’s the code:

-}
opt : Parser ( a, String ) -> Parser ( Maybe a, String )
opt p =
    let
        some =
            -- lift the Just function into a parser
            p |>> Just

        none =
            return Nothing
    in
        -- orElse some none
        some <|> none


{-| .>> & >>. - Throwing results away

We often want to match something in the input, but we don’t care about the parsed value itself. For example:

For a quoted string, we need to parse the quotes, but we don’t need the quotes themselves.
For a statement ending in a semicolon, we need to ensure the semicolon is there, but we don’t need the semicolon itself.
For whitespace separators, we need to ensure the whitespace is there, but we don’t need the actual whitespace data.
To handle these requirements, we will define some new combinators that throw away the results of a parser:

p1 >>. p2 will apply p1 and p2 in sequence, just like .>>., but throw away the result of p1 and keep the result of p2.
p1 .>> p2 will apply p1 and p2 in sequence, just like .>>., but keep the result of p1 and throw away the result of p2.
These are easy to define – just map over the result of .>>., which is a tuple, and keep only one element of the pair.

These combinators allow us to simplify the digitThenSemicolon example shown earlier:

digit = anyOf ['0'..'9']

-- use .>> below
digitThenSemicolon = digit .>> opt (pchar ';')

run digitThenSemicolon "1;" -- Success ('1', "")
run digitThenSemicolon "1" -- Success ('1', "")
You can see that the result now is the same, whether or not the semicolon was present.

How about an example with whitespace?

The following code creates a parser that looks for “AB” followed by one or more whitespace chars, followed by “CD”.

whitespaceChar = anyOf [' '; '\t'; '\n']
whitespace = many1 whitespaceChar

ab = string "AB"
cd = string "CD"
ab_cd = (ab .>> whitespace) .>>. cd

run ab_cd "AB \t\nCD" -- Success (("AB", "CD"), "")
The result contains “AB” and “CD” only. The whitespace between them has been discarded.

Keep only the result of the left side parser

-}
(.>>) : Parser ( a, String ) -> Parser ( b, String ) -> Parser ( a, String )
(.>>) p1 p2 =
    -- create a pair
    -- andThen p1 p2
    p1
        .>>. p2
        -- then only keep the first value
        |> map (\( a, b ) -> a)


{-| Keep only the result of the right side parser
-}
(>>.) : Parser ( a, String ) -> Parser ( b, String ) -> Parser ( b, String )
(>>.) p1 p2 =
    -- create a pair
    -- andThen p1 p2
    p1
        .>>. p2
        -- then only keep the second value
        |> map (\( a, b ) -> b)


{-| between - A particularly common requirement is to look for a parser between delimiters such as quotes or brackets.

Creating a combinator for this is trivial:

-- Keep only the result of the middle parser
between p1 p2 p3 =
p1 >>. p2 .>> p3
And here it is in use, to parse a quoted integer:

pdoublequote = pchar '"' let quotedInteger = between pdoublequote int pdoublequote run quotedInteger ""1234"" -- Success (1234, "")
run quotedInteger "1234" -- Failure "Expecting '"'. Got '1'"

-}
between : Parser ( a, String ) -> Parser ( b, String ) -> Parser ( d, String ) -> Parser ( b, String )
between p1 p2 p3 =
    -- >>. keeps only the result of the right hand parser
    -- .>> keeps only the result of the left hand parser
    p1 >>. p2 .>> p3


{-| sepBy - Parsing lists with separators
Another common requirement is parsing lists, seperated by something like commas or whitespace.

To implement a “one or more” list, we need to:

First combine the separator and parser into one combined parser, but using >>. to throw away the separator value.
Next, look for a list of the separator/parser combo using many.
Then prefix that with the first parser and combine the results.
Here’s the code:

-- Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
let sepThenP = sep >>. p
p .>>. many sepThenP
|>> fun (p,pList) -> p::pList
For the “zero or more” version, we can choose the empty list as an alternate if sepBy1 does not find any matches:

-- Parses zero or more occurrences of p separated by sep
let sepBy p sep =
sepBy1 p sep <|> returnP []

-}
sepBy1 : Parser ( b, String ) -> Parser ( a, String ) -> Parser ( List b, String )
sepBy1 p sep =
    let
        sepThenP =
            sep >>. p
    in
        p
            .>>. (many sepThenP)
            |>> \( p, pList ) -> p :: pList


sepBy : Parser ( b, String ) -> Parser ( a, String ) -> Parser ( List b, String )
sepBy p sep =
    sepBy1 p sep <|> (return [])


{-| -}
run : Parser a -> String -> ParserResult a
run parser input =
    let
        (Parser innerFn) =
            parser
    in
        innerFn input
