module Internal.Filter.Parser exposing (OperandConst, enum, operation)

import Internal.Filter.Operand as Operand exposing (Enum, Operand)
import Internal.Filter.Operation exposing (Operation(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , backtrackable
        , chompIf
        , chompUntil
        , chompWhile
        , getChompedString
        , succeed
        , symbol
        , token
        , variable
        )
import Set exposing (Set)


type alias OperandConst =
    String -> Operand


operation : Bool -> Parser (OperandConst -> Operation)
operation isItem =
    Parser.oneOf
        [ pattern isItem
        , is
        , equals isItem
        , inTheFuture
        , inThePast
        , lesserThan isItem
        , greaterThan isItem
        , lesserOrEqual isItem
        , greaterOrEqual isItem
        ]


{-| Pattern operands are read whole, then classified by their unescaped
leading/trailing `*`. `\*` unescapes to `%` — PostgREST rewrites `*` to `%`
even when backslash-escaped, so a literal `*` is unrepresentable.
-}
pattern : Bool -> Parser (OperandConst -> Operation)
pattern isItem =
    succeed identity
        |. symbol "ilike."
        |= (value isItem |> Parser.andThen classify)


classify : String -> Parser (OperandConst -> Operation)
classify raw =
    patternOperation raw
        |> Maybe.map succeed
        |> Maybe.withDefault (Parser.problem "pattern not representable as a filter")


patternOperation : String -> Maybe (OperandConst -> Operation)
patternOperation raw =
    let
        ( leading, inner1 ) =
            unconsUnescaped '*' raw

        ( trailing, inner ) =
            unsnocUnescaped '*' inner1
    in
    if String.isEmpty raw || hasUnescapedStar inner then
        Nothing

    else
        case ( leading, trailing ) of
            ( True, True ) ->
                Just <| operationCons Contains (unescape inner)

            ( False, True ) ->
                Just <| operationCons StartsWith (unescape inner)

            ( True, False ) ->
                Just <| operationCons EndsWith (unescape inner)

            ( False, False ) ->
                Nothing


unconsUnescaped : Char -> String -> ( Bool, String )
unconsUnescaped char str =
    case String.uncons str of
        Just ( c, rest ) ->
            ( c == char, if c == char then rest else str )

        Nothing ->
            ( False, str )


unsnocUnescaped : Char -> String -> ( Bool, String )
unsnocUnescaped char str =
    let
        before =
            String.dropRight 1 str
    in
    if
        String.right 1 str
            == String.fromChar char
            && (modBy 2 (countTrailing before) == 0)
    then
        ( True, before )

    else
        ( False, str )


countTrailing : String -> Int
countTrailing str =
    if String.endsWith "\\" str then
        1 + countTrailing (String.dropRight 1 str)

    else
        0


hasUnescapedStar : String -> Bool
hasUnescapedStar str =
    case String.uncons str of
        Just ( '\\', rest ) ->
            case String.uncons rest of
                Just ( _, rest2 ) ->
                    hasUnescapedStar rest2

                Nothing ->
                    False

        Just ( '*', _ ) ->
            True

        Just ( _, rest ) ->
            hasUnescapedStar rest

        Nothing ->
            False


unescape : String -> String
unescape str =
    case String.uncons str of
        Just ( '\\', rest ) ->
            case String.uncons rest of
                Just ( c, rest2 ) ->
                    (if c == '*' then
                        "%"

                     else
                        String.fromChar c
                    )
                        ++ unescape rest2

                Nothing ->
                    "\\"

        Just ( c, rest ) ->
            String.fromChar c ++ unescape rest

        Nothing ->
            ""


is : Parser (OperandConst -> Operation)
is =
    succeed identity
        |. symbol "is."
        |= Parser.oneOf [ true, false, null ]


true : Parser (OperandConst -> Operation)
true =
    succeed (always IsTrue) |. symbol "true"


false : Parser (OperandConst -> Operation)
false =
    succeed (always IsFalse) |. symbol "false"


null : Parser (OperandConst -> Operation)
null =
    succeed (always (IsNull Nothing)) |. symbol "null"


equals : Bool -> Parser (OperandConst -> Operation)
equals isItem =
    succeed (operationCons Equals)
        |. symbol "eq."
        |= value isItem


inTheFuture : Parser (OperandConst -> Operation)
inTheFuture =
    succeed (always <| IsInTheFuture Nothing)
        |. symbol "gt.now"


inThePast : Parser (OperandConst -> Operation)
inThePast =
    succeed (always <| IsInThePast Nothing)
        |. symbol "lt.now"


lesserThan : Bool -> Parser (OperandConst -> Operation)
lesserThan isItem =
    succeed (operationCons LesserThan)
        |. symbol "lt."
        |= value isItem


greaterThan : Bool -> Parser (OperandConst -> Operation)
greaterThan isItem =
    succeed (operationCons GreaterThan)
        |. symbol "gt."
        |= value isItem


lesserOrEqual : Bool -> Parser (OperandConst -> Operation)
lesserOrEqual isItem =
    succeed (operationCons LesserOrEqual)
        |. symbol "lte."
        |= value isItem


greaterOrEqual : Bool -> Parser (OperandConst -> Operation)
greaterOrEqual isItem =
    succeed (operationCons GreaterOrEqual)
        |. symbol "gte."
        |= value isItem


enum : Parser (List String -> Operation)
enum =
    Parser.oneOf
        [ succeed (Set.fromList >> enumOperationCons OneOf)
            |. symbol "in."
            |= items
        , succeed (Set.fromList >> enumOperationCons NoneOf)
            |. symbol "not.in."
            |= items
        ]


items : Parser (List String)
items =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = item
        , trailing = Forbidden
        }


item : Parser String
item =
    Parser.oneOf
        [ succeed identity |= string '"'
        , succeed identity
            |= variable
                { start = always True
                , inner = (/=) ','
                , reserved = Set.empty
                }
        ]


string : Char -> Parser String
string delimiter =
    Parser.succeed ()
        |. token (String.fromChar delimiter)
        |. Parser.loop delimiter stringHelp
        |> getChompedString
        -- Remove quotes
        |> Parser.map (String.dropLeft 1 >> String.dropRight 1)


stringHelp : Char -> Parser (Parser.Step Char ())
stringHelp delimiter =
    Parser.oneOf
        [ succeed (Parser.Done ())
            |. token (String.fromChar delimiter)
        , succeed (Parser.Loop delimiter)
            |. chompIf ((==) '\\')
            |. chompIf (always True)
        , succeed (Parser.Loop delimiter)
            |. chompIf (\char -> char /= '\\' && char /= delimiter)
        ]


value : Bool -> Parser String
value isItem =
    let
        exclude =
            if isItem then
                [ ',', ')' ]

            else
                []
    in
    succeed identity
        |= (succeed ()
                |. chompWhile (\c -> List.member c exclude |> not)
                |> getChompedString
           )


operationCons : (Operand -> Operation) -> String -> OperandConst -> Operation
operationCons makeOperation val makeOperator =
    makeOperation <| makeOperator val


enumOperationCons :
    (Enum -> Operation)
    -> Set String
    -> List String
    -> Operation
enumOperationCons makeOperation chosen choices =
    makeOperation <| Operand.enum choices chosen
