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
        [ contains
        , endsWith isItem
        , startsWith
        , is
        , equals isItem
        , inTheFuture
        , inThePast
        , lesserThan isItem
        , greaterThan isItem
        , lesserOrEqual isItem
        , greaterOrEqual isItem
        ]


contains : Parser (OperandConst -> Operation)
contains =
    succeed (operationCons Contains)
        |. symbol "ilike."
        |. symbol "*"
        |= getUntil "*"
        |> backtrackable


endsWith : Bool -> Parser (OperandConst -> Operation)
endsWith isItem =
    succeed (operationCons EndsWith)
        |. symbol "ilike."
        |. symbol "*"
        |= value isItem
        |> backtrackable


startsWith : Parser (OperandConst -> Operation)
startsWith =
    succeed (operationCons StartsWith)
        |. symbol "ilike."
        |= getUntil "*"


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


getUntil : String -> Parser String
getUntil terminator =
    getChompedString <| succeed () |. chompUntil terminator


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
