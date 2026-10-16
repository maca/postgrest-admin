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
import Url exposing (percentDecode)


type alias OperandConst =
    String -> Operand


operation : Parser (OperandConst -> Operation)
operation =
    Parser.oneOf
        [ contains
        , endsWith
        , startsWith
        , is
        , equals
        , inTheFuture
        , inThePast
        , lesserThan
        , greaterThan
        , lesserOrEqual
        , greaterOrEqual
        ]


contains : Parser (OperandConst -> Operation)
contains =
    succeed (operationCons Contains)
        |. symbol "ilike."
        |. symbol "*"
        |= getUntil "*"
        |> backtrackable


endsWith : Parser (OperandConst -> Operation)
endsWith =
    succeed (operationCons EndsWith)
        |. symbol "ilike."
        |. symbol "*"
        |= value
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


equals : Parser (OperandConst -> Operation)
equals =
    succeed (operationCons Equals)
        |. symbol "eq."
        |= value


inTheFuture : Parser (OperandConst -> Operation)
inTheFuture =
    succeed (always <| IsInTheFuture Nothing)
        |. symbol "gt.now"


inThePast : Parser (OperandConst -> Operation)
inThePast =
    succeed (always <| IsInThePast Nothing)
        |. symbol "lt.now"


lesserThan : Parser (OperandConst -> Operation)
lesserThan =
    succeed (operationCons LesserThan)
        |. symbol "lt."
        |= value


greaterThan : Parser (OperandConst -> Operation)
greaterThan =
    succeed (operationCons GreaterThan)
        |. symbol "gt."
        |= value


lesserOrEqual : Parser (OperandConst -> Operation)
lesserOrEqual =
    succeed (operationCons LesserOrEqual)
        |. symbol "lte."
        |= value


greaterOrEqual : Parser (OperandConst -> Operation)
greaterOrEqual =
    succeed (operationCons GreaterOrEqual)
        |. symbol "gte."
        |= value


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


value : Parser String
value =
    let
        exclude =
            [ ',', ')' ]
    in
    succeed (percentDecode >> Maybe.withDefault "")
        |= (succeed ()
                |. chompWhile (\c -> List.member c exclude |> not)
                |> getChompedString
           )


getUntil : String -> Parser String
getUntil terminator =
    succeed (percentDecode >> Maybe.withDefault "")
        |= (getChompedString <| succeed () |. chompUntil terminator)


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
