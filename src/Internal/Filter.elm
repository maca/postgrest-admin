module Internal.Filter exposing
    ( Filter
    , between
    , columnName
    , contains
    , date2
    , endsWith
    , equals
    , float
    , float2
    , fromColumn
    , greaterOrEqual
    , greaterThan
    , inDate
    , init
    , int2
    , isFalse
    , isInTheFuture
    , isInThePast
    , isNull
    , isTrue
    , lesserOrEqual
    , lesserThan
    , operation
    , parse
    , startsWith
    , text
    , time
    , time2
    , toPGQuery
    , toQueryString
    )

import Dict
import Internal.Filter.Operand as Operand exposing (Operand)
import Internal.Filter.Operation as Operation exposing (Operation(..))
import Internal.Filter.Parser as FilterParser
import Internal.Schema exposing (Column, ColumnType(..), Table, Value(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , succeed
        , symbol
        , token
        )
import Postgrest.Client as PG
import Set exposing (Set)
import Time exposing (posixToMillis, toHour, utc)
import Time.Extra as Time
import Url exposing (percentDecode)


type Filter
    = Filter String Operation


type alias OperandConst =
    String -> Operand


toPGQuery : Filter -> Maybe PG.Param
toPGQuery (Filter name op) =
    Operation.toPGQuery name op


toQueryString : Filter -> String
toQueryString (Filter name op) =
    Operation.toPGQuery name op
        |> List.singleton
        |> List.filterMap identity
        |> PG.toQueryString


columnName : Filter -> String
columnName (Filter name _) =
    name


operation : Filter -> Operation
operation (Filter _ op) =
    op



-- Constructors


fromColumn : String -> Column -> Maybe Filter
fromColumn colName col =
    case col.columnType of
        StringCol ->
            let
                enumChoices =
                    col.options
                        |> List.filterMap
                            (\value ->
                                case value of
                                    String str ->
                                        Just str

                                    _ ->
                                        Nothing
                            )
            in
            if not (List.isEmpty enumChoices) then
                Just (oneOf colName enumChoices Set.empty)

            else
                Just (text colName equals "")

        TextCol ->
            Just (text colName equals "")

        IntegerCol ->
            Just (int colName equals "")

        FloatCol ->
            Just (float colName equals "")

        BoolCol ->
            Just (isTrue colName)

        TimeWithoutTimezoneCol ->
            Just (time colName inDate "")

        TimeCol ->
            Just (time colName inDate "")

        TimestampWithoutTimezomeCol ->
            Just (time colName inDate "")

        TimestampCol ->
            Just (time colName inDate "")

        DateCol ->
            Just (date colName inDate "")

        JsonCol ->
            Just (text colName equals "")

        _ ->
            Nothing


init : String -> Operation -> Filter
init =
    Filter


oneOf : String -> List String -> Set String -> Filter
oneOf name choices chosen =
    Filter name <| OneOf <| Operand.enum choices chosen


noneOf : String -> List String -> Set String -> Filter
noneOf name choices chosen =
    Filter name <| NoneOf <| Operand.enum choices chosen



-- Operation constructors


equals : Operand -> Operation
equals =
    Equals


lesserOrEqual : Operand -> Operation
lesserOrEqual =
    LesserOrEqual


greaterOrEqual : Operand -> Operation
greaterOrEqual =
    GreaterOrEqual


lesserThan : Operand -> Operation
lesserThan =
    LesserThan


greaterThan : Operand -> Operation
greaterThan =
    GreaterThan


between : Operand -> Operand -> Operation
between =
    Between


contains : Operand -> Operation
contains =
    Contains


startsWith : Operand -> Operation
startsWith =
    StartsWith


endsWith : Operand -> Operation
endsWith =
    EndsWith


inDate : Operand -> Operation
inDate =
    InDate



-- Filter constructors


text : String -> (Operand -> Operation) -> String -> Filter
text name operationCons value =
    init name (operationCons <| Operand.text value)


int : String -> (Operand -> Operation) -> String -> Filter
int name operationCons value =
    init name (operationCons <| Operand.int value)


int2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
int2 name operationCons valueA valueB =
    init name (operationCons (Operand.int valueA) (Operand.int valueB))


float : String -> (Operand -> Operation) -> String -> Filter
float name operationCons value =
    init name (operationCons <| Operand.float value)


float2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
float2 name operationCons valueA valueB =
    init name (operationCons (Operand.float valueA) (Operand.float valueB))


date : String -> (Operand -> Operation) -> String -> Filter
date name operationCons value =
    init name (operationCons <| Operand.date value)


date2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
date2 name operationCons valueA valueB =
    init name (operationCons (Operand.date valueA) (Operand.date valueB))


time : String -> (Operand -> Operation) -> String -> Filter
time name operationCons value =
    init name (operationCons <| Operand.time value)


time2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
time2 name operationCons valueA valueB =
    init name (operationCons (Operand.time valueA) (Operand.time valueB))


isTrue : String -> Filter
isTrue name =
    Filter name IsTrue


isFalse : String -> Filter
isFalse name =
    Filter name IsFalse


isNull : String -> Filter
isNull name =
    Filter name (IsNull Nothing)


isInTheFuture : String -> Filter
isInTheFuture name =
    Filter name (IsInTheFuture Nothing)


isInThePast : String -> Filter
isInThePast name =
    Filter name (IsInThePast Nothing)



-- Parsing


parse : Table -> String -> List Filter
parse table query =
    let
        ( name, rawValue ) =
            breakOn "=" query

        value =
            percentDecode rawValue |> Maybe.withDefault rawValue
    in
    if name == "and" then
        Parser.run (list table) value
            |> Result.toMaybe
            |> Maybe.withDefault []
            |> List.filterMap identity
            |> combine

    else
        Parser.run (valueFilter table name) value
            |> Result.toMaybe
            |> Maybe.withDefault Nothing
            |> Maybe.map List.singleton
            |> Maybe.withDefault []


breakOn : String -> String -> ( String, String )
breakOn separator string =
    case String.split separator string of
        first :: rest ->
            ( first, String.join separator rest )

        [] ->
            ( "", "" )



-- Parse helpers


valueFilter : Table -> String -> Parser (Maybe Filter)
valueFilter table name =
    succeed (\cons -> cons name)
        |= Parser.oneOf
            [ succeed (enumCons table)
                |= FilterParser.enum
            , succeed (filterCons table)
                |= FilterParser.operation False
            ]


list : Table -> Parser (List (Maybe Filter))
list table =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = columnFilter table
        , trailing = Forbidden
        }


columnFilter : Table -> Parser (Maybe Filter)
columnFilter table =
    let
        colNames =
            Dict.keys table.columns
                |> List.map (\s -> succeed (always s) |= token s)
    in
    succeed (\name f -> f name)
        |= Parser.oneOf colNames
        |. symbol "."
        |= Parser.oneOf
            [ succeed (enumCons table)
                |= FilterParser.enum
            , succeed (filterCons table)
                |= FilterParser.operation True
            ]


-- A gte/lte pair presents as Between and a gte/lt pair spanning a midnight
-- day as InDate; every other condition stays its own filter.


combine : List Filter -> List Filter
combine filters =
    case filters of
        [] ->
            []

        f :: rest ->
            case mergeFrom f rest of
                Just ( merged, remaining ) ->
                    merged :: combine remaining

                Nothing ->
                    f :: combine rest


mergeFrom : Filter -> List Filter -> Maybe ( Filter, List Filter )
mergeFrom f rest =
    case rest of
        [] ->
            Nothing

        f_ :: tail ->
            case merge f f_ of
                Just merged ->
                    Just ( merged, tail )

                Nothing ->
                    mergeFrom f tail |> Maybe.map (Tuple.mapSecond ((::) f_))


merge : Filter -> Filter -> Maybe Filter
merge (Filter name op) (Filter name_ op_) =
    if name /= name_ then
        Nothing

    else
        case ( op, op_ ) of
            ( GreaterOrEqual min, LesserOrEqual max ) ->
                Just <| Filter name (Between min max)

            ( LesserOrEqual max, GreaterOrEqual min ) ->
                Just <| Filter name (Between min max)

            ( GreaterOrEqual min, LesserThan max ) ->
                makeInDate name min max

            ( LesserThan max, GreaterOrEqual min ) ->
                makeInDate name min max

            _ ->
                Nothing


makeInDate : String -> Operand -> Operand -> Maybe Filter
makeInDate name min max =
    case ( min, max ) of
        ( Operand.Time tmin, Operand.Time tmax ) ->
            let
                inDateMapFn pmin pmax =
                    (posixToMillis pmax - posixToMillis pmin == Time.hours24)
                        && toHour utc pmin == 0
            in
            if
                Maybe.map2 inDateMapFn (Time.parse tmin) (Time.parse tmax)
                    == Just True
            then
                Just <| time name inDate (String.slice 0 10 tmin)

            else
                Nothing

        _ ->
            Nothing



-- False


filterCons : Table -> (OperandConst -> Operation) -> String -> Maybe Filter
filterCons table operationCons name =
    Dict.get name table.columns
        |> Maybe.andThen
            (\column ->
                case column.columnType of
                    StringCol ->
                        let
                            enumChoices =
                                column.options
                                    |> List.filterMap
                                        (\value ->
                                            case value of
                                                String str ->
                                                    Just str

                                                _ ->
                                                    Nothing
                                        )
                        in
                        if not (List.isEmpty enumChoices) then
                            IsNull (Just <| NoneOf <| Operand.enum enumChoices Set.empty)
                                |> (Just << Filter name)

                        else
                            Just <| Filter name <| operationCons Operand.text

                    TextCol ->
                        Just <| Filter name <| operationCons Operand.text

                    IntegerCol ->
                        Just <| Filter name <| operationCons Operand.int

                    FloatCol ->
                        Just <| Filter name <| operationCons Operand.float

                    BoolCol ->
                        Just <| Filter name <| operationCons Operand.text

                    TimeWithoutTimezoneCol ->
                        Just <| Filter name <| operationCons Operand.time

                    TimeCol ->
                        Just <| Filter name <| operationCons Operand.time

                    TimestampWithoutTimezomeCol ->
                        Just <| Filter name <| operationCons Operand.time

                    TimestampCol ->
                        Just <| Filter name <| operationCons Operand.time

                    DateCol ->
                        Just <| Filter name <| operationCons Operand.date

                    _ ->
                        Nothing
            )


enumCons : Table -> (List String -> Operation) -> String -> Maybe Filter
enumCons table operationCons name =
    Dict.get name table.columns
        |> Maybe.andThen
            (\column ->
                let
                    choices =
                        column.options
                            |> List.filterMap
                                (\value ->
                                    case value of
                                        String str ->
                                            Just str

                                        _ ->
                                            Nothing
                                )
                in
                if not (List.isEmpty choices) then
                    if operationCons choices == IsNull Nothing then
                        Just <| Filter name <| OneOf <| Operand.enum choices Set.empty

                    else
                        Just <| Filter name <| operationCons choices

                else
                    Nothing
            )
