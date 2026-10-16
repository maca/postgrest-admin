module SearchTest exposing (suite)

{-| URL query level: the query string of an app URL is a PostgREST query
string; splitting it into filters must follow PostgREST's layering — params
split on the raw string, values percent-decoded once at extraction.
-}

import Array
import Dict
import Expect
import Internal.Filter as Filter exposing (Filter)
import Internal.Filter.Operand as Operand
import Internal.Filter.Operation as Operation exposing (Operation(..))
import Internal.Schema exposing (ColumnType(..), Table, Value(..))
import Internal.Search as Search
import Test exposing (..)


suite : Test
suite =
    describe "Search query parsing"
        [ test "splits params on the raw query" <|
            \_ ->
                Search.init table "text=eq.a%26b&int=gt.3"
                    |> filters
                    |> Expect.equal
                        [ textEq "text" "a&b", greaterThanInt "int" "3" ]
        , test "decodes values once" <|
            \_ ->
                Search.init table "text=eq.100%25"
                    |> filters
                    |> Expect.equal [ textEq "text" "100%" ]
        , test "keeps separators inside values" <|
            \_ ->
                Search.init table "text=eq.a%2Cb&text=eq.a%29b"
                    |> filters
                    |> Expect.equal
                        [ textEq "text" "a,b", textEq "text" "a)b" ]
        , test "does not decode values twice" <|
            \_ ->
                Search.init table "text=eq.a%2520b"
                    |> filters
                    |> Expect.equal [ textEq "text" "a%20b" ]
        , test "parses and= groups" <|
            \_ ->
                Search.init table "and=(int.gte.1,int.lte.10)&text=eq.x"
                    |> filters
                    |> List.map (filterName >> Just)
                    |> Expect.equal [ Just "int", Just "text" ]
        , test "ignores non-filter params" <|
            \_ ->
                Search.init table "order=name.asc&limit=5"
                    |> filters
                    |> Expect.equal []
        ]


filters : Search.Search -> List Filter
filters search =
    Array.toList search.filters


filterName : Filter -> String
filterName =
    Filter.columnName


textEq : String -> String -> Filter
textEq name value =
    Filter.text name Filter.equals value


greaterThanInt : String -> String -> Filter
greaterThanInt name value =
    Filter.init name (Filter.greaterThan (Operand.int value))


table : Table
table =
    let
        column val colType =
            { primaryKey = False
            , foreignKey = Nothing
            , required = False
            , value = val
            , columnType = colType
            , options = []
            }
    in
    { name = "test_table"
    , columns =
        Dict.fromList
            [ ( "int", column (Int 0) IntegerCol )
            , ( "text", column (String "") TextCol )
            , ( "name", column (String "") StringCol )
            ]
    , referencedBy = []
    }
