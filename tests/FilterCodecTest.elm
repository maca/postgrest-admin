module FilterCodecTest exposing (suite)

{-| Round-trip laws for the filter codec: a filter state serialized to a query
param must parse back to the same state, and parsing must be canonical.
-}

import Dict
import Expect
import Internal.Filter as Filter exposing (Filter)
import Internal.Filter.Operand as Operand
import Internal.Schema exposing (ColumnType(..), Table, Value(..))
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Filter codec"
        [ describe "serialize >> parse round trip" (List.map roundTrip cases)
        , describe "parse is canonical" (List.map canonical strings)
        ]


roundTrip : Filter -> Test
roundTrip filter =
    test (Filter.toQueryString filter) <|
        \_ ->
            Filter.parse table (Filter.toQueryString filter)
                |> Expect.equal (Just filter)


canonical : String -> Test
canonical query =
    test query <|
        \_ ->
            let
                once =
                    Filter.parse table query |> Maybe.map Filter.toQueryString

                twice =
                    once
                        |> Maybe.andThen (Filter.parse table)
                        |> Maybe.map Filter.toQueryString
            in
            once |> Expect.equal twice


cases : List Filter
cases =
    [ Filter.text "text" Filter.equals "bar baz"
    , Filter.text "text" Filter.equals "café ñ"
    , Filter.text "text" Filter.equals "a=b"
    , Filter.text "text" Filter.equals "a&b"
    , Filter.text "text" Filter.equals "a+b"
    , Filter.text "text" Filter.equals "0.5"
    , Filter.text "text" Filter.equals "\"hello\""
    , Filter.text "text" Filter.equals "say \"hi\" now"
    , Filter.text "text" Filter.contains "bar baz"
    , Filter.text "text" Filter.startsWith "bar baz"
    , Filter.text "text" Filter.endsWith "bar baz"
    , Filter.init "int" (Filter.equals (Operand.int "5"))
    , Filter.int2 "int" Filter.between "1" "10"
    , Filter.float "float" Filter.lesserThan "1.1"
    , Filter.float "float" Filter.greaterOrEqual "2.2"
    , Filter.date2 "date" Filter.between "2021-01-01" "2021-12-31"
    , Filter.time2 "time" Filter.between "2021-01-02T00:00" "2021-01-03T00:00"
    , Filter.time "time" Filter.inDate "2021-01-02"
    , Filter.isTrue "bool"
    , Filter.isFalse "bool"
    , Filter.isNull "text"
    , Filter.isInTheFuture "time"
    , Filter.isInThePast "time"
    ]


strings : List String
strings =
    [ "text=eq.bar%20baz"
    , "text=ilike.*bar%20baz*"
    , "text=ilike.bar%20baz*"
    , "text=ilike.*bar%20baz"
    , "and=(int.gte.1,int.lte.10)"
    , "and=(time.gte.2021-01-02T00%3A00,time.lte.2021-01-03T00%3A00)"
    , "and=(time.gte.2021-01-02T00%3A00%3A00.000Z,time.lt.2021-01-03T00%3A00%3A00.000Z)"
    , "bool=is.true"
    , "bool=is.false"
    , "text=is.null"
    , "time=gt.now"
    , "time=lt.now"
    , "status=in.(%22draft%22,%22published%22)"
    , "status=not.in.(%22draft%22,%22published%22)"
    ]


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

        status =
            column (String "") StringCol
    in
    { name = "test_table"
    , columns =
        Dict.fromList
            [ ( "float", column (Float 0.0) FloatCol )
            , ( "int", column (Int 0) IntegerCol )
            , ( "text", column (String "") TextCol )
            , ( "bool", column (Bool False) BoolCol )
            , ( "time", column (String "") TimeCol )
            , ( "date", column (String "") DateCol )
            , ( "status"
              , { status
                    | options =
                        [ String "draft", String "published", String "archived" ]
                }
              )
            ]
    , referencedBy = []
    }
