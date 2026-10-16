module FilterTest exposing (suite)

import Dict
import Expect
import Internal.Filter as Filter exposing (..)
import Internal.Schema exposing (ColumnType(..), Table, Value(..))
import Test exposing (..)


suite : Test
suite =
    describe "Filter"
        [ test "codec" <|
            \_ ->
                Expect.all
                    ([ "bool=is.true"
                     , "bool=is.false"
                     , "bool=is.null"
                     , "text=eq.bar%20baz"
                     , "text=ilike.*bar%20baz*"
                     , "text=ilike.bar%20baz*"
                     , "text=ilike.*bar%20baz"
                     , "int=lt.1"
                     , "int=gt.1"
                     , "float=lt.1.1"
                     , "float=gt.1.1"
                     , "date=gt.now"
                     , "date=lt.now"
                     , "time=gt.now"
                     , "time=lt.now"
                     , "int=lte.1"
                     , "int=gte.1"
                     , "float=lte.1.1"
                     , "float=gte.1.1"
                     , "date=gte.now"
                     , "date=lte.now"
                     , "time=gte.now"
                     , "time=lte.now"
                     , "and=(int.gte.1,int.lte.2)"
                     , "and=(float.gte.1.1,float.lte.2.2)"
                     , "and=(date.gte.1800-01-01,date.lte.2021-01-02)"
                     , "and=(time.gte.2021-01-02T01%3A10,time.lte.2027-01-03T01%3A00)"
                     ]
                        |> List.map
                            (\q ->
                                parse table q
                                    |> List.map Filter.toQueryString
                                    |> Expect.equal [ q ]
                                    |> always
                            )
                    )
                    ()
        , describe "parse and construct"
            [ test "is.true" <|
                \_ ->
                    parse table "bool=is.true"
                        |> Expect.equal [ isTrue "bool" ]
            , test "is.false" <|
                \_ ->
                    parse table "bool=is.false"
                        |> Expect.equal [ isFalse "bool" ]
            , test "is.null" <|
                \_ ->
                    parse table "bool=is.null"
                        |> Expect.equal [ isNull "bool" ]
            , test "equals" <|
                \_ ->
                    parse table "text=eq.bar%20baz"
                        |> Expect.equal [ text "text" equals "bar baz" ]
            , test "contains" <|
                \_ ->
                    Filter.parse table "text=ilike.*bar%20baz*"
                        |> Expect.equal [ text "text" contains "bar baz" ]
            , test "starts with" <|
                \_ ->
                    Filter.parse table "text=ilike.bar%20baz*"
                        |> Expect.equal
                            [ text "text" startsWith "bar baz" ]
            , test "ends with" <|
                \_ ->
                    Filter.parse table "text=ilike.*bar%20baz"
                        |> Expect.equal [ text "text" endsWith "bar baz" ]
            , test "lesser than" <|
                \_ ->
                    Filter.parse table "float=lt.1.1"
                        |> Expect.equal [ float "float" lesserThan "1.1" ]
            , test "greater than" <|
                \_ ->
                    Filter.parse table "float=gt.1.1"
                        |> Expect.equal
                            [ float "float" greaterThan "1.1" ]
            , test "lesser or equal to" <|
                \_ ->
                    Filter.parse table "float=lte.1.1"
                        |> Expect.equal
                            [ float "float" lesserOrEqual "1.1" ]
            , test "greater or equal to" <|
                \_ ->
                    Filter.parse table "float=gte.1.1"
                        |> Expect.equal
                            [ float "float" greaterOrEqual "1.1" ]
            , test "between int" <|
                \_ ->
                    Filter.parse table
                        "and=(int.gte.1,int.lte.10)"
                        |> Expect.equal
                            [ int2 "int" between "1" "10" ]
            , test "between float" <|
                \_ ->
                    Filter.parse table
                        "and=(float.gte.1.1,float.lte.10.1)"
                        |> Expect.equal
                            [ float2 "float" between "1.1" "10.1" ]
            , test "between date" <|
                \_ ->
                    Filter.parse table
                        "and=(date.gte.1800-01-01,date.lte.2021-01-02)"
                        |> Expect.equal
                            [
                                date2 "date" between "1800-01-01" "2021-01-02"
                             ]
            , test "between time" <|
                \_ ->
                    Filter.parse table
                        "and=(time.gte.1800-01-01,time.lte.2021-01-02)"
                        |> Expect.equal
                            [
                                time2 "time" between "1800-01-01" "2021-01-02"
                             ]
            , test "between time inverted" <|
                \_ ->
                    Filter.parse table
                        "and=(time.lte.2021-01-02,time.gte.1800-01-01)"
                        |> Expect.equal
                            [
                                time2 "time" between "1800-01-01" "2021-01-02"
                             ]
            , test "time in the future" <|
                \_ ->
                    Filter.parse table "time=gt.now"
                        |> Expect.equal [ isInTheFuture "time" ]
            , test "time in the past" <|
                \_ ->
                    Filter.parse table "time=lt.now"
                        |> Expect.equal
                            [ isInThePast "time" ]
            , test "date in the future" <|
                \_ ->
                    Filter.parse table "date=gt.now"
                        |> Expect.equal [ isInTheFuture "date" ]
            , test "date in the past" <|
                \_ ->
                    Filter.parse table "date=lt.now"
                        |> Expect.equal
                            [ isInThePast "date" ]
            , test "time is in date" <|
                \_ ->
                    let
                        times =
                            String.join ","
                                [ "time.gte.2021-01-01T00%3A00"
                                , "time.lt.2021-01-02T00%3A00"
                                ]

                        query =
                            "and=(" ++ times ++ ")"
                    in
                    Filter.parse table query
                        |> Expect.equal
                            [ time "time" inDate "2021-01-01" ]
            , test "time is in date inverted" <|
                \_ ->
                    let
                        times =
                            String.join ","
                                [ "time.lt.2021-01-02T00%3A00"
                                , "time.gte.2021-01-01T00%3A00"
                                ]

                        query =
                            "and=(" ++ times ++ ")"
                    in
                    Filter.parse table query
                        |> Expect.equal
                            [ time "time" inDate "2021-01-01" ]
            ]
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
    in
    { name = "test_table"
    , columns =
        Dict.fromList
            [ ( "float", column (Float 0.0) FloatCol )
            , ( "int", column (Int 0) IntegerCol )
            , ( "string", column (String "") StringCol )
            , ( "text", column (String "") TextCol )
            , ( "enum", column (String "") StringCol )
            , ( "bool", column (Bool False) BoolCol )
            , ( "time", column (String "") TimeCol )
            , ( "date", column (String "") DateCol )
            ]
    , referencedBy = []
    }
