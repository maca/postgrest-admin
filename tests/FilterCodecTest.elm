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
        , describe "and= groups keep every condition"
            [ test "merges a range and keeps the rest" <|
                \_ ->
                    Filter.parse table "and=(int.gte.1,int.lte.10,text.eq.x)"
                        |> Expect.equal
                            [ Filter.int2 "int" Filter.between "1" "10"
                            , Filter.text "text" Filter.equals "x"
                            ]
            , test "keeps same-column conditions" <|
                \_ ->
                    Filter.parse table "and=(text.eq.a,text.eq.b)"
                        |> Expect.equal
                            [ Filter.text "text" Filter.equals "a"
                            , Filter.text "text" Filter.equals "b"
                            ]
            , test "keeps unmergeable conditions" <|
                \_ ->
                    Filter.parse table "and=(int.gte.1,int.gt.10)"
                        |> Expect.equal
                            [ Filter.init "int"
                                (Filter.greaterOrEqual (Operand.int "1"))
                            , Filter.init "int"
                                (Filter.greaterThan (Operand.int "10"))
                            ]
            , test "keeps pattern conditions" <|
                \_ ->
                    Filter.parse table "and=(text.ilike.*a*,text.ilike.*b*)"
                        |> Expect.equal
                            [ Filter.text "text" Filter.contains "a"
                            , Filter.text "text" Filter.contains "b"
                            ]
            ]
        , describe "parse-back for the filter vocabulary"
            [ test "unquoted in-list items" <|
                \_ ->
                    Filter.parse table "status=in.(draft,published)"
                        |> List.map Filter.toQueryString
                        |> Expect.equal [ "status=in.(\"draft\",\"published\")" ]
            , test "single unquoted in-list item" <|
                \_ ->
                    Filter.parse table "status=in.(draft)"
                        |> List.map Filter.toQueryString
                        |> Expect.equal [ "status=in.(\"draft\")" ]
            , test "enum columns keep is.null" <|
                \_ ->
                    Filter.parse table "status=is.null"
                        |> List.map Filter.toQueryString
                        |> Expect.equal [ "status=is.null" ]
            , test "enum columns drop other operators" <|
                \_ ->
                    Filter.parse table "status=eq.x" |> Expect.equal []
            , test "blank operands are not filters" <|
                \_ ->
                    Filter.parse table "text=eq." |> Expect.equal []
            , test "empty in-lists are not filters" <|
                \_ ->
                    Filter.parse table "status=in.()" |> Expect.equal []
            , test "a blank condition drops only itself" <|
                \_ ->
                    Filter.parse table "and=(text.eq.,int.gte.1)"
                        |> Expect.equal
                            [ Filter.init "int"
                                (Filter.greaterOrEqual (Operand.int "1"))
                            ]
            ]
        , describe "unrepresentable patterns" <|
            List.map drops
                [ "text=ilike.*a*b*"
                , "text=ilike.a"
                , "text=ilike."
                , "text=ilike.%5C%2A"
                , "text=ilike.*100%25*"
                , "text=ilike.*a_b*"
                ]
                ++ [ test "a * in the value is not serialized" <|
                        \_ ->
                            Filter.text "text" Filter.contains "a*b"
                                |> Filter.toQueryString
                                |> Expect.equal ""
                   ]
        ]


drops : String -> Test
drops query =
    test query <|
        \_ ->
            Filter.parse table query |> Expect.equal []


roundTrip : Filter -> Test
roundTrip filter =
    test (Filter.toQueryString filter) <|
        \_ ->
            Filter.parse table (Filter.toQueryString filter)
                |> Expect.equal [ filter ]


canonical : String -> Test
canonical query =
    test query <|
        \_ ->
            let
                serialize =
                    List.map Filter.toQueryString >> String.join "&"

                parseAll =
                    String.split "&" >> List.concatMap (Filter.parse table)

                once =
                    parseAll query

                twice =
                    serialize once |> parseAll
            in
            serialize once |> Expect.equal (serialize twice)


cases : List Filter
cases =
    [ Filter.text "text" Filter.equals "bar baz"
    , Filter.text "text" Filter.equals "café ñ"
    , Filter.text "text" Filter.equals "a=b"
    , Filter.text "text" Filter.equals "a&b"
    , Filter.text "text" Filter.equals "a+b"
    , Filter.text "text" Filter.equals "0.5"
    , Filter.text "text" Filter.equals "a,b"
    , Filter.text "text" Filter.equals "a)b"
    , Filter.text "text" Filter.equals "100%"
    , Filter.text "text" Filter.equals "a%20b"
    , Filter.text "text" Filter.equals "\"hello\""
    , Filter.text "text" Filter.equals "say \"hi\" now"
    , Filter.text "text" Filter.contains "bar baz"
    , Filter.text "text" Filter.contains "100%"
    , Filter.text "text" Filter.contains "a_b"
    , Filter.text "text" Filter.contains "a,b"
    , Filter.text "text" Filter.contains "back\\slash"
    , Filter.text "text" Filter.startsWith "50%"
    , Filter.text "text" Filter.endsWith "a%b"
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
    , "text=ilike.*100%5C%25*"
    , "text=ilike.%5C%25%5C%5F*"
    , "and=(int.gte.1,int.lte.10,text.eq.x)"
    , "status=in.(draft,published)"
    , "status=in.(draft)"
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
