module Internal.Search exposing
    ( Msg
    , Search
    , init
    , isApplyMsg
    , isBlank
    , toPGQuery
    , update
    , view
    )

import Array exposing (Array)
import Array.Extra as Array
import Basics.Extra exposing (flip)
import Dict
import Html exposing (Attribute, Html, button, div, i, label, option, span)
import Html.Attributes
    exposing
        ( checked
        , class
        , for
        , hidden
        , id
        , selected
        , step
        , title
        , type_
        , value
        )
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Internal.Filter as Filter exposing (Filter)
import Internal.Filter.Operand as Operand
    exposing
        ( Enum
        , Operand
        , date
        , float
        , int
        , text
        , time
        )
import Internal.Filter.Operation as Operation exposing (Operation(..))
import Internal.Schema exposing (ColumnType(..), Table)
import Json.Decode as Decode
import Postgrest.Client as PG
import Set
import String.Extra as String


type Msg
    = UpdateFilter Int Filter
    | AddFilter AddPosition
    | RemoveFilter Int
    | Apply


type AddPosition
    = Prepend
    | Append


type alias OperationConst =
    String -> String -> Operation


type alias Options =
    List ( String, OperationConst )


type alias Search =
    { table : Table
    , filters : Array Filter
    }


init : Table -> String -> Search
init table query =
    { table = table
    , filters =
        String.split "&" query
            |> List.filterMap (Filter.parse table)
            |> Array.fromList
    }


isApplyMsg : Msg -> Bool
isApplyMsg msg =
    case msg of
        Apply ->
            True

        _ ->
            False


isBlank : Search -> Bool
isBlank { filters } =
    Array.isEmpty filters


toPGQuery : Search -> List PG.Param
toPGQuery { filters } =
    Array.toList filters |> List.filterMap Filter.toPGQuery


update : Msg -> Search -> ( Search, Cmd Msg )
update msg search =
    case msg of
        UpdateFilter idx filter ->
            ( { search | filters = Array.set idx filter search.filters }
            , Cmd.none
            )

        AddFilter position ->
            let
                mfilter =
                    search.table.columns
                        |> Dict.toList
                        |> List.head
                        |> Maybe.andThen (\( n, c ) -> Filter.fromColumn n c)
            in
            ( { search
                | filters =
                    case mfilter of
                        Just filter ->
                            if position == Prepend then
                                search.filters
                                    |> Array.append (Array.fromList [ filter ])

                            else
                                Array.push filter search.filters

                        Nothing ->
                            search.filters
              }
            , Cmd.none
            )

        RemoveFilter idx ->
            ( { search | filters = Array.removeAt idx search.filters }
            , Cmd.none
            )

        Apply ->
            ( search, Cmd.none )



-- View


view : Bool -> Search -> Html Msg
view open { table, filters } =
    div [ class "search", hidden <| not open ]
        [ div [ class "actions" ] [ buttonAdd Prepend ]
        , Keyed.node "div"
            [ class "filters" ]
            (Array.indexedMap
                (\idx filter ->
                    ( String.fromInt idx
                    , Lazy.lazy3 viewFilter table idx filter
                    )
                )
                filters
                |> Array.toList
            )
        , if Array.isEmpty filters then
            Html.text ""

          else
            div [ class "actions" ] [ buttonAdd Append ]
        ]


buttonAdd : AddPosition -> Html Msg
buttonAdd tagger =
    button
        [ onClick (AddFilter tagger)
        , class "button-clear"
        , class "add-filter"
        ]
        [ Html.text "Add filter", i [ class "icono-plus" ] [] ]


viewFilter : Table -> Int -> Filter -> Html Msg
viewFilter table idx filter =
    let
        name =
            Filter.columnName filter

        inputs kind content =
            div
                [ class "filter"
                , class kind
                ]
                [ div [ class "filter-inputs" ]
                    (fieldSelect table idx filter :: content)
                , button
                    [ class "button-clear"
                    , onClick <| RemoveFilter idx
                    , title "Remove filter"
                    , class "filter-remove"
                    ]
                    [ i [ class "icono-cross" ] [] ]
                ]
    in
    case Dict.get name table.columns of
        Just column ->
            let
                op =
                    Filter.operation filter
            in
            case column.columnType of
                StringCol ->
                    let
                        hasEnumOptions =
                            not (List.isEmpty column.options)
                    in
                    if hasEnumOptions then
                        enumInputs column.required name idx op
                            |> inputs "enum"

                    else
                        textFilterInputs column.required name idx op
                            |> inputs "text"

                TextCol ->
                    textFilterInputs column.required name idx op
                        |> inputs "text"

                IntegerCol ->
                    intFilterInputs column.required name idx op
                        |> inputs "number"

                FloatCol ->
                    floatFilterInputs column.required name idx op
                        |> inputs "number"

                BoolCol ->
                    boolFilterInputs column.required name idx op
                        |> inputs "bool"

                TimeWithoutTimezoneCol ->
                    timeFilterInputs column.required name idx op
                        |> inputs "time"

                TimeCol ->
                    timeFilterInputs column.required name idx op
                        |> inputs "time"

                TimestampWithoutTimezomeCol ->
                    timeFilterInputs column.required name idx op
                        |> inputs "time"

                TimestampCol ->
                    timeFilterInputs column.required name idx op
                        |> inputs "time"

                DateCol ->
                    dateFilterInputs column.required name idx op
                        |> inputs "time"

                _ ->
                    Html.text ""

        Nothing ->
            Html.text ""


fieldSelect : Table -> Int -> Filter -> Html Msg
fieldSelect table idx filter =
    let
        op =
            Filter.operation filter

        makeFilter selection =
            case defaultFilter selection table of
                Just filter_ ->
                    if
                        Operation.toString (Filter.operation filter_)
                            == Operation.toString op
                    then
                        Filter.init (Filter.columnName filter_) op

                    else
                        filter_

                Nothing ->
                    filter

        makeOption s =
            option
                [ value s, selected (s == Filter.columnName filter) ]
                [ Html.text <| String.humanize s ]
    in
    Html.select
        [ onInput (makeFilter >> UpdateFilter idx) ]
        (Dict.toList table.columns
            |> List.filterMap
                (\( s, column ) ->
                    Filter.fromColumn s column
                        |> Maybe.map (always <| makeOption s)
                )
        )


textFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
textFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.equals text
                , map Filter.contains text
                , map Filter.startsWith text
                , map Filter.endsWith text
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


intFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
intFilterInputs required name idx op =
    [ select (numberOptions int ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


floatFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
floatFilterInputs required name idx op =
    [ select (numberOptions float ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


dateFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
dateFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.equals date
                , dropBoth (IsInTheFuture <| Just op)
                , dropBoth (IsInThePast <| Just op)
                , map Filter.lesserOrEqual date
                , map Filter.greaterOrEqual date
                , map Filter.lesserThan date
                , map Filter.greaterThan date
                , map2 Filter.between date
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


timeFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
timeFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.inDate date
                , dropBoth (IsInTheFuture <| Just op)
                , dropBoth (IsInThePast <| Just op)
                , map Filter.lesserOrEqual time
                , map Filter.greaterOrEqual time
                , map Filter.lesserThan time
                , map Filter.greaterThan time
                , map2 Filter.between time
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


boolFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
boolFilterInputs required name idx op =
    let
        options =
            List.map (dropBoth >> operationOption) [ IsTrue, IsFalse ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


enumInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
enumInputs required name idx op =
    let
        options enum =
            List.map operationOption
                [ map OneOf (always enum)
                , map NoneOf (always enum)
                ]

        inputs makeOp enum =
            [ select (options enum ++ nullOption required op) op
                |> Html.map (Filter.init name >> UpdateFilter idx)
            , Operand.choices enum
                |> List.map (checkbox makeOp name idx enum)
                |> div [ class "checkboxes" ]
            ]
    in
    case op of
        OneOf enum ->
            inputs (map OneOf (flip choose enum)) enum

        NoneOf enum ->
            inputs (map NoneOf (flip choose enum)) enum

        IsNull (Just op_) ->
            case op_ of
                OneOf enum ->
                    [ select (options enum ++ nullOption required op_) op
                        |> Html.map (Filter.init name >> UpdateFilter idx)
                    ]

                NoneOf enum ->
                    [ select (options enum ++ nullOption required op_) op
                        |> Html.map (Filter.init name >> UpdateFilter idx)
                    ]

                _ ->
                    []

        _ ->
            []


choose : String -> Enum -> Enum
choose choice enum =
    let
        choices =
            Operand.choices enum

        chosen =
            Operand.chosen enum
    in
    if Set.member choice chosen then
        Operand.enum choices (Set.remove choice chosen)

    else
        Operand.enum choices (Set.insert choice chosen)


select : Options -> Operation -> Html Operation
select options op =
    let
        opSelect a b =
            let
                makeOption ( s, _ ) =
                    option
                        [ selected (Operation.toString op == s) ]
                        [ Html.text s ]

                optionSelected selection =
                    let
                        makeOperation =
                            Dict.fromList options
                                |> Dict.get selection
                                |> Maybe.withDefault
                                    (dropBoth <| IsNull <| Just op)
                    in
                    makeOperation a b
            in
            Html.select
                [ onInput optionSelected ]
                (List.map makeOption options)
    in
    case Operation.values op of
        [ a, b ] ->
            opSelect a b

        [ a ] ->
            opSelect a ""

        _ ->
            opSelect "" ""


input : String -> Int -> Operation -> Html Msg
input name idx op =
    let
        input_ operationCons operand attrs =
            if text (Operand.value operand) == operand then
                textInput (type_ "text" :: attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if int (Operand.value operand) == operand then
                textInput (type_ "number" :: step "1" :: attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if float (Operand.value operand) == operand then
                textInput (type_ "number" :: step "0.01" :: attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if date (Operand.value operand) == operand then
                textInput (type_ "date" :: attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if time (Operand.value operand) == operand then
                textInput (type_ "datetime-local" :: attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else
                Html.text ""
    in
    case op of
        Equals a ->
            input_ Equals a []

        Contains a ->
            input_ (Contains << noStars) a []

        StartsWith a ->
            input_ (StartsWith << noStars) a []

        EndsWith a ->
            input_ (EndsWith << noStars) a []

        LesserThan a ->
            input_ LesserThan a []

        GreaterThan a ->
            input_ GreaterThan a []

        LesserOrEqual a ->
            input_ LesserOrEqual a []

        GreaterOrEqual a ->
            input_ GreaterOrEqual a []

        Between a b ->
            div []
                [ input_ (flip Between b) a []
                , span [] [ Html.text "and" ]
                , input_ (Between a) b []
                ]

        InDate a ->
            input_ InDate a [ type_ "date" ]

        OneOf _ ->
            Html.text ""

        NoneOf _ ->
            Html.text ""

        IsTrue ->
            Html.text ""

        IsFalse ->
            Html.text ""

        IsNull _ ->
            Html.text ""

        IsInTheFuture _ ->
            Html.text ""

        IsInThePast _ ->
            Html.text ""


-- A literal `*` cannot be expressed in a PostgREST pattern (it is the
-- wildcard and escaping does not survive the server's rewrite), so pattern
-- operands never take one.
noStars : Operand -> Operand
noStars operand =
    Operand.constructor operand (String.replace "*" "" (Operand.value operand))


textInput : List (Attribute Msg) -> (Operand -> Msg) -> Operand -> Html Msg
textInput attributes makeOperation operand =
    Html.input
        ([ onInput (makeOperation << Operand.constructor operand)
         , onEnter Apply
         , value <| Operand.rawValue operand
         ]
            ++ attributes
        )
        []


checkbox : OperationConst -> String -> Int -> Enum -> String -> Html Msg
checkbox makeOp name idx enum choice =
    let
        inputId =
            String.fromInt idx |> (++) choice
    in
    label
        [ for inputId ]
        [ Html.input
            [ id inputId
            , value choice
            , onInput (\s -> makeOp s "" |> Filter.init name |> UpdateFilter idx)
            , Html.Attributes.type_ "checkbox"
            , checked <| Set.member choice (Operand.chosen enum)
            ]
            []
        , Html.text <| String.humanize choice
        ]


nullOption : Bool -> Operation -> List ( String, OperationConst )
nullOption required op =
    if required then
        []

    else
        [ operationOption <| dropBoth (IsNull <| Just op) ]


operationOption : OperationConst -> ( String, OperationConst )
operationOption cons =
    ( Operation.toString (cons "" ""), cons )


numberOptions : (String -> Operand) -> List ( String, OperationConst )
numberOptions cons =
    List.map operationOption
        [ map Filter.equals cons
        , map Filter.lesserOrEqual cons
        , map Filter.greaterOrEqual cons
        , map Filter.lesserThan cons
        , map Filter.greaterThan cons
        , map2 Filter.between cons
        ]


defaultFilter : String -> Table -> Maybe Filter
defaultFilter colName table =
    Dict.get colName table.columns
        |> Maybe.andThen (Filter.fromColumn colName)



-- Utils


dropBoth : c -> a -> b -> c
dropBoth c _ _ =
    c


map : (op -> c) -> (s -> op) -> s -> s -> c
map cons mfun a _ =
    cons (mfun a)


map2 : (op -> op -> c) -> (s -> op) -> s -> s -> c
map2 cons mfun a b =
    cons (mfun a) (mfun b)


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)
