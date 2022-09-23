module Internal.Input exposing
    ( Input(..)
    , Msg
    , fromField
    , isRequired
    , toField
    , toHtml
    , toValue
    , update
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (Html, a, div, label, option, p, text)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , classList
        , for
        , href
        , id
        , list
        , required
        , selected
        , target
        )
import Html.Events exposing (onInput)
import Internal.Cmd as AppCmd
import Internal.Field as Field exposing (Field)
import Internal.Http exposing (Error(..))
import Internal.Record as Record exposing (Record)
import Internal.Schema exposing (Constraint(..), ForeignKeyParams)
import Internal.Value as Value exposing (Value(..))
import Json.Decode as Decode
import Maybe.Extra as Maybe
import PostgRestAdmin.Client as Client exposing (Client, Collection)
import Postgrest.Client as PG
import String.Extra as String
import Url.Builder as Url


type alias Autocomplete =
    { results : List Record
    , userInput : String
    , blocked : Bool
    }


type Msg
    = Changed ( String, Input ) String
    | ListingFetched String (Result Error (Collection Record))


type Input
    = Text Field
    | TextArea Field
    | Select Field (List String)
    | Number Field
    | Checkbox Field
    | DateTime Field
    | Date Field
    | Association Field Autocomplete
    | Blank Field


update :
    Client
    -> Msg
    -> Dict String Input
    -> ( Dict String Input, AppCmd.Cmd Msg )
update client msg record =
    case msg of
        Changed ( name, Association field autocomplete ) "" ->
            let
                input =
                    Association (Field.updateWithString "" field)
                        { autocomplete
                            | userInput = ""
                            , blocked = False
                        }
            in
            ( Dict.insert name input record
            , AppCmd.none
            )

        Changed ( name, Association field autocomplete ) userInput ->
            case field.constraint of
                ForeignKey params ->
                    let
                        resource =
                            findRecord params autocomplete userInput

                        value =
                            Maybe.andThen Record.primaryKey resource
                                |> Maybe.map .value
                                |> Maybe.withDefault field.value

                        label =
                            Maybe.andThen (resourceLabel params) resource

                        fkey =
                            ForeignKey { params | label = label }
                    in
                    ( Dict.insert name
                        (Association
                            (Field.update value { field | constraint = fkey })
                            { autocomplete
                                | userInput = Maybe.withDefault userInput label
                                , blocked = False
                            }
                        )
                        record
                    , fetchRecords client name params userInput
                    )

                _ ->
                    ( record, AppCmd.none )

        Changed ( name, input ) userInput ->
            let
                updateFun =
                    Field.updateWithString userInput
            in
            ( Dict.insert name (mapField updateFun input) record
            , AppCmd.none
            )

        ListingFetched name (Ok { records }) ->
            case Dict.get name record of
                Just (Association field params) ->
                    let
                        input =
                            Association field <|
                                if List.isEmpty records then
                                    { params
                                        | results = []
                                        , blocked = True
                                    }

                                else
                                    { params | results = records }
                    in
                    ( Dict.insert name input record, AppCmd.none )

                _ ->
                    ( record, AppCmd.none )

        ListingFetched _ (Err _) ->
            ( record, AppCmd.none )


mapField : (Field -> Field) -> Input -> Input
mapField updateField input =
    case input of
        Text field ->
            Text (updateField field)

        TextArea field ->
            TextArea (updateField field)

        Select field opts ->
            Select (updateField field) opts

        Number field ->
            Number (updateField field)

        Checkbox field ->
            Checkbox (updateField field)

        DateTime field ->
            DateTime (updateField field)

        Date field ->
            Date (updateField field)

        Association field autocomplete ->
            Association (updateField field) autocomplete

        Blank _ ->
            input


toField : Input -> Field
toField input =
    case input of
        Text field ->
            field

        TextArea field ->
            field

        Select field _ ->
            field

        Number field ->
            field

        Checkbox field ->
            field

        DateTime field ->
            field

        Date field ->
            field

        Association field _ ->
            field

        Blank field ->
            field


fromField : Field -> Input
fromField field =
    case field.constraint of
        PrimaryKey ->
            fromFieldWithValue field

        ForeignKey { label } ->
            Association field
                { userInput = Maybe.withDefault "" label
                , results = []
                , blocked = False
                }

        NoConstraint ->
            fromFieldWithValue field


fromFieldWithValue : Field -> Input
fromFieldWithValue field =
    case field.value of
        PString _ ->
            Text field

        PText _ ->
            TextArea field

        PEnum _ opts ->
            Select field opts

        PFloat _ ->
            Number field

        PInt _ ->
            Number field

        PBool _ ->
            Checkbox field

        PTime _ ->
            DateTime field

        PDate _ ->
            Date field

        PJson _ ->
            TextArea
                (Field.setValidation
                    (\value ->
                        Value.toString value
                            |> Maybe.andThen
                                (\v ->
                                    case Decode.decodeString Decode.value v of
                                        Ok _ ->
                                            Nothing

                                        Err err ->
                                            Just (Decode.errorToString err)
                                )
                    )
                    field
                )

        Unknown _ ->
            Blank field


toValue : Input -> Value
toValue input =
    .value <| toField input


isRequired : Input -> Bool
isRequired input =
    .required <| toField input


toError : Input -> Maybe String
toError input =
    .error <| toField input


toHtml : String -> Input -> Html Msg
toHtml name input =
    case input of
        Text { value } ->
            Value.toString value
                |> displayInput "text" input
                |> wrapInput input name

        TextArea { value } ->
            Value.toString value
                |> displayTextArea input
                |> wrapInput input name

        Select { value } opts ->
            Value.toString value
                |> displaySelect opts input
                |> wrapInput input name

        Number { value } ->
            Value.toString value
                |> displayInput "number" input
                |> wrapInput input name

        Checkbox { value } ->
            Value.isTrue value
                |> displayCheckbox input
                |> wrapInput input name

        DateTime { value } ->
            Value.toString value
                |> displayInput "datetime-local" input
                |> wrapInput input name

        Date { value } ->
            Value.toString value
                |> displayInput "date" input
                |> wrapInput input name

        Association field params ->
            displayAutocompleteInput params field input
                |> wrapInput input name

        Blank _ ->
            text ""


wrapInput : Input -> String -> (String -> Html Msg) -> Html Msg
wrapInput input name buildInput =
    div
        [ class "field"
        , classList
            [ ( "with-error", Maybe.isJust <| toError input )
            , ( "required", isRequired input )
            ]
        ]
        [ label [ for name ] [ text <| String.humanize name ]
        , buildInput name
        , displayError <| toError input
        ]


displayAutocompleteInput : Autocomplete -> Field -> Input -> String -> Html Msg
displayAutocompleteInput autocomplete field input name =
    case field.constraint of
        ForeignKey foreignKeyParams ->
            let
                prevLength =
                    String.length autocomplete.userInput

                inputCallback string =
                    Changed ( name, input ) <|
                        if
                            autocomplete.blocked
                                && (String.length string >= prevLength)
                        then
                            autocomplete.userInput

                        else
                            string
            in
            div [ class "autocomplete-input" ]
                [ Html.datalist
                    [ id foreignKeyParams.tableName ]
                    (List.map (autocompleteOption foreignKeyParams)
                        autocomplete.results
                    )
                , div
                    [ class "association-link" ]
                    [ associationLink foreignKeyParams field ]
                , Html.input
                    [ onInput inputCallback
                    , id name
                    , list foreignKeyParams.tableName
                    , required field.required
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.value autocomplete.userInput
                    ]
                    []
                ]

        _ ->
            text ""


associationLink : ForeignKeyParams -> Field -> Html Msg
associationLink params { value } =
    if Value.isNothing value then
        text "-"

    else
        let
            id =
                Value.toString value |> Maybe.withDefault ""
        in
        a
            [ href (Url.absolute [ params.tableName, id ] [])
            , target "_blank"
            ]
            [ text id ]


autocompleteOption : ForeignKeyParams -> Record -> Html Msg
autocompleteOption params resource =
    option [ Html.Attributes.value <| optionText params resource ] []


optionText : ForeignKeyParams -> Record -> String
optionText ({ primaryKeyName } as params) resource =
    let
        id =
            Record.fieldToString primaryKeyName resource
    in
    [ id, resourceLabel params resource ]
        |> List.filterMap identity
        |> String.join " - "


resourceLabel : ForeignKeyParams -> Record -> Maybe String
resourceLabel { labelColumnName } resource =
    Maybe.andThen (flip Record.fieldToString resource) labelColumnName


displayInput : String -> Input -> Maybe String -> String -> Html Msg
displayInput type_ input mstring name =
    Html.input
        [ onInput (Changed ( name, input ))
        , id name
        , required <| isRequired input
        , Html.Attributes.type_ type_
        , Html.Attributes.value <| Maybe.withDefault "" mstring
        ]
        []


displayTextArea : Input -> Maybe String -> String -> Html Msg
displayTextArea input mstring name =
    let
        content =
            Maybe.withDefault "" mstring
    in
    div
        [ class "grow-wrap"
        , attribute "data-replicated-value" content
        ]
        [ Html.textarea
            [ onInput (Changed ( name, input ))
            , id name
            , required <| isRequired input
            , Html.Attributes.value content
            ]
            []
        ]


displaySelect : List String -> Input -> Maybe String -> String -> Html Msg
displaySelect options input mstring name =
    let
        optionValues =
            if isRequired input then
                options

            else
                "" :: options

        selectOption opt =
            Html.option
                [ Html.Attributes.value opt, selected <| mstring == Just opt ]
                [ text <| String.humanize opt ]
    in
    Html.select
        [ onInput (Changed ( name, input ))
        , id name
        , required <| isRequired input
        , Html.Attributes.value <| Maybe.withDefault "" mstring
        ]
        (List.map selectOption optionValues)


displayCheckbox : Input -> Maybe Bool -> String -> Html Msg
displayCheckbox input mchecked name =
    Html.input
        [ onInput (Changed ( name, input ))
        , id name
        , Html.Attributes.type_ "checkbox"
        , Maybe.map checked mchecked |> Maybe.withDefault (class "")
        ]
        []


displayError : Maybe String -> Html Msg
displayError error =
    error
        |> Maybe.map (text >> List.singleton >> p [ class "error" ])
        |> Maybe.withDefault (text "")


fetchRecords : Client -> String -> ForeignKeyParams -> String -> AppCmd.Cmd Msg
fetchRecords client name { tableName, labelColumnName } userInput =
    case Client.getTable tableName client of
        Just table ->
            let
                selects =
                    labelColumnName
                        |> Maybe.map (flip (::) [ "id" ])
                        |> Maybe.withDefault [ "id" ]
                        |> PG.attributes

                idQuery =
                    String.toInt userInput
                        |> Maybe.map (PG.param "id" << PG.eq << PG.int)

                ilike column string =
                    PG.param column (PG.ilike ("*" ++ string ++ "*"))

                labelQuery =
                    Maybe.map2 ilike
                        labelColumnName
                        (String.nonBlank userInput)

                queries =
                    List.filterMap identity [ idQuery, labelQuery ]

                queryString =
                    PG.toQueryString
                        [ PG.select selects, PG.or queries, PG.limit 40 ]
            in
            Client.fetchRecordList
                { client = client
                , table = table
                , queryString = queryString
                , expect = ListingFetched name
                }

        Nothing ->
            AppCmd.none


findRecord : ForeignKeyParams -> Autocomplete -> String -> Maybe Record
findRecord foreignKeyParams autocomplete userInput =
    let
        userInputLower =
            String.toLower userInput

        findByOptionText resource =
            String.toLower (optionText foreignKeyParams resource)
                == userInputLower

        findByLabel resource =
            case resourceLabel foreignKeyParams resource of
                Just lab ->
                    String.toLower lab == userInputLower

                Nothing ->
                    False
    in
    List.filter findByOptionText autocomplete.results
        ++ List.filter findByLabel autocomplete.results
        |> List.head
