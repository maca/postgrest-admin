module Internal.PageForm exposing
    ( Msg
    , PageForm
    , errors
    , init
    , onLogin
    , toId
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, a, button, fieldset, h1, section, text)
import Html.Attributes exposing (autocomplete, class, disabled, href, novalidate)
import Html.Events exposing (onSubmit)
import Internal.Cmd as AppCmd
import Internal.Field as Field
import Internal.Input as Input exposing (Input)
import Internal.Schema exposing (Table)
import Internal.Value exposing (Value(..))
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Record as Record exposing (Record)
import String.Extra as String
import Url
import Url.Builder as Url
import Utils.Task exposing (Error(..))


type Msg
    = LoggedIn Client
    | Fetched (Result Error Record)
    | Saved (Result Error Record)
    | InputChanged Input.Msg
    | Submitted


type alias Inputs =
    Dict String Input


type PageForm
    = PageForm
        { client : Client
        , key : Nav.Key
        , inputs : Inputs
        , table : Table
        , fieldNames : List String
        , id : Maybe String
        }


init :
    { client : Client
    , fieldNames : List String
    , id : Maybe String
    , table : Table
    }
    -> Nav.Key
    -> ( PageForm, AppCmd.Cmd Msg )
init { client, fieldNames, id, table } key =
    let
        pageForm =
            PageForm
                { client = client
                , key = key
                , inputs =
                    Maybe.map (always Dict.empty) id
                        |> Maybe.withDefault
                            (recordToInputs (Record.fromTable table))
                , table = table
                , fieldNames = fieldNames
                , id = id
                }
    in
    ( pageForm, fetchRecord pageForm )


fetchRecord : PageForm -> AppCmd.Cmd Msg
fetchRecord (PageForm { id, client, table }) =
    case id of
        Just recordId ->
            Client.fetchRecord
                { client = client
                , table = table
                , id = recordId
                , expect = Client.expectRecord Fetched table
                }

        Nothing ->
            AppCmd.none


onLogin : Client -> Msg
onLogin =
    LoggedIn



-- Update


update : Msg -> PageForm -> ( PageForm, AppCmd.Cmd Msg )
update msg (PageForm params) =
    case msg of
        LoggedIn client ->
            let
                pageForm =
                    PageForm { params | client = client }
            in
            ( pageForm
            , if params.inputs == Dict.empty then
                fetchRecord pageForm

              else
                AppCmd.none
            )

        Fetched (Ok record) ->
            ( PageForm { params | inputs = recordToInputs record }
            , AppCmd.none
            )

        Fetched (Err err) ->
            ( PageForm params
            , Notification.error (Utils.Task.errorToString err)
            )

        Saved (Ok record) ->
            ( PageForm params
            , AppCmd.batch
                [ Url.absolute [ params.table.name, Maybe.withDefault "" (Record.id record) ] []
                    |> Nav.pushUrl params.key
                    |> AppCmd.wrap
                , Notification.confirm "The record was saved"
                ]
            )

        Saved (Err err) ->
            ( PageForm params
            , Notification.error (Utils.Task.errorToString err)
            )

        InputChanged inputMsg ->
            let
                ( inputs, cmd ) =
                    Input.update params.client inputMsg params.inputs
            in
            ( PageForm { params | inputs = inputs }
            , AppCmd.batch [ AppCmd.map InputChanged cmd, Notification.dismiss ]
            )

        Submitted ->
            ( PageForm params
            , Client.saveRecord
                { client = params.client
                , record = toFormFields (PageForm params)
                , id = params.id
                , expect = Client.expectRecord Saved params.table
                }
            )



-- UTILS


toRecord : PageForm -> Record
toRecord (PageForm { table, inputs }) =
    { table = table
    , fields = Dict.map (\_ input -> Input.toField input) inputs
    }


toFormFields : PageForm -> Record
toFormFields form =
    toRecord (filterInputs form)


formInputs : PageForm -> List ( String, Input )
formInputs (PageForm { inputs }) =
    Dict.toList inputs |> List.sortWith sortInputs


filterInputs : PageForm -> PageForm
filterInputs (PageForm params) =
    PageForm
        { params
            | inputs =
                params.inputs
                    |> Dict.filter (inputIsEditable params.fieldNames)
        }


inputIsEditable : List String -> String -> Input -> Bool
inputIsEditable fieldNames name input =
    let
        field =
            Input.toField input
    in
    (List.isEmpty fieldNames && not (Field.isPrimaryKey field))
        || List.member name fieldNames


recordToInputs : Record -> Inputs
recordToInputs record =
    record.fields |> Dict.map (\_ input -> Input.fromField input)


changed : PageForm -> Bool
changed (PageForm { inputs }) =
    Dict.values inputs |> List.any (.changed << Input.toField)


errors : PageForm -> Dict String (Maybe String)
errors record =
    toFormFields record |> Record.errors


hasErrors : PageForm -> Bool
hasErrors record =
    toFormFields record |> Record.hasErrors


toId : PageForm -> Maybe String
toId (PageForm params) =
    params.id



-- VIEW


view : PageForm -> Html Msg
view ((PageForm { table, id }) as form) =
    let
        fields =
            filterInputs form
                |> formInputs
                |> List.map
                    (\( name, input ) ->
                        Input.view name input |> Html.map InputChanged
                    )
    in
    section
        [ class "resource-form" ]
        [ h1
            []
            [ text (String.humanize table.name ++ " - ")
            , case
                Maybe.map2 Tuple.pair
                    (Record.label (toRecord form))
                    id
              of
                Just ( resourceLabel, resourceId ) ->
                    a
                        [ href (Url.absolute [ table.name, resourceId ] [])
                        ]
                        [ text resourceLabel ]

                Nothing ->
                    text "New"
            ]
        , Html.form
            [ autocomplete False
            , onSubmit Submitted
            , novalidate True
            ]
            [ fieldset [] fields
            , fieldset []
                [ button
                    [ disabled (not (changed form) || hasErrors form) ]
                    [ text "Save" ]
                ]
            ]
        ]



-- Sort


sortInputs : ( String, Input ) -> ( String, Input ) -> Order
sortInputs ( name, input ) ( name_, input_ ) =
    Field.compareTuple
        ( name, Input.toField input )
        ( name_, Input.toField input_ )
