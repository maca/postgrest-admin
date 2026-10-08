module Internal.PageForm exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import FormToolkit.Error
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as FormValue
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Cmd as AppCmd exposing (AppCmd)
import Inflect
import Internal.Schema as Schema exposing (Table, Value(..))
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import Postgrest.Client as PG
import String as CoreString
import String.Extra as String
import Task
import Url.Builder as Url


type Msg
    = Fetched (Result Client.Error Decode.Value)
    | ParentLabelFetched (Result Client.Error String)
    | AutocompleteValuesFetched String (Result Client.Error AutocompleteOptions)
    | Saved (Result Client.Error String)
    | FormChanged (Field.Msg String)
    | Submitted


type alias Model =
    { client : Client
    , table : Table
    , mountPath : MountPath
    , key : Nav.Key
    , id : Maybe String
    , parent : Maybe { tableName : String, id : String }
    , parentLabel : Maybe String
    , form : Field.Field String
    , autocompleteValues : Dict String AutocompleteOptions
    , saveError : Maybe String
    }


type AutocompleteOptions
    = Local (List { id : String, label : String })
    | Remote


init :
    { client : Client
    , navKey : Nav.Key
    , mountPath : MountPath
    , table : Table
    , id : Maybe String
    , parent : Maybe { tableName : String, id : String }
    }
    -> ( Model, AppCmd Msg )
init { client, navKey, mountPath, table, id, parent } =
    ( { client = client
      , table = table
      , mountPath = mountPath
      , key = navKey
      , id = id
      , parent = parent
      , parentLabel = Nothing
      , form = buildForm { isEdit = id /= Nothing, table = table }
      , autocompleteValues = Dict.empty
      , saveError = Nothing
      }
        , AppCmd.batch
            (List.concat
                [ [ if Schema.tablePrimaryKeyColumns table |> List.isEmpty then
                        AppCmd.none

                    else
                        id
                            |> Maybe.map (fetch client table)
                            |> Maybe.withDefault AppCmd.none
                  , parent
                        |> Maybe.andThen (Schema.buildParentReference client.schema table)
                        |> Maybe.map
                            (Client.fetchParentLabel client
                                >> Task.attempt ParentLabelFetched
                                >> AppCmd.wrap
                            )
                        |> Maybe.withDefault AppCmd.none
                  ]
                , Schema.buildReferences table
                    |> Dict.toList
                    |> List.map (fetchAutcompleteValues client)
                ]
            )
        )


fetchAutcompleteValues : Client -> ( String, Schema.Reference ) -> AppCmd Msg
fetchAutcompleteValues client ( colName, ref ) =
    Client.count
        { client = client
        , path = Url.absolute [ ref.tableName ] []
        }
        |> Task.andThen
            (\count ->
                case ( count < 1000, ref.labelColumn ) of
                    ( True, Just labelColumnName ) ->
                        Client.jsonRequest
                            { client = client
                            , method = "GET"
                            , headers = []
                            , path =
                                String.concat
                                    [ "/"
                                    , ref.tableName
                                    , "?"
                                    , PG.toQueryString
                                        [ PG.select
                                            (PG.attributes
                                                [ ref.foreignKey
                                                , labelColumnName
                                                ]
                                            )
                                        ]
                                    ]
                            , body = Http.emptyBody
                            , decoder =
                                Decode.list
                                    (Decode.map2
                                        (\id label -> Just { id = id, label = label })
                                        (Decode.field ref.foreignKey
                                            (Decode.oneOf
                                                [ Decode.string
                                                , Decode.float |> Decode.map String.fromFloat
                                                ]
                                            )
                                        )
                                        (Decode.field labelColumnName Decode.string)
                                    )
                                    |> Decode.map (List.filterMap identity >> Local)
                            }

                    _ ->
                        Task.succeed Remote
            )
        |> Task.attempt (AutocompleteValuesFetched colName)
        |> AppCmd.wrap


fetch : Client -> Table -> String -> AppCmd Msg
fetch client table recordId =
    let
        selectedCols =
            PG.select
                (Dict.keys table.columns
                    |> List.filter (\name -> Schema.tablePrimaryKeyColumns table |> List.any (\( keyName, _ ) -> keyName == name) |> not)
                    |> List.map PG.attribute
                )

        queryString =
            PG.toQueryString
                (List.filterMap identity
                    [ Just selectedCols
                    , (Schema.recordIdFilters table recordId
                        |> List.map (\( column, value ) ->
                            PG.param column (PG.eq (PG.string value))
                        )
                        |> (\filters ->
                                if List.isEmpty filters then
                                    Nothing

                                else
                                    Just (PG.and filters)
                           )
                      )
                    , Just (PG.limit 1)
                    ]
                )
    in
    Client.jsonRequest
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ table.name ++ "?" ++ queryString
        , body = Http.emptyBody
        , decoder = Decode.value
        }
        |> Task.attempt Fetched
        |> AppCmd.wrap



-- Update


update : Msg -> Model -> ( Model, AppCmd Msg )
update msg model =
    case msg of
        Fetched (Ok response) ->
            case Field.updateValuesFromJson response model.form of
                Ok form ->
                    ( { model | form = form }
                    , AppCmd.none
                    )

                Err err ->
                    ( model
                    , AppCmd.error (FormToolkit.Error.toEnglish err)
                    )

        Fetched (Err err) ->
            ( model
            , AppCmd.clientError err
            )

        ParentLabelFetched (Ok label) ->
            ( { model | parentLabel = Just label, form = updateFormParent model label }
            , AppCmd.none
            )

        ParentLabelFetched (Err err) ->
            ( model
            , AppCmd.clientError err
            )

        AutocompleteValuesFetched colName (Ok values) ->
            ( { model
                | autocompleteValues =
                    Dict.insert colName values model.autocompleteValues
                , form = updateAutocompletOptions colName values model.form
              }
            , AppCmd.none
            )

        AutocompleteValuesFetched _ (Err err) ->
            ( model
            , AppCmd.clientError err
            )

        Saved (Ok id) ->
            ( { model | saveError = Nothing }
            , AppCmd.batch
                [ AppCmd.wrap
                    (Nav.pushUrl model.key
                        (Url.absolute [ model.table.name, id ] [])
                    )
                , AppCmd.confirm "The record was saved"
                ]
            )

        Saved (Err (Client.ConstraintViolation dbErr)) ->
            -- The database rejected the value for a specific reason (missing
            -- parent row, duplicate key, ...). Say which field and why,
            -- instead of surfacing the raw SQLSTATE-backed response body.
            ( { model | saveError = Just (Client.dbErrorMessage dbErr) }
            , AppCmd.none
            )

        Saved (Err err) ->
            ( { model | saveError = Nothing }
            , AppCmd.clientError err
            )

        FormChanged innerMsg ->
            ( { model | form = Field.update innerMsg model.form, saveError = Nothing }
            , AppCmd.dismiss
            )

        Submitted ->
            ( { model | form = Field.validate model.form |> Field.touch, saveError = Nothing }
            , case Parse.parse Parse.json model.form of
                Ok jsonBody ->
                    Client.saveRecord
                        { client = model.client
                        , id = model.id
                        , body = jsonBody
                        , table = model.table
                        , decoder =
                            case Schema.tablePrimaryKeyName model.table of
                                Just pkName ->
                                    -- The PK can come back as a JSON string,
                                    -- int, or float depending on the
                                    -- column's Postgres type; a decoder
                                    -- hardcoded to `Decode.string` fails on
                                    -- the very common numeric-PK case even
                                    -- though the save itself succeeded.
                                    Decode.field pkName idDecoder

                                Nothing ->
                                    Decode.fail "Coudn't figure the field id"
                        }
                        |> Task.attempt Saved
                        |> AppCmd.wrap

                Err _ ->
                    AppCmd.error "Please check the form errors"
            )


updateFormParent : Model -> String -> Field.Field String
updateFormParent model label =
    model.parent
        |> Maybe.map
            (\p ->
                case
                    Dict.filter (\_ v -> p.tableName == v.tableName)
                        (Schema.buildReferences model.table)
                        |> Dict.keys
                of
                    [ colName ] ->
                        (case Dict.get colName model.autocompleteValues of
                            Just (Local _) ->
                                model.form

                            _ ->
                                Field.updateWithId colName
                                    (Field.options [ autocompleteOption { id = p.id, label = label } ])
                                    model.form
                        )
                            |> Field.updateWithId colName
                                (Field.stringValue (autocompleteLabel { id = p.id, label = label }))

                    _ ->
                        model.form
            )
        |> Maybe.withDefault model.form


updateAutocompletOptions : String -> AutocompleteOptions -> Field.Field String -> Field.Field String
updateAutocompletOptions colName values =
    case values of
        Local list ->
            Field.updateWithId colName
                (Field.options (List.map autocompleteOption list))

        Remote ->
            identity


idDecoder : Decode.Decoder String
idDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.map CoreString.fromInt Decode.int
        , Decode.map CoreString.fromFloat Decode.float
        ]


autocompleteOption : { id : String, label : String } -> ( String, FormValue.Value )
autocompleteOption attrs =
    ( autocompleteLabel attrs, FormValue.string attrs.id )


autocompleteLabel : { id : String, label : String } -> String
autocompleteLabel { label, id } =
    label ++ " (" ++ id ++ ")"



-- VIEW


view : Model -> Html Msg
view ({ table, id, mountPath } as model) =
    Html.section
        [ Attrs.class "resource-form" ]
        [ MountPath.breadcrumbs mountPath
            table.name
            (case id of
                Just idStr ->
                    [ ( table.name, Nothing )
                    , ( idStr, Schema.label model.table )
                    , ( "edit", Nothing )
                    ]

                Nothing ->
                    case model.parent of
                        Just parent ->
                            [ ( parent.tableName, Nothing )
                            , ( parent.id, model.parentLabel )
                            , ( table.name, Nothing )
                            , ( "new", Nothing )
                            ]

                        Nothing ->
                            [ ( table.name, Nothing )
                            , ( "new", Nothing )
                            ]
            )
        , Html.h2
            []
            [ Html.text
                ((case id of
                    Just _ ->
                        "Edit "

                    Nothing ->
                        "New "
                 )
                    ++ (String.humanize table.name |> Inflect.toSingular)
                )
            ]
        , let
            formValid =
                Parse.parse (Parse.succeed True) model.form
                    |> Result.withDefault False
          in
          Html.form
            [ Attrs.autocomplete False
            , if formValid then
                Events.onSubmit Submitted

              else
                Attrs.class ""
            , Attrs.novalidate True
            ]
            [ case model.saveError of
                Just message ->
                    Html.div
                        [ Attrs.class "form-error-message" ]
                        [ Html.text message ]

                Nothing ->
                    Html.text ""
            , Field.toHtml FormChanged model.form
            , Html.fieldset []
                [ Html.a
                    [ Attrs.class "button button-clear"
                    , Attrs.href
                        (path mountPath <|
                            Url.absolute
                                (List.filterMap identity
                                    [ Maybe.map .tableName model.parent
                                    , Maybe.map .id model.parent
                                    , Just table.name
                                    , id
                                    ]
                                )
                                []
                        )
                    ]
                    [ Html.text "Cancel" ]
                , Html.button
                    [ Attrs.type_ "submit"
                    , Attrs.disabled (not formValid)
                    ]
                    [ Html.text "Save" ]
                ]
            ]
        ]


buildForm : { isEdit : Bool, table : Table } -> Field.Field String
buildForm { isEdit, table } =
    Field.group []
        (Dict.toList table.columns
            |> List.sortWith sortColumns
            |> List.filterMap (fieldFromColumn isEdit)
        )


{-| A primary key is only addressable through the URL on an edit form.

On a *create* form it has to stay editable whenever the database will not
fill it in by itself — a uuid/identity column with a `default` (or a
serial) can be left blank and omitted from the payload, but a plain
`integer PRIMARY KEY` with no default is required, and dropping the field
made such tables impossible to create through the UI at all.

-}
fieldFromColumn : Bool -> ( String, Schema.Column ) -> Maybe (Field.Field String)
fieldFromColumn isEdit ( name, column ) =
    let
        attrs =
            [ Field.name name
            , Field.identifier name
            , Field.label (String.humanize name)
            , Field.required
                (case column.value of
                    Schema.Blank ->
                        column.required

                    _ ->
                        -- The schema reports a server-side default for this
                        -- column: leaving it empty is fine, the database
                        -- fills it in.
                        False
                )
            , Field.value
                (case column.value of
                    String str ->
                        FormValue.string str

                    Bool bool ->
                        FormValue.bool bool

                    Int int ->
                        FormValue.int int

                    Float float ->
                        FormValue.float float

                    _ ->
                        FormValue.blank
                )
            ]
    in
    if column.primaryKey && isEdit then
        Nothing

    else if column.primaryKey then
        -- `Field.text` on purpose: PK types vary (int, uuid, text) and the
        -- int/float widgets reject the uuid/text cases.
        Just (Field.text attrs)

    else if column.foreignKey /= Nothing then
        Just (Field.strictAutocomplete attrs)

    else
        Just
            (case column.columnType of
                Schema.StringCol ->
                    case
                        column.options
                            |> List.filterMap
                                (\value ->
                                    case value of
                                        String str ->
                                            Just str

                                        _ ->
                                            Nothing
                                )
                    of
                        [] ->
                            Field.text attrs

                        options ->
                            Field.select (Field.stringOptions options :: attrs)

                Schema.TextCol ->
                    Field.textarea (Field.autogrow True :: attrs)

                Schema.FloatCol ->
                    Field.float attrs

                Schema.IntegerCol ->
                    Field.int attrs

                Schema.BoolCol ->
                    Field.checkbox attrs

                Schema.TimestampWithoutTimezomeCol ->
                    Field.datetime attrs

                Schema.TimestampCol ->
                    Field.datetime attrs

                Schema.TimeWithoutTimezoneCol ->
                    Field.datetime attrs

                Schema.TimeCol ->
                    Field.datetime attrs

                Schema.DateCol ->
                    Field.date attrs

                Schema.JsonCol ->
                    Field.textarea (Field.autogrow True :: attrs)

                Schema.UuidCol ->
                    Field.text attrs

                _ ->
                    Field.text (Field.disabled True :: attrs)
            )


sortColumns : ( String, Schema.Column ) -> ( String, Schema.Column ) -> Order
sortColumns ( name, column ) ( name_, column_ ) =
    if column.primaryKey then
        LT

    else if column_.primaryKey then
        GT

    else if column.foreignKey /= Nothing then
        LT

    else if column_.foreignKey /= Nothing then
        GT

    else
        compare name name_
