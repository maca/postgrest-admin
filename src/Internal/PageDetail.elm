module Internal.PageDetail exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Internal.Cmd as AppCmd exposing (AppCmd)
import Internal.Schema as Schema exposing (Record, Table)
import PostgRestAdmin.Client as Client exposing (Client, Error)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath)
import PostgRestAdmin.Views as Views
import String.Extra as String
import Task
import Url.Builder as Url


type Msg
    = Fetched (Result Error Record)
    | GotCount String (Result Error Int)
    | Deleted (Result Error ())
    | DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed


type alias Model =
    { client : Client
    , mountPath : MountPath
    , key : Nav.Key
    , table : Table
    , id : String
    , record : Maybe Record
    , confirmDelete : Bool
    , deleteError : Maybe String
    , countTotals : Dict String Int
    }


init :
    { client : Client
    , mountPath : MountPath
    , table : Table
    , id : String
    }
    -> Nav.Key
    -> ( Model, AppCmd Msg )
init { client, mountPath, table, id } key =
    ( { client = client
      , mountPath = mountPath
      , key = key
      , table = table
      , id = id
      , record = Nothing
      , confirmDelete = False
      , deleteError = Nothing
      , countTotals = Dict.empty
      }
    , Client.fetchRecord
        { client = client
        , table = table
        , id = id
        }
        |> Task.attempt Fetched
        |> AppCmd.wrap
    )


update : Msg -> Model -> ( Model, AppCmd Msg )
update msg model =
    case msg of
        Fetched (Ok record) ->
            ( { model | record = Just record }
            , model.table.referencedBy
                |> List.map
                    (\ref ->
                        Client.count
                            { client = model.client
                            , path =
                                Url.absolute
                                    [ ref.tableName ]
                                    [ Url.string ref.foreignKey ("eq." ++ model.id) ]
                            }
                            |> Task.attempt (GotCount ref.tableName)
                            |> AppCmd.wrap
                    )
                |> AppCmd.batch
            )

        GotCount tableName (Ok total) ->
            ( { model | countTotals = Dict.insert tableName total model.countTotals }
            , AppCmd.none
            )

        GotCount _ _ ->
            ( model, AppCmd.none )

        Fetched (Err err) ->
            ( model
            , AppCmd.clientError err
            )

        Deleted (Ok _) ->
            ( model
            , AppCmd.batch
                [ Url.absolute [ model.table.name ] []
                    |> Nav.pushUrl model.key
                    |> AppCmd.wrap
                , AppCmd.confirm "The record was deleted"
                ]
            )

        Deleted (Err (Client.ConstraintViolation dbErr)) ->
            -- The row is still referenced by other rows (23503), or some
            -- other constraint blocks the delete. Explain that in place,
            -- with the modal kept open so the message is next to the
            -- button that caused it.
            ( { model | deleteError = Just (Client.dbErrorMessage dbErr) }
            , AppCmd.none
            )

        Deleted (Err err) ->
            ( { model | deleteError = Nothing }
            , AppCmd.clientError err
            )

        DeleteModalOpened ->
            ( { model | confirmDelete = True, deleteError = Nothing }, AppCmd.none )

        DeleteModalClosed ->
            ( { model | confirmDelete = False, deleteError = Nothing }, AppCmd.none )

        DeleteConfirmed ->
            ( model
            , Client.deleteRecord
                { client = model.client, table = model.table, id = model.id }
                |> Task.attempt Deleted
                |> AppCmd.wrap
            )



-- View


view : Model -> Html Msg
view model =
    case model.record of
        Nothing ->
            Html.text ""

        Just record ->
            let
                tableName =
                    model.table.name
            in
            Html.section
                [ Attrs.class "record-detail" ]
                [ MountPath.breadcrumbs model.mountPath
                    tableName
                    [ ( tableName, Nothing )
                    , ( model.id, recordLabel model.table record )
                    ]
                , Html.h2
                    []
                    [ recordLabel model.table record
                        |> Maybe.map Html.text
                        |> Maybe.withDefault (Html.text "")
                    ]
                , Html.article
                    [ Attrs.class "card" ]
                    [ Html.table
                        []
                        (Schema.tableToSortedColumnList model.table
                            |> List.map
                                (\( colName, col ) ->
                                    Dict.get colName record
                                        |> Maybe.map
                                            (\value ->
                                                Html.tr
                                                    []
                                                    [ Html.th
                                                        []
                                                        [ Html.text (String.humanize colName) ]
                                                    , Html.td
                                                        []
                                                        [ Views.renderValue model.mountPath col value ]
                                                    ]
                                            )
                                        |> Maybe.withDefault (Html.text "")
                                )
                        )
                    , actions model
                    ]
                , if model.confirmDelete then
                    Html.div
                        [ Attrs.class "modal-background" ]
                        [ Html.div
                            [ Attrs.class "modal-dialog" ]
                            [ Html.h2 []
                                [ Html.text """Are you sure you want to delete the
                                          record?"""
                                ]
                            , Html.p [] [ Html.text "This action cannot be undone." ]
                            , case model.deleteError of
                                Just message ->
                                    Html.div
                                        [ Attrs.class "form-error-message" ]
                                        [ Html.text message ]

                                Nothing ->
                                    Html.text ""
                            , Html.div
                                [ Attrs.class "actions" ]
                                [ Html.button
                                    [ Attrs.class "button button-danger"
                                    , Events.onClick DeleteConfirmed
                                    ]
                                    [ Html.text "Delete" ]
                                , Html.button
                                    [ Attrs.class "button"
                                    , Events.onClick DeleteModalClosed
                                    ]
                                    [ Html.text "Cancel" ]
                                ]
                            ]
                        ]

                  else
                    Html.text ""
                , Html.aside
                    [ Attrs.class "associations" ]
                    (model.table.referencedBy
                        |> List.map
                            (\ref ->
                                Html.a
                                    [ Attrs.class "card association"
                                    , Attrs.href
                                        (MountPath.path model.mountPath <|
                                            Url.absolute [ model.table.name, model.id, ref.tableName ] []
                                        )
                                    ]
                                    [ Html.text (String.humanize ref.tableName)
                                    , Dict.get ref.tableName model.countTotals
                                        |> Maybe.map (\i -> " (" ++ String.fromInt i ++ ")")
                                        |> Maybe.withDefault ""
                                        |> Html.text
                                    ]
                            )
                    )
                ]


actions : Model -> Html Msg
actions { mountPath, table, id } =
    Html.div
        [ Attrs.class "actions" ]
        [ Html.a
            [ Attrs.href
                (MountPath.path mountPath <|
                    Url.absolute
                        [ table.name, id, "edit" ]
                        []
                )
            , Attrs.class "button"
            ]
            [ Html.text "Edit" ]
        , Html.button
            [ Events.onClick DeleteModalOpened
            , Attrs.class "button button-danger"
            ]
            [ Html.text "Delete" ]
        ]



-- UTILS


recordLabel : Table -> Record -> Maybe String
recordLabel table record =
    Schema.label table
        |> Maybe.andThen
            (\fieldName ->
                Dict.get fieldName record
                    |> Maybe.andThen Schema.valueToHumanString
            )
