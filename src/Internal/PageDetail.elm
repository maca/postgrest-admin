module Internal.PageDetail exposing
    ( Msg
    , PageDetail
    , init
    , onLogin
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict
import Html
    exposing
        ( Html
        , a
        , article
        , aside
        , button
        , div
        , h1
        , h2
        , p
        , section
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Internal.Cmd as AppCmd
import Internal.Field as Field exposing (Field)
import Internal.Record as Record exposing (Record)
import Internal.Schema exposing (Constraint(..), Reference, Schema, Table)
import Internal.Value exposing (Value(..))
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.Notification as Notification
import String.Extra as String
import Url
import Url.Builder as Url
import Utils.Task exposing (Error(..))


type Msg
    = LoggedIn Client
    | Fetched (Result Error Record)
    | Deleted (Result Error Record)
    | DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed


type PageDetail
    = PageDetail
        { client : Client
        , key : Nav.Key
        , table : Table
        , id : String
        , record : Maybe Record
        , confirmDelete : Bool
        }


init :
    { client : Client, table : Table, id : String }
    -> Nav.Key
    -> ( PageDetail, AppCmd.Cmd Msg )
init { client, table, id } key =
    let
        pageDetail =
            PageDetail
                { client = client
                , key = key
                , table = table
                , id = id
                , record = Nothing
                , confirmDelete = False
                }
    in
    ( pageDetail, fetch pageDetail )


fetch : PageDetail -> AppCmd.Cmd Msg
fetch (PageDetail { client, table, id }) =
    Client.fetchRecord
        { client = client
        , table = table
        , id = id
        , expect = Client.expectRecord Fetched table
        }


onLogin : Client -> Msg
onLogin =
    LoggedIn


update : Msg -> PageDetail -> ( PageDetail, AppCmd.Cmd Msg )
update msg (PageDetail params) =
    case msg of
        LoggedIn client ->
            let
                pageDetail =
                    PageDetail { params | client = client }
            in
            ( pageDetail, fetch pageDetail )

        Fetched (Ok record) ->
            ( PageDetail { params | record = Just record }
            , AppCmd.none
            )

        Fetched (Err err) ->
            ( PageDetail params
            , Notification.error (Utils.Task.errorToString err)
            )

        Deleted (Ok _) ->
            ( PageDetail params
            , AppCmd.batch
                [ Url.absolute [ params.table.name ] []
                    |> Nav.pushUrl params.key
                    |> AppCmd.wrap
                , Notification.confirm "The record was deleted"
                ]
            )

        Deleted (Err err) ->
            ( PageDetail params
            , Notification.error (Utils.Task.errorToString err)
            )

        DeleteModalOpened ->
            ( PageDetail { params | confirmDelete = True }, AppCmd.none )

        DeleteModalClosed ->
            ( PageDetail { params | confirmDelete = False }, AppCmd.none )

        DeleteConfirmed ->
            ( PageDetail params
            , case params.record of
                Just record ->
                    Client.deleteRecord
                        { client = params.client
                        , record = record
                        , expect = Client.expectRecord Deleted params.table
                        }

                Nothing ->
                    AppCmd.none
            )



-- View


view : Schema -> PageDetail -> Html Msg
view schema (PageDetail params) =
    case params.record of
        Nothing ->
            text ""

        Just record ->
            section
                [ class "record-detail" ]
                [ h1
                    []
                    [ Record.label record
                        |> Maybe.withDefault ""
                        |> (++) (String.humanize (Record.tableName record) ++ " - ")
                        |> text
                    ]
                , article
                    [ class "card" ]
                    [ table
                        []
                        (sortedFields record |> List.map (tableRow (Record.tableName record)))
                    , actions record
                    ]
                , if params.confirmDelete then
                    div
                        [ class "modal-background" ]
                        [ div
                            [ class "modal-dialog" ]
                            [ h2 []
                                [ text """Are you sure you want to delete the
                                                  record?"""
                                ]
                            , p [] [ text "This action cannot be undone." ]
                            , div
                                [ class "actions" ]
                                [ button
                                    [ class "button button-danger"
                                    , onClick DeleteConfirmed
                                    ]
                                    [ text "Delete" ]
                                , button
                                    [ class "button"
                                    , onClick DeleteModalClosed
                                    ]
                                    [ text "Cancel" ]
                                ]
                            ]
                        ]

                  else
                    text ""
                , aside
                    [ class "associations" ]
                    (Record.referencedBy schema record
                        |> List.map referenceToHtml
                    )
                ]


referenceToHtml : Reference -> Html Msg
referenceToHtml { foreignKeyName, foreignKeyValue, table } =
    a
        [ class "card association"
        , href
            ("/"
                ++ table.name
                ++ "?"
                ++ foreignKeyName
                ++ "=eq."
                ++ foreignKeyValue
            )
        ]
        [ text (String.humanize table.name) ]


actions : Record -> Html Msg
actions record =
    case Record.id record of
        Just id ->
            div
                [ class "actions" ]
                [ a
                    [ href
                        (Url.absolute [ Record.tableName record, id, "edit" ] [])
                    , class "button"
                    ]
                    [ text "Edit" ]
                , button
                    [ onClick DeleteModalOpened
                    , class "button button-danger"
                    ]
                    [ text "Delete" ]
                ]

        Nothing ->
            text ""


tableRow : String -> ( String, Field ) -> Html Msg
tableRow resourcesName ( name, field ) =
    tr
        []
        [ th [] [ text (String.humanize name) ]
        , td [] [ Field.toHtml (\_ _ -> class "") resourcesName field ]
        ]



-- Utils


sortedFields : Record -> List ( String, Field )
sortedFields record =
    Dict.toList record.fields
        |> List.sortWith Field.compareTuple
