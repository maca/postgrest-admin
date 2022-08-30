module Internal.PageListing exposing
    ( Msg
    , PageListing
    , ascendingBy
    , descendingBy
    , hideSearch
    , init
    , isSearchVisible
    , onLogin
    , showSearch
    , subscriptions
    , update
    , view
    )

import Browser.Dom as Dom exposing (Viewport, blur)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Csv exposing (Csv)
import Dict
import Dict.Extra as Dict
import File exposing (File)
import File.Select as Select
import Html
    exposing
        ( Html
        , a
        , aside
        , b
        , br
        , button
        , div
        , h2
        , h4
        , header
        , i
        , p
        , section
        , span
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , href
        , id
        )
import Html.Events as Events
    exposing
        ( on
        , onClick
        , onMouseDown
        , onMouseUp
        )
import Http
import Inflect
import Internal.Client exposing (listableColumns, listingSelects)
import Internal.Cmd as AppCmd
import Internal.Download as Download exposing (Download, Format(..))
import Internal.Field as Field
import Internal.Http exposing (Error(..), errorToString, handleJsonResponse)
import Internal.Record exposing (primaryKey, setValidation, updateWithString)
import Internal.Schema as Schema exposing (Constraint(..), Table)
import Internal.Search as Search exposing (Search)
import Internal.Value as Value
import Internal.ViewHelp exposing (breadcrumbs)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import List.Split as List
import Markdown
import PostgRestAdmin.Client as Client exposing (Client, Collection)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG
import Set
import String.Extra as String
import Task
import Time
import Time.Extra as Time
import Url
import Url.Builder as Url exposing (QueryParameter, string)


type Page
    = Page (List Record)
    | Blank


type SortOrder
    = Asc String
    | Desc String
    | Unordered


type alias CsvUpload =
    { missingColumns : List String
    , extraColumns : List String
    , headers : List String
    , records : List ( Int, Record )
    }


type UploadState
    = Idle
    | Fetching
    | BadCsvSchema CsvUpload
    | UploadReady (List ( Int, Record ))
    | UploadWithErrors (List ( Int, Record ))


type Msg
    = LoggedIn Client
    | Fetched (Result Error (Collection Record))
    | ParentFetched (Result Error Record)
    | RecordLinkClicked String String
    | ApplyFilters
    | Sort SortOrder
    | Reload
    | Scrolled
    | ScrollInfo (Result Dom.Error Viewport)
    | SearchChanged Search.Msg
    | SelectEnter
    | SelectOn
    | SelectOff
    | DownloadRequested Format
    | Downloaded (Result Error Download)
    | ToggleSearchOpen
    | CsvUploadRequested
    | CsvUploadSelected File
    | CsvUploadLoaded String
    | CsvProcessed Csv (Result Error (List Record))
    | CsvUploadPosted (Result Error ())
    | CsvUploadAccepted
    | CsvUploadCanceled
    | NoOp


type TextSelect
    = Enter
    | On
    | Off


type alias PageListing =
    { client : Client
    , key : Nav.Key
    , table : Table
    , parent : Maybe Record
    , foreignKey : Maybe ( String, String )
    , scrollPosition : Float
    , pages : List Page
    , page : Int
    , order : SortOrder
    , search : Search
    , searchOpen : Bool
    , textSelect : TextSelect
    , uploadState : UploadState
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


init :
    { client : Client
    , table : Table
    , parent : Maybe { tableName : String, id : String }
    }
    -> Url.Url
    -> Nav.Key
    -> ( PageListing, AppCmd.Cmd Msg )
init { client, table, parent } url key =
    let
        order =
            Maybe.map parseQuery url.query
                |> Maybe.withDefault []
                |> (List.filter (Tuple.first >> (==) "order") >> List.head)
                |> Maybe.map (Tuple.second >> parseOrder)
                |> Maybe.withDefault Unordered

        parentParams =
            Maybe.andThen
                (\params ->
                    Client.getTable params.tableName client
                        |> Maybe.map (\parentTable -> ( parentTable, params ))
                )
                parent

        listing =
            { client = client
            , key = key
            , table = table
            , parent = Nothing
            , foreignKey = parentReference table parent
            , page = 0
            , scrollPosition = 0
            , pages = []
            , order = order
            , search = Search.init table (url.query |> Maybe.withDefault "")
            , searchOpen = False
            , textSelect = Off
            , uploadState = Idle
            }
    in
    ( listing
    , AppCmd.batch
        [ fetch listing
        , case parentParams of
            Just ( parentTable, { id } ) ->
                Client.fetchRecord
                    { client = client
                    , table = parentTable
                    , id = id
                    , expect = ParentFetched
                    }

            Nothing ->
                AppCmd.none
        ]
    )


onLogin : Client -> Msg
onLogin =
    LoggedIn


isSearchVisible : PageListing -> Bool
isSearchVisible { searchOpen, search } =
    searchOpen || Search.isBlank search


showSearch : PageListing -> PageListing
showSearch listing =
    { listing | searchOpen = True }


hideSearch : PageListing -> PageListing
hideSearch listing =
    { listing | searchOpen = False }


ascendingBy : String -> PageListing -> PageListing
ascendingBy column listing =
    { listing | order = Asc column }


descendingBy : String -> PageListing -> PageListing
descendingBy column listing =
    { listing | order = Desc column }


fetch : PageListing -> AppCmd.Cmd Msg
fetch pageListing =
    Client.requestMany
        { client = pageListing.client
        , method = "GET"
        , headers = [ Http.header "Prefer" "count=planned" ]
        , path =
            listingPath { limit = True, selectAll = False, nest = False }
                pageListing
        , body = Http.emptyBody
        , decoder = Record.decoder pageListing.table
        , expect = Fetched
        }


update : Msg -> PageListing -> ( PageListing, AppCmd.Cmd Msg )
update msg listing =
    case msg of
        LoggedIn client ->
            fetchListing
                { listing | client = client }

        Fetched (Ok { list }) ->
            ( { listing
                | page = listing.page + 1
                , pages =
                    case listing.pages of
                        Blank :: ps ->
                            if List.length list < perPage then
                                Blank :: Page list :: ps

                            else
                                Page list :: ps

                        _ ->
                            Page list :: listing.pages
              }
            , AppCmd.none
            )

        Fetched (Err err) ->
            case listing.order of
                Unordered ->
                    ( listing
                    , Notification.error (errorToString err)
                    )

                _ ->
                    ( { listing | order = Unordered }
                    , reload listing.table
                    )

        ParentFetched (Ok parent) ->
            ( { listing | parent = Just parent }, AppCmd.none )

        ParentFetched (Err err) ->
            ( listing, Notification.error (Internal.Http.errorToString err) )

        RecordLinkClicked tableName id ->
            ( listing
            , Url.absolute [ tableName, id ] []
                |> Nav.pushUrl listing.key
                |> AppCmd.wrap
            )

        ApplyFilters ->
            ( listing, reload listing.table )

        Sort order ->
            ( { listing | order = order }, reload listing.table )

        Reload ->
            ( listing
            , listing
                |> listingPath { limit = False, selectAll = True, nest = True }
                |> Nav.replaceUrl listing.key
                |> AppCmd.wrap
            )

        Scrolled ->
            ( listing
            , Dom.getViewportOf listing.table.name
                |> Task.attempt ScrollInfo
                |> AppCmd.wrap
            )

        ScrollInfo result ->
            case result of
                Ok viewport ->
                    if
                        scrollingDown viewport listing
                            && closeToBottom viewport
                    then
                        case listing.pages of
                            Blank :: _ ->
                                ( { listing
                                    | scrollPosition =
                                        viewport.viewport.y
                                  }
                                , AppCmd.none
                                )

                            _ ->
                                fetchListing
                                    { listing
                                        | scrollPosition = viewport.viewport.y
                                        , pages = Blank :: listing.pages
                                    }

                    else
                        ( { listing | scrollPosition = viewport.viewport.y }
                        , AppCmd.none
                        )

                Err _ ->
                    ( listing
                    , Notification.error "Dom element not found"
                    )

        SearchChanged searchMsg ->
            Search.update searchMsg listing.search
                |> Tuple.mapFirst (\search -> { listing | search = search })
                |> Tuple.mapSecond (searchChanged searchMsg)

        ToggleSearchOpen ->
            ( { listing | searchOpen = not listing.searchOpen }
            , AppCmd.none
            )

        SelectEnter ->
            ( { listing | textSelect = Enter }
            , AppCmd.none
            )

        SelectOff ->
            ( { listing | textSelect = Off }
            , AppCmd.none
            )

        SelectOn ->
            ( { listing | textSelect = On }
            , AppCmd.none
            )

        DownloadRequested format ->
            ( listing
            , Download.init format
                (listingPath
                    { limit = False
                    , selectAll = True
                    , nest = False
                    }
                    listing
                )
                |> Download.fetch listing.client
                |> Task.attempt Downloaded
                |> AppCmd.wrap
            )

        Downloaded (Ok download) ->
            ( listing
            , Download.save listing.table.name download
                |> AppCmd.wrap
            )

        Downloaded (Err err) ->
            ( listing, Notification.error (Internal.Http.errorToString err) )

        CsvUploadRequested ->
            ( listing
            , AppCmd.batch
                [ Task.attempt (always NoOp) (Dom.blur "upload-csv-button")
                    |> AppCmd.wrap
                , Select.file [ "text/csv" ] CsvUploadSelected
                    |> AppCmd.wrap
                ]
            )

        CsvUploadSelected file ->
            ( listing
            , Task.perform CsvUploadLoaded (File.toString file)
                |> AppCmd.wrap
            )

        CsvUploadLoaded string ->
            case Csv.parse string of
                Ok rawCsv ->
                    let
                        csv =
                            { rawCsv
                                | headers = List.map String.trim rawCsv.headers
                            }

                        headers =
                            Set.fromList csv.headers

                        columnNames =
                            Schema.columnNames listing.table

                        missing =
                            Set.diff columnNames headers

                        extra =
                            Set.diff headers columnNames
                    in
                    if Set.isEmpty extra && Set.isEmpty missing then
                        ( { listing | uploadState = Fetching }
                        , processCsv listing csv
                        )

                    else
                        ( { listing
                            | uploadState =
                                BadCsvSchema
                                    { extraColumns = Set.toList extra
                                    , missingColumns = Set.toList missing
                                    , headers = csv.headers
                                    , records =
                                        buildImportedRecords listing [] csv
                                    }
                          }
                        , AppCmd.none
                        )

                Err _ ->
                    ( listing
                    , Notification.error "This CSV file cound not be parsed."
                    )

        CsvProcessed csv (Ok existing) ->
            let
                records =
                    buildImportedRecords listing existing csv
            in
            case List.filter (\( _, rec ) -> Record.hasErrors rec) records of
                [] ->
                    ( { listing | uploadState = UploadReady records }
                    , AppCmd.none
                    )

                withErrors ->
                    ( { listing | uploadState = UploadWithErrors withErrors }
                    , AppCmd.none
                    )

        CsvProcessed _ (Err err) ->
            ( listing
            , Notification.error (errorToString err)
            )

        CsvUploadPosted (Ok ()) ->
            ( listing
            , AppCmd.batch
                [ Dom.setViewportOf listing.table.name 0 0
                    |> Task.attempt (always Reload)
                    |> AppCmd.wrap
                , Notification.confirm "The upload was succesful."
                ]
            )

        CsvUploadPosted (Err err) ->
            ( listing, Notification.error (Internal.Http.errorToString err) )

        CsvUploadAccepted ->
            ( listing
            , case listing.uploadState of
                UploadReady records ->
                    Client.request
                        { client = listing.client
                        , method = "POST"
                        , headers =
                            [ Http.header "Prefer"
                                "resolution=merge-duplicates"
                            ]
                        , path = Url.absolute [ listing.table.name ] []
                        , body =
                            Http.jsonBody
                                (Encode.list
                                    (\( _, rec ) -> Record.encode rec)
                                    records
                                )
                        , decoder = Decode.succeed ()
                        , expect = CsvUploadPosted
                        }

                _ ->
                    AppCmd.none
            )

        CsvUploadCanceled ->
            ( { listing | uploadState = Idle }, AppCmd.none )

        NoOp ->
            ( listing, AppCmd.none )


reload : Table -> AppCmd.Cmd Msg
reload table =
    Dom.setViewportOf table.name 0 0
        |> Task.attempt (always Reload)
        |> AppCmd.wrap


scrollingDown : Viewport -> PageListing -> Bool
scrollingDown { viewport } { scrollPosition } =
    scrollPosition < viewport.y


closeToBottom : Viewport -> Bool
closeToBottom { scene, viewport } =
    scene.height - viewport.y < (viewport.height * 2)


fetchListing : PageListing -> ( PageListing, AppCmd.Cmd Msg )
fetchListing listing =
    ( listing, fetch listing )


searchChanged : Search.Msg -> Cmd Search.Msg -> AppCmd.Cmd Msg
searchChanged msg cmd =
    if Search.isApplyMsg msg then
        Time.now
            |> Task.perform (always ApplyFilters)
            |> AppCmd.wrap

    else
        Cmd.map SearchChanged cmd |> AppCmd.wrap


listingPath :
    { limit : Bool, selectAll : Bool, nest : Bool }
    -> PageListing
    -> String
listingPath { limit, selectAll, nest } { search, page, table, order, parent, foreignKey } =
    let
        selectQuery =
            if selectAll then
                []

            else
                [ PG.select (listingSelects table) ]

        parentQuery =
            foreignKey
                |> Maybe.map
                    (\( colName, id ) ->
                        [ PG.param colName (PG.eq (PG.string id)) ]
                    )
                |> Maybe.withDefault []

        limitQuery =
            if limit then
                [ PG.limit perPage, PG.offset (perPage * page) ]

            else
                []

        queryParams =
            orderToQueryParams order

        baseUrl =
            if nest then
                Url.absolute
                    (List.filterMap identity
                        [ Maybe.map (Record.getTable >> .name) parent
                        , Maybe.andThen Record.id parent
                        , Just table.name
                        ]
                    )
                    (orderToQueryParams order)

            else
                Url.absolute [ table.name ]
                    (orderToQueryParams order)

        filterQuery =
            PG.toQueryString <|
                parentQuery
                    ++ limitQuery
                    ++ selectQuery
                    ++ Search.toPGQuery search

        joinChar =
            if List.isEmpty queryParams then
                "?"

            else
                "&"
    in
    [ baseUrl, filterQuery ]
        |> List.filterMap String.nonBlank
        |> String.join joinChar



-- UPLOAD


buildImportedRecords : PageListing -> List Record -> Csv -> List ( Int, Record )
buildImportedRecords { table, foreignKey } persistedRecords csv =
    let
        primaryKeyName =
            Schema.tablePrimaryKeyName table

        persisted =
            persistedRecords
                |> List.map
                    (\rec ->
                        ( primaryKey rec
                            |> Maybe.andThen
                                (\{ value } -> Value.toString value)
                            |> Maybe.withDefault ""
                        , rec
                        )
                    )
                |> Dict.fromList
    in
    csv.records
        |> List.indexedMap
            (\idx row ->
                let
                    blank =
                        Dict.fromList (List.zip csv.headers row)

                    dict =
                        foreignKey
                            |> Maybe.map
                                (\( colName, fkVal ) ->
                                    Dict.get colName blank
                                        |> Maybe.andThen String.nonBlank
                                        |> Maybe.map (always blank)
                                        |> Maybe.withDefault
                                            (Dict.insert colName fkVal blank)
                                )
                            |> Maybe.withDefault blank

                    record =
                        primaryKeyName
                            |> Maybe.andThen (\pkName -> Dict.get pkName dict)
                            |> Maybe.andThen (\id -> Dict.get id persisted)
                            |> Maybe.withDefault (Record.fromTable table)
                            |> setAssociationValidation foreignKey
                in
                ( idx + 1
                , Dict.foldl updateWithString record dict
                )
            )


setAssociationValidation : Maybe ( String, String ) -> Record -> Record
setAssociationValidation foreignKey record =
    foreignKey
        |> Maybe.map
            (\( colName, fkVal ) ->
                setValidation
                    (\value ->
                        if Value.toString value == Just fkVal then
                            Nothing

                        else
                            Just
                                ("Should correspond to the "
                                    ++ String.humanize colName
                                )
                    )
                    colName
                    record
            )
        |> Maybe.withDefault record


processCsv : PageListing -> Csv -> AppCmd.Cmd Msg
processCsv { table, client } csv =
    let
        primaryKeyName =
            Schema.tablePrimaryKeyName table

        blank =
            Record.fromTable table

        ids =
            primaryKeyName
                |> Maybe.map
                    (\pkName ->
                        List.filterMap
                            (List.zip csv.headers
                                >> Dict.fromList
                                >> Dict.get pkName
                                >> Maybe.andThen
                                    (\id ->
                                        updateWithString pkName id blank
                                            |> primaryKey
                                    )
                                >> Maybe.andThen
                                    (\{ value } -> Value.toString value)
                            )
                            csv.records
                            |> List.chunksOfLeft 100
                    )
                |> Maybe.withDefault []
    in
    if List.isEmpty ids then
        CsvProcessed csv (Ok [])
            |> Task.succeed
            |> Task.perform identity
            |> AppCmd.wrap

    else
        ids
            |> List.map
                (\chunk ->
                    let
                        url =
                            Client.toHostUrl client

                        path =
                            Url.absolute
                                [ table.name ]
                                (existenceQuery primaryKeyName
                                    chunk
                                )
                    in
                    Http.task
                        { url =
                            Url.toString { url | path = path }
                        , method = "GET"
                        , headers = []
                        , body = Http.emptyBody
                        , resolver =
                            Http.stringResolver
                                (handleJsonResponse
                                    (Decode.list
                                        (Record.decoder table)
                                    )
                                )
                        , timeout = Nothing
                        }
                )
            |> Task.sequence
            |> Task.map List.concat
            |> Task.attempt (CsvProcessed csv)
            |> AppCmd.wrap



-- VIEW


view : PageListing -> Html Msg
view listing =
    let
        fields =
            Dict.toList (listableColumns listing.table)
                |> List.sortWith Field.compareTuple
                |> List.map Tuple.first
    in
    section
        [ class "resources-listing" ]
        [ listHeader listing
        , div
            [ id listing.table.name
            , class "resources-listing-results"
            , case listing.pages of
                Blank :: _ ->
                    class ""

                _ ->
                    Events.on "scroll" (Decode.succeed Scrolled)
            ]
            [ uploadModal fields listing
            , Html.table []
                (tableHeading listing fields
                    :: pagesFold listing fields [] 0 listing.pages
                )
            ]
        , aside
            [ class "listing-controls" ]
            [ div
                [ class "controls" ]
                [ div
                    [ class "downloads" ]
                    [ button
                        [ class "button-clear"
                        , onClick (DownloadRequested JSON)
                        ]
                        [ text "Download JSON" ]
                    , button
                        [ class "button-clear"
                        , onClick (DownloadRequested CSV)
                        ]
                        [ text "Download CSV" ]
                    ]
                , div
                    []
                    [ if Search.isBlank listing.search then
                        text ""

                      else
                        toggleSearchButton listing
                    , button
                        [ onClick ApplyFilters
                        , disabled (Search.isBlank listing.search)
                        ]
                        [ text "Apply Filters" ]
                    ]
                ]
            , Html.map SearchChanged
                (Search.view (isSearchVisible listing) listing.search)
            ]
        ]


listHeader : PageListing -> Html Msg
listHeader { parent, table } =
    header
        []
        [ case parent of
            Just record ->
                breadcrumbs table.name
                    [ ( Record.getTable record |> .name, Nothing )
                    , ( Record.id record |> Maybe.withDefault ""
                      , Record.label record
                      )
                    , ( table.name, Nothing )
                    ]

            Nothing ->
                breadcrumbs table.name [ ( table.name, Nothing ) ]
        , div
            []
            [ button
                [ id "upload-csv-button"
                , class "button"
                , class ("button-upload-csv-" ++ table.name)
                , onClick CsvUploadRequested
                ]
                [ text "Upload CSV" ]
            , a
                [ class "button"
                , class ("button-new-" ++ table.name)
                , href <|
                    Url.absolute
                        (List.filterMap identity
                            [ Maybe.map (Record.getTable >> .name) parent
                            , Maybe.andThen Record.id parent
                            , Just table.name
                            , Just "new"
                            ]
                        )
                        []
                ]
                [ text
                    ("New " ++ (String.humanize table.name |> Inflect.toSingular))
                ]
            ]
        ]


tableHeading : PageListing -> List String -> Html Msg
tableHeading listing fields =
    thead
        []
        [ tr [] (List.map (tableHeader listing) fields) ]


tableHeader : PageListing -> String -> Html Msg
tableHeader { order } name =
    let
        defaultHeader =
            span
                [ class "sort"
                , attribute "aria-sort" "other"
                , onClick <| Sort <| Asc name
                ]
                [ text <| String.humanize name
                , i [ class "icono-play" ] []
                ]
    in
    th
        []
        [ case order of
            Asc col ->
                if col == name then
                    span
                        [ class "sort"
                        , attribute "aria-sort" "ascending"
                        , onClick <| Sort <| Desc name
                        ]
                        [ text <| String.humanize name
                        , i [ class "asc icono-play" ] []
                        ]

                else
                    defaultHeader

            Desc col ->
                if col == name then
                    span
                        [ class "sort"
                        , attribute "aria-sort" "descending"
                        , onClick <| Sort <| Asc name
                        ]
                        [ text <| String.humanize name
                        , i [ class "desc icono-play" ] []
                        ]

                else
                    defaultHeader

            Unordered ->
                defaultHeader
        ]


pagesFold :
    PageListing
    -> List String
    -> List (Html Msg)
    -> Int
    -> List Page
    -> List (Html Msg)
pagesFold listing fields acc pageNum pages =
    case pages of
        [] ->
            acc

        page :: rest ->
            let
                elem =
                    case page of
                        Page resources ->
                            pageHtml listing fields pageNum resources

                        Blank ->
                            text ""
            in
            pagesFold listing fields (elem :: acc) (pageNum + 1) rest


pageHtml : PageListing -> List String -> Int -> List Record -> Html Msg
pageHtml listing fields pageNum records =
    tbody
        [ id (pageId pageNum) ]
        (List.map (rowHtml listing fields) records)


rowHtml : PageListing -> List String -> Record -> Html Msg
rowHtml { table, textSelect } names record =
    let
        cell fieldName =
            Dict.get fieldName record.fields
                |> Maybe.map
                    (\field ->
                        td
                            []
                            [ span
                                []
                                [ field
                                    |> Field.toHtml (clickRecord textSelect)
                                        table.name
                                ]
                            ]
                    )
    in
    tr
        [ class "listing-row"
        , onMouseDown SelectEnter
        , if textSelect == Enter then
            on "mousemove" (Decode.succeed SelectOn)

          else
            class ""
        , onMouseUp SelectOff
        , Record.id record
            |> Maybe.withDefault ""
            |> clickRecord textSelect table.name
        ]
        (List.filterMap cell names)


clickRecord : TextSelect -> String -> String -> Html.Attribute Msg
clickRecord textSelect tableName id =
    let
        msg =
            if textSelect == On then
                SelectOff

            else
                RecordLinkClicked tableName id
    in
    Events.custom "click" <|
        Decode.map (EventConfig True True)
            (Decode.succeed msg)


toggleSearchButton : PageListing -> Html Msg
toggleSearchButton listing =
    button
        [ class "toggle-button"
        , class "button-clear"
        , classList [ ( "open", isSearchVisible listing ) ]
        , onClick ToggleSearchOpen
        ]
        [ i [ class "icono-play" ] []
        , if isSearchVisible listing then
            text "Hide"

          else
            text "Show"
        , text " Filters"
        ]


uploadModal : List String -> PageListing -> Html Msg
uploadModal fieldNames { uploadState, parent } =
    case uploadState of
        UploadReady records ->
            div
                [ class "modal-background"
                , class "upload-preview"
                ]
                [ uploadPreview parent fieldNames records ]

        UploadWithErrors records ->
            div
                [ class "modal-background"
                , class "upload-preview"
                ]
                [ uploadWithErrorsPreview parent fieldNames records
                ]

        BadCsvSchema csvUpload ->
            div
                [ class "modal-background"
                , class "upload-preview"
                ]
                [ badSchemaPreview csvUpload ]

        Idle ->
            text ""

        Fetching ->
            text ""


uploadPreview : Maybe Record -> List String -> List ( Int, Record ) -> Html Msg
uploadPreview parent fieldNames records =
    let
        ( toUpdate, toCreate ) =
            List.partition (\( _, { persisted } ) -> persisted) records
    in
    div
        [ class "modal-dialog" ]
        [ h2
            []
            [ text "CSV Upload" ]
        , case parent of
            Just record ->
                p
                    []
                    [ text "Upload records for "
                    , text <| String.humanize (.name (Record.getTable record))
                    , Record.id record
                        |> Maybe.map (\id -> text (" with id of " ++ id ++ "."))
                        |> Maybe.withDefault (text "")
                    ]

            Nothing ->
                text ""
        , div
            [ class "csv-records-preview" ]
            [ previewUploadTable "Records to create" fieldNames toCreate
            , previewUploadTable "Records to update" fieldNames toUpdate
            ]
        , Markdown.toHtml [ class "disclaimer" ] <|
            "Please review the following changes: "
                ++ (List.filterMap identity
                        [ previewUploadCopy "created" toCreate
                        , previewUploadCopy "updated" toUpdate
                        ]
                        |> String.toSentence
                   )
                ++ ".\n This action cannot be undone."
        , div
            [ class "actions" ]
            [ button
                [ class "button"
                , class "button-clear"
                , onClick CsvUploadCanceled
                ]
                [ text "Cancel" ]
            , button
                [ class "button button"
                , onClick CsvUploadAccepted
                ]
                [ text "Save" ]
            ]
        ]


previewUploadCopy : String -> List ( Int, Record ) -> Maybe String
previewUploadCopy action records =
    if List.isEmpty records then
        Nothing

    else
        Just
            ("**"
                ++ (List.length records |> String.fromInt)
                ++ String.surround " "
                    (case records of
                        _ :: [] ->
                            "record"

                        _ ->
                            "records"
                    )
                ++ "** will be "
                ++ action
            )


previewUploadTable : String -> List String -> List ( Int, Record ) -> Html Msg
previewUploadTable action fieldNames records =
    if List.isEmpty records then
        text ""

    else
        div
            []
            [ h4
                []
                [ text action
                , b []
                    [ text " ("
                    , text (List.length records |> String.fromInt)
                    , text ")"
                    ]
                ]
            , previewTable fieldNames records
            ]


badSchemaPreview : CsvUpload -> Html Msg
badSchemaPreview { extraColumns, missingColumns, headers, records } =
    let
        displayColumns =
            List.map (String.surround "**") >> String.toSentence
    in
    div
        [ class "modal-dialog" ]
        [ h2 [] [ text "Wrong columns" ]
        , p []
            [ ("The CSV file "
                ++ (List.filterMap identity
                        [ if not (List.isEmpty missingColumns) then
                            Just
                                ("is missing the following columns: "
                                    ++ displayColumns missingColumns
                                )

                          else
                            Nothing
                        , if not (List.isEmpty extraColumns) then
                            Just
                                ("should not contain the following columns: "
                                    ++ displayColumns extraColumns
                                )

                          else
                            Nothing
                        ]
                        |> String.join ", and it"
                   )
                ++ "."
              )
                |> Markdown.toHtml []
            , p
                []
                [ text "Please update the CSV and try again." ]
            ]
        , div
            [ class "csv-records-preview" ]
            [ previewTable headers records ]
        , div
            [ class "actions" ]
            [ button
                [ class "button button"
                , onClick CsvUploadCanceled
                ]
                [ text "Ok" ]
            ]
        ]


uploadWithErrorsPreview :
    Maybe Record
    -> List String
    -> List ( Int, Record )
    -> Html Msg
uploadWithErrorsPreview parent fieldNames records =
    div
        [ class "modal-dialog" ]
        [ h2
            []
            [ text "Validation failed" ]
        , p []
            [ case parent of
                Just record ->
                    text <|
                        "There where some errors validating the records for "
                            ++ String.humanize (.name (Record.getTable record))
                            ++ (Record.id record
                                    |> Maybe.map
                                        (\id -> " with id of " ++ id ++ ". ")
                                    |> Maybe.withDefault ""
                               )

                Nothing ->
                    text "There where some errors validating the records. "
            , br [] []
            , text "Please update the CSV and try again."
            , br [] []
            ]
        , div
            [ class "csv-records-preview" ]
            [ previewTable fieldNames
                (List.filter (\( _, rec ) -> Record.hasErrors rec) records)
            ]
        , div
            [ class "actions" ]
            [ button
                [ class "button button"
                , onClick CsvUploadCanceled
                ]
                [ text "Ok" ]
            ]
        ]


previewTable : List String -> List ( Int, Record ) -> Html Msg
previewTable fieldNames records =
    Html.table
        []
        [ thead
            []
            (List.map
                (\name -> th [] [ text name ])
                ("Row No." :: fieldNames)
            )
        , tbody
            []
            (List.map
                (\( idx, record ) ->
                    tr
                        [ if Record.hasErrors record then
                            class "with-errors"

                          else
                            class ""
                        ]
                        (td [] [ text (String.fromInt idx) ]
                            :: List.map (previewListCell record) fieldNames
                        )
                )
                records
            )
        ]


previewListCell : Record -> String -> Html Msg
previewListCell record fieldName =
    td
        []
        (Dict.get fieldName record.fields
            |> Maybe.map
                (\({ value } as field) ->
                    [ span
                        []
                        [ Field.valueToHtml value ]
                    , field
                        |> Field.validate record.persisted
                        |> .error
                        |> Maybe.map
                            (\err -> span [ class "error" ] [ text err ])
                        |> Maybe.withDefault (text "")
                    ]
                )
            |> Maybe.withDefault [ text "" ]
        )


pageId : Int -> String
pageId pageNum =
    "page-" ++ String.fromInt pageNum


perPage : Int
perPage =
    50


subscriptions : PageListing -> Sub Msg
subscriptions pageListing =
    onKeyDown
        (Decode.andThen
            (\key ->
                case key of
                    27 ->
                        Decode.succeed CsvUploadCanceled

                    13 ->
                        case pageListing.uploadState of
                            BadCsvSchema _ ->
                                Decode.succeed CsvUploadCanceled

                            UploadWithErrors _ ->
                                Decode.succeed CsvUploadCanceled

                            UploadReady _ ->
                                Decode.succeed CsvUploadAccepted

                            Idle ->
                                Decode.fail "upload not ready"

                            Fetching ->
                                Decode.fail "upload not ready"

                    _ ->
                        Decode.fail "wrong key"
            )
            (Decode.field "which" Decode.int)
        )



-- URL PARSING


parseQuery : String -> List ( String, String )
parseQuery queryString =
    String.split "&" queryString |> List.filterMap parseQueryHelp


parseQueryHelp : String -> Maybe ( String, String )
parseQueryHelp fragment =
    case String.split "=" fragment of
        [ key, val ] ->
            Url.percentDecode val
                |> Maybe.map2 Tuple.pair (Url.percentDecode key)

        _ ->
            Nothing


parseOrder : String -> SortOrder
parseOrder fragment =
    case String.split "." fragment of
        [ col, "asc" ] ->
            Asc col

        [ col, "desc" ] ->
            Desc col

        _ ->
            Unordered



-- UTILS


existenceQuery : Maybe String -> List String -> List QueryParameter
existenceQuery primaryKeyName ids =
    List.filterMap identity
        [ primaryKeyName
            |> Maybe.map
                (\pkn ->
                    let
                        conds =
                            ids
                                |> List.map (\id -> pkn ++ ".eq." ++ id)
                                |> String.join ","
                    in
                    Url.string "or" <| "(" ++ conds ++ ")"
                )
        ]


orderToQueryParams : SortOrder -> List QueryParameter
orderToQueryParams order =
    case order of
        Asc column ->
            [ Url.string "order" <| column ++ "." ++ "asc" ]

        Desc column ->
            [ Url.string "order" <| column ++ "." ++ "desc" ]

        Unordered ->
            []


parentReference :
    Table
    -> Maybe { tableName : String, id : String }
    -> Maybe ( String, String )
parentReference table parent =
    Dict.toList table.columns
        |> List.filterMap
            (\( colName, { constraint } ) ->
                case constraint of
                    ForeignKey foreignKey ->
                        case parent of
                            Just { tableName, id } ->
                                if foreignKey.tableName == tableName then
                                    Just ( colName, id )

                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing
            )
        |> List.head
