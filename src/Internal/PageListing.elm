module Internal.PageListing exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom as Dom exposing (Viewport)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Csv
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Inflect
import Internal.Cmd as AppCmd exposing (AppCmd)
import Internal.Schema as Schema exposing (Column, ColumnType(..), Record, Table)
import Internal.Search as Search exposing (Search)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import List.Split as List
import Markdown
import PostgRestAdmin.Client as Client exposing (Client, Count, Error)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath)
import PostgRestAdmin.Views as Views
import Postgrest.Client as PG
import Set exposing (Set)
import String.Extra as String
import Task
import Time
import Url
import Url.Builder as Url exposing (QueryParameter)


type Page
    = Page (List Record)
    | Unloaded Int


type SortOrder
    = Asc String
    | Desc String
    | Unordered


type alias CsvRow =
    { id : Maybe String
    , record : Dict String String
    , rowNum : Int
    , persisted : Bool
    }


type alias CsvUpload =
    { missingColumns : List String
    , extraColumns : List String
    , headers : List String
    , records : List CsvRow
    }


type UploadState
    = Idle
    | FetchingIds
    | BadCsvSchema CsvUpload
    | UploadWithoutPreview File
    | UploadWithPreview File (List CsvRow)
    | UploadWithErrors (List CsvRow)


type Format
    = CSV
    | JSON


type Msg
    = Fetched Int (Result Error ( List Record, Count ))
    | ParentLabelFetched (Result Client.Error String)
    | ApplyFilters
    | Sort SortOrder
    | Reload
    | Scrolled
    | ScrollInfo (Result Dom.Error Viewport)
    | PageHeightCalculated (Result Dom.Error Float)
    | ColumnWidthsCalculated (Result Dom.Error (List Float))
    | JumpToPage Int
    | SearchChanged Search.Msg
    | DownloadRequested Format
    | Downloaded Format (Result Error Bytes)
    | ToggleSearchOpen
    | CsvUploadRequested
    | CsvUploadSelected File
    | CsvUploadLoaded File String
    | CsvProcessed File (Result Error (List CsvRow))
    | CsvUploadPosted (Result Error ())
    | CsvUploadAccepted File
    | CsvUploadCanceled
    | NoOp


type alias Model =
    { client : Client
    , mountPath : MountPath
    , key : Nav.Key
    , table : Table
    , columns : List ( String, Column )
    , parent : Maybe { tableName : String, id : String }
    , parentLabel : Maybe String
    , scrollPosition : Float
    , pages : List Page
    , pageHeight : Int
    , columnWidths : List Float
    , total : Maybe Int
    , order : SortOrder
    , search : Search
    , searchOpen : Bool
    , uploadState : UploadState
    , recordsPerPage : Int
    , requested : Set Int
    }


init :
    { client : Client
    , mountPath : MountPath
    , table : Table
    , parent : Maybe { tableName : String, id : String }
    , recordsPerPage : Int
    }
    -> Url.Url
    -> Nav.Key
    -> ( Model, AppCmd Msg )
init { client, mountPath, table, parent, recordsPerPage } url key =
    let
        order =
            Maybe.map parseQuery url.query
                |> Maybe.withDefault []
                |> List.filter (Tuple.first >> (==) "order")
                |> List.head
                |> Maybe.map (Tuple.second >> parseOrder)
                |> Maybe.withDefault Unordered

        model =
            { client = client
            , mountPath = mountPath
            , key = key
            , table = table
            , columns = Schema.tableToSortedColumnList table |> List.filter isColumnVisible
            , parent = parent
            , parentLabel = Nothing
            , pageHeight = 0
            , columnWidths = []
            , total = Nothing
            , scrollPosition = 0
            , pages = [ Unloaded 0 ]
            , order = order
            , search = Search.init table (url.query |> Maybe.withDefault "")
            , searchOpen = True
            , uploadState = Idle
            , recordsPerPage = recordsPerPage
            , requested = Set.empty
            }
    in
    ( model
    , AppCmd.batch
        [ fetchOffset model 0
        , parent
            |> Maybe.andThen (Schema.buildParentReference client.schema table)
            |> Maybe.map
                (Client.fetchParentLabel client
                    >> Task.attempt ParentLabelFetched
                    >> AppCmd.wrap
                )
            |> Maybe.withDefault AppCmd.none
        ]
    )


fetchOffset : Model -> Int -> AppCmd Msg
fetchOffset model offset =
    Client.fetchRecords
        { client = model.client
        , path =
            listingPath
                { limit = True
                , selectAll = False
                , nest = False
                , offset = offset
                }
                model
        , table = model.table
        }
        |> Task.attempt (Fetched offset)
        |> AppCmd.wrap


update : Msg -> Model -> ( Model, AppCmd Msg )
update msg model =
    case msg of
        Fetched offset (Ok ( records, count )) ->
            ( { model
                | pages =
                    model.pages
                        |> List.map
                            (\page ->
                                case page of
                                    Unloaded pageOffset ->
                                        if pageOffset == offset then
                                            Page records

                                        else
                                            page

                                    _ ->
                                        page
                            )
                , total = Just count.total
              }
            , if model.pageHeight == 0 then
                AppCmd.batch
                    [ Dom.getElement "page-0"
                        |> Task.map (\elem -> elem.element.height)
                        |> Task.attempt PageHeightCalculated
                        |> AppCmd.wrap
                    , Task.map2
                        (\tableWidth -> List.map (min (tableWidth / 2)))
                        (Dom.getElement model.table.name
                            |> Task.map (\elem -> elem.element.width)
                        )
                        (model.columns
                            |> List.indexedMap
                                (\idx _ ->
                                    Dom.getElement ("col-" ++ String.fromInt idx)
                                        |> Task.map (\elem -> elem.element.width)
                                )
                            |> Task.sequence
                        )
                        |> Task.attempt ColumnWidthsCalculated
                        |> AppCmd.wrap
                    ]

              else
                AppCmd.none
            )

        Fetched offset (Err err) ->
            ( { model | requested = Set.remove offset model.requested }
            , AppCmd.clientError err
            )

        PageHeightCalculated result ->
            ( { model | pageHeight = Result.map ceiling result |> Result.withDefault 0 }
            , AppCmd.none
            )

        ColumnWidthsCalculated result ->
            ( { model | columnWidths = Result.withDefault [] result }
            , AppCmd.none
            )

        ParentLabelFetched (Ok parentLabel) ->
            ( { model | parentLabel = Just parentLabel }
            , AppCmd.none
            )

        ParentLabelFetched (Err err) ->
            ( model, AppCmd.clientError err )

        ApplyFilters ->
            ( model, reload model.table )

        Sort order ->
            ( { model | order = order }, reload model.table )

        Reload ->
            ( { model | pages = [ Unloaded 0 ] }
            , AppCmd.batch
                [ fetchOffset model 0
                , listingPath { limit = False, selectAll = True, nest = True, offset = 0 } model
                    |> MountPath.path model.mountPath
                    |> Nav.replaceUrl model.key
                    |> AppCmd.wrap
                ]
            )

        Scrolled ->
            ( model
            , Dom.getViewportOf model.table.name
                |> Task.attempt ScrollInfo
                |> AppCmd.wrap
            )

        ScrollInfo (Ok ({ scene, viewport } as viewPortRec)) ->
            case unloadedPageInView model viewPortRec of
                Just offset ->
                    ( { model
                        | scrollPosition = viewport.y
                        , requested = Set.insert offset model.requested
                      }
                    , if Set.member offset model.requested then
                        AppCmd.none

                      else
                        fetchOffset model offset
                    )

                Nothing ->
                    let
                        scrolledToBottom =
                            (model.scrollPosition < viewport.y)
                                && (scene.height - viewport.y < viewport.height * 2)

                        offset =
                            model.recordsPerPage * currentPage model viewport.y
                    in
                    if
                        (scrolledToBottom && unloadedMissing model offset)
                            && (offset <= Maybe.withDefault 0 model.total)
                    then
                        ( { model
                            | scrollPosition = viewport.y
                            , requested = Set.insert offset model.requested
                            , pages = Unloaded offset :: model.pages
                          }
                        , fetchOffset model offset
                        )

                    else
                        ( { model | scrollPosition = viewport.y }, AppCmd.none )

        ScrollInfo (Err _) ->
            ( model, AppCmd.none )

        JumpToPage pageNum ->
            let
                loadedCount =
                    List.length model.pages

                scrollTo =
                    toFloat ((pageNum - 1) * model.pageHeight)
            in
            ( if pageNum <= loadedCount then
                model

              else
                { model
                    | pages =
                        List.range (loadedCount + 1) pageNum
                            |> List.foldl
                                (\num -> (::) (Unloaded ((num - 1) * model.recordsPerPage)))
                                model.pages
                }
            , AppCmd.batch
                [ Dom.setViewportOf model.table.name 0 scrollTo
                    |> Task.attempt (always NoOp)
                    |> AppCmd.wrap
                , fetchOffset model ((pageNum - 1) * model.recordsPerPage)
                ]
            )

        SearchChanged searchMsg ->
            Search.update searchMsg model.search
                |> Tuple.mapFirst (\search -> { model | search = search })
                |> Tuple.mapSecond (searchChanged searchMsg)

        ToggleSearchOpen ->
            ( { model | searchOpen = not model.searchOpen }
            , AppCmd.none
            )

        DownloadRequested format ->
            ( model
            , Client.bytesRequest
                { client = model.client
                , method = "GET"
                , headers =
                    [ case format of
                        CSV ->
                            Http.header "Accept" "text/csv"

                        JSON ->
                            Http.header "Accept" "application/json"
                    ]
                , path =
                    listingPath
                        { limit = False
                        , selectAll = True
                        , nest = False
                        , offset = 0
                        }
                        model
                , body = Http.emptyBody
                }
                |> Task.attempt (Downloaded format)
                |> AppCmd.wrap
            )

        Downloaded format (Ok body) ->
            let
                download ext =
                    (model.table.name ++ "-list." ++ ext)
                        |> Download.bytes
            in
            ( model
            , AppCmd.wrap
                (case format of
                    CSV ->
                        download "csv" "text/csv" body

                    JSON ->
                        download "json" "application/json" body
                )
            )

        Downloaded _ (Err err) ->
            ( model, AppCmd.clientError err )

        CsvUploadRequested ->
            ( model
            , AppCmd.batch
                [ Task.attempt (always NoOp) (Dom.blur "upload-csv-button")
                    |> AppCmd.wrap
                , Select.file [ "text/csv" ] CsvUploadSelected
                    |> AppCmd.wrap
                ]
            )

        CsvUploadSelected file ->
            if File.size file < 500000 then
                ( model
                , Task.perform (CsvUploadLoaded file) (File.toString file)
                    |> AppCmd.wrap
                )

            else
                ( { model | uploadState = UploadWithoutPreview file }
                , AppCmd.none
                )

        CsvUploadLoaded file string ->
            case Csv.parse string of
                Ok rawCsv ->
                    let
                        csv =
                            { rawCsv | headers = List.map String.trim rawCsv.headers }

                        headers =
                            Set.fromList csv.headers

                        columnNames =
                            Set.fromList (Dict.keys model.table.columns)

                        missing =
                            Set.diff columnNames headers

                        extra =
                            Set.diff headers columnNames

                        ( ids, records ) =
                            List.indexedFoldl
                                (\idx row ( idsAcc, recsAcc ) ->
                                    let
                                        record =
                                            List.zip csv.headers row |> Dict.fromList

                                        id =
                                            Schema.tablePrimaryKeyName model.table
                                                |> Maybe.andThen (\n -> Dict.get n record)
                                    in
                                    ( case id of
                                        Just i ->
                                            i :: idsAcc

                                        Nothing ->
                                            idsAcc
                                    , { id = id, record = record, rowNum = idx, persisted = False }
                                        :: recsAcc
                                    )
                                )
                                ( [], [] )
                                csv.records
                    in
                    if Set.isEmpty extra && Set.isEmpty missing then
                        case List.filter (hasErrors model.table) records of
                            [] ->
                                ( { model | uploadState = FetchingIds }
                                , (if List.isEmpty ids then
                                    CsvProcessed file (Ok records)
                                        |> Task.succeed
                                        |> Task.perform identity

                                   else
                                    List.chunksOfLeft 1000 ids
                                        |> List.map (Client.chunk model.client model.table)
                                        |> Task.sequence
                                        |> Task.map (List.concat >> Set.fromList)
                                        |> Task.map
                                            (\fetchIds ->
                                                List.map
                                                    (\rec ->
                                                        { rec
                                                            | persisted =
                                                                rec.id
                                                                    |> Maybe.map
                                                                        (\id -> Set.member id fetchIds)
                                                                    |> Maybe.withDefault False
                                                        }
                                                    )
                                                    records
                                            )
                                        |> Task.attempt (CsvProcessed file)
                                  )
                                    |> AppCmd.wrap
                                )

                            withErrors ->
                                -- Bad rows are caught before the id-existence
                                -- pre-check ever runs, so a malformed PK
                                -- value (e.g. non-numeric text in an integer
                                -- column) can't crash that query with a raw
                                -- Postgres error before any preview is shown.
                                ( { model | uploadState = UploadWithErrors withErrors }
                                , AppCmd.none
                                )

                    else
                        ( { model
                            | uploadState =
                                BadCsvSchema
                                    { extraColumns = Set.toList extra
                                    , missingColumns = Set.toList missing
                                    , headers = csv.headers
                                    , records = records
                                    }
                          }
                        , AppCmd.none
                        )

                Err _ ->
                    ( model
                    , AppCmd.error "This CSV file cound not be parsed."
                    )

        CsvProcessed file (Ok records) ->
            case List.filter (hasErrors model.table) records of
                [] ->
                    ( { model | uploadState = UploadWithPreview file records }
                    , AppCmd.none
                    )

                withErrors ->
                    ( { model | uploadState = UploadWithErrors withErrors }
                    , AppCmd.none
                    )

        CsvProcessed _ (Err err) ->
            ( model
            , AppCmd.clientError err
            )

        CsvUploadPosted (Ok ()) ->
            ( model
            , AppCmd.batch
                [ Dom.setViewportOf model.table.name 0 0
                    |> Task.attempt (always Reload)
                    |> AppCmd.wrap
                , AppCmd.confirm "The upload was succesful."
                ]
            )

        CsvUploadPosted (Err err) ->
            ( model, AppCmd.clientError err )

        CsvUploadAccepted file ->
            ( { model | uploadState = FetchingIds }
            , Client.jsonRequest
                { client = model.client
                , method = "POST"
                , headers =
                    [ Http.header "Prefer" "resolution=merge-duplicates"
                    , Http.header "Content-Type" "text/csv"
                    , Http.header "Accept" "application/json"
                    ]
                , path = "/" ++ model.table.name
                , body = Http.fileBody file
                , decoder =
                    Decode.value
                        |> Decode.map
                            (\resp ->
                                Encode.encode 0 resp
                            )
                        |> Decode.map (\_ -> ())
                }
                |> Task.attempt CsvUploadPosted
                |> AppCmd.wrap
            )

        CsvUploadCanceled ->
            ( { model | uploadState = Idle }, AppCmd.none )

        NoOp ->
            ( model, AppCmd.none )


currentPage : { a | pageHeight : Int, pages : List b } -> Float -> Int
currentPage { pageHeight, pages } scrollPosition =
    let
        pageNumber =
            floor (scrollPosition / toFloat pageHeight) + 1
    in
    max 1 (min pageNumber (max 1 (List.length pages)))


reload : Table -> AppCmd Msg
reload table =
    Dom.setViewportOf table.name 0 0
        |> Task.attempt (always Reload)
        |> AppCmd.wrap


unloadedPageInView : Model -> Viewport -> Maybe Int
unloadedPageInView model { viewport } =
    model.pages
        |> List.reverse
        |> List.indexedMap
            (\index page ->
                let
                    pageTop =
                        toFloat (index * model.pageHeight)

                    isInViewPort =
                        (pageTop <= (viewport.y + viewport.height))
                            && ((pageTop + toFloat model.pageHeight) >= viewport.y)
                in
                case ( page, isInViewPort ) of
                    ( Unloaded offset, True ) ->
                        Just offset

                    _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> List.head


unloadedMissing : Model -> Int -> Bool
unloadedMissing model offset =
    List.find
        (\page ->
            case page of
                Unloaded pageOffset ->
                    pageOffset == offset

                _ ->
                    False
        )
        model.pages
        |> Maybe.map (always False)
        |> Maybe.withDefault True


searchChanged : Search.Msg -> Cmd Search.Msg -> AppCmd Msg
searchChanged msg cmd =
    if Search.isApplyMsg msg then
        Time.now
            |> Task.perform (always ApplyFilters)
            |> AppCmd.wrap

    else
        Cmd.map SearchChanged cmd |> AppCmd.wrap


listingPath :
    { limit : Bool, selectAll : Bool, nest : Bool, offset : Int }
    -> Model
    -> String
listingPath { limit, selectAll, nest, offset } model =
    let
        selectQuery =
            if selectAll then
                []

            else
                [ PG.select
                    (List.concat
                        [ model.columns
                            |> List.map (Tuple.first >> PG.attribute)
                        , model.columns
                            |> List.filterMap
                                (Tuple.second >> Client.associationJoin)
                        ]
                    )
                ]

        parentQuery =
            parentReference model.table model.parent
                |> Maybe.map
                    (\( colName, id ) ->
                        [ PG.param colName (PG.eq (PG.string id)) ]
                    )
                |> Maybe.withDefault []

        limitQuery =
            if limit then
                [ PG.limit model.recordsPerPage, PG.offset offset ]

            else
                []

        queryParams =
            orderToQueryParams model.order

        baseUrl =
            if nest then
                Url.absolute
                    (List.filterMap identity
                        [ Maybe.map .tableName model.parent
                        , Maybe.map .id model.parent
                        , Just model.table.name
                        ]
                    )
                    (orderToQueryParams model.order)

            else
                Url.absolute [ model.table.name ]
                    (orderToQueryParams model.order)

        filterQuery =
            PG.toQueryString <|
                parentQuery
                    ++ limitQuery
                    ++ selectQuery
                    ++ Search.toPGQuery model.search

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


view : Model -> Html Msg
view model =
    Html.section
        [ Attrs.class "resources-listing" ]
        [ viewPageHeader model
        , Html.div
            [ Attrs.id model.table.name
            , Attrs.class "resources-listing-results"
            , Events.on "scroll" (Decode.succeed Scrolled)
            ]
            [ case model.uploadState of
                UploadWithPreview file records ->
                    uploadPreview model file records

                UploadWithErrors records ->
                    uploadWithErrorsPreview model records

                UploadWithoutPreview file ->
                    uploadWithoutPreview file

                BadCsvSchema csvUpload ->
                    badSchemaPreview model.table csvUpload

                Idle ->
                    Html.text ""

                FetchingIds ->
                    Html.text ""
            , viewTable model
            ]
        , Html.aside
            [ Attrs.class "listing-controls" ]
            [ Html.div
                [ Attrs.class "controls" ]
                [ Html.div
                    [ Attrs.class "downloads" ]
                    [ Html.button
                        [ Attrs.class "button-clear"
                        , Events.onClick (DownloadRequested JSON)
                        ]
                        [ Html.text "Download JSON" ]
                    , Html.button
                        [ Attrs.class "button-clear"
                        , Events.onClick (DownloadRequested CSV)
                        ]
                        [ Html.text "Download CSV" ]
                    ]
                , Html.div
                    []
                    [ if Search.isBlank model.search then
                        Html.text ""

                      else
                        toggleSearchButton model
                    , Html.button
                        [ Events.onClick ApplyFilters
                        , Attrs.disabled (Search.isBlank model.search)
                        ]
                        [ Html.text "Apply Filters" ]
                    ]
                ]
            , Html.map SearchChanged
                (Search.view (model.searchOpen || Search.isBlank model.search) model.search)
            ]
        ]


viewTable : Model -> Html Msg
viewTable model =
    Html.table
        [ Attrs.style "position" "relative"
        , Attrs.style "table-layout" "fixed"
        , Attrs.style "width" "max-content"
        , Attrs.style "min-width" "100%"
        ]
        (Html.colgroup []
            (List.map
                (\width ->
                    Html.col
                        (if width > 0 then
                            [ Attrs.style "width" (String.fromFloat width ++ "px") ]

                         else
                            []
                        )
                        []
                )
                (model.columnWidths ++ [ 0 ])
            )
            :: (case model.total of
                    Just _ ->
                        Html.thead []
                            [ Html.tr []
                                (List.indexedMap (viewHeaderCell model) model.columns
                                    ++ [ Html.th [] [] ]
                                )
                            ]

                    Nothing ->
                        Html.text ""
               )
            :: (case model.pages of
                    [ Unloaded _ ] ->
                        []

                    _ ->
                        List.reverse model.pages
                            |> List.indexedMap (viewPage model)
               )
        )


viewPageHeader : Model -> Html Msg
viewPageHeader model =
    Html.header
        []
        [ Html.div
            [ Attrs.class "listing-breadcrumbs" ]
            [ MountPath.breadcrumbs model.mountPath
                model.table.name
                (case model.parent of
                    Just parent ->
                        [ ( parent.tableName, Nothing )
                        , ( parent.id, model.parentLabel )
                        , ( model.table.name, Nothing )
                        ]

                    Nothing ->
                        [ ( model.table.name, Nothing ) ]
                )
            , viewPageSelect model
            ]
        , Html.div
            [ Attrs.class "listing-actions" ]
            [ Html.button
                [ Attrs.id "upload-csv-button"
                , Attrs.class "button"
                , Attrs.class ("button-upload-csv-" ++ model.table.name)
                , Events.onClick CsvUploadRequested
                ]
                [ Html.text "Upload CSV" ]
            , Html.a
                [ Attrs.class "button"
                , Attrs.class ("button-new-" ++ model.table.name)
                , Attrs.href
                    (MountPath.path model.mountPath <|
                        Url.absolute
                            (List.filterMap identity
                                [ Maybe.map .tableName model.parent
                                , Maybe.map .id model.parent
                                , Just model.table.name
                                , Just "new"
                                ]
                            )
                            []
                    )
                ]
                [ Html.text
                    ("New " ++ (String.humanize model.table.name |> Inflect.toSingular))
                ]
            ]
        ]


viewPage : Model -> Int -> Page -> Html Msg
viewPage model pageNum page =
    let
        pageHeight =
            String.fromInt model.pageHeight ++ "px"
    in
    case page of
        Page records ->
            Html.tbody
                [ Attrs.id ("page-" ++ String.fromInt pageNum) ]
                (List.map
                    (\record ->
                        Html.tr
                            [ Attrs.class "listing-row" ]
                            (viewRowCells model record ++ [ viewActions model record ])
                    )
                    records
                )

        Unloaded _ ->
            Html.tbody
                [ Attrs.id ("page-" ++ String.fromInt pageNum)
                , Attrs.style "height" pageHeight
                , Attrs.class "unloaded-page"
                ]
                (List.range 0 model.recordsPerPage
                    |> List.map
                        (\_ ->
                            Html.tr
                                [ Attrs.class "skeleton-row"
                                ]
                                (List.map
                                    (\_ ->
                                        Html.td
                                            []
                                            [ Html.div [ Attrs.class "skeleton-box" ] []
                                            ]
                                    )
                                    model.columns
                                    ++ [ Html.td [] [] ]
                                )
                        )
                )


onActivate : Msg -> Html.Attribute Msg
onActivate msg =
    Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" || key == " " then
                        Decode.succeed msg

                    else
                        Decode.fail "not an activation key"
                )
        )


sortControl : Msg -> List (Html Msg) -> Html Msg
sortControl msg =
    Html.span
        [ Attrs.class "sort"
        , Attrs.attribute "role" "button"
        , Attrs.tabindex 0
        , Events.onClick msg
        , onActivate msg
        ]


viewHeaderCell : Model -> Int -> ( String, Column ) -> Html Msg
viewHeaderCell { order } idx ( name, _ ) =
    let
        defaultHeader =
            sortControl (Sort <| Asc name)
                [ Html.text <| String.humanize name
                , Html.i [ Attrs.class "icono-play" ] []
                ]

        ( ariaSort, content ) =
            case order of
                Asc col ->
                    if col == name then
                        ( "ascending"
                        , sortControl (Sort <| Desc name)
                            [ Html.text <| String.humanize name
                            , Html.i [ Attrs.class "asc icono-play" ] []
                            ]
                        )

                    else
                        ( "none", defaultHeader )

                Desc col ->
                    if col == name then
                        ( "descending"
                        , sortControl (Sort <| Asc name)
                            [ Html.text <| String.humanize name
                            , Html.i [ Attrs.class "desc icono-play" ] []
                            ]
                        )

                    else
                        ( "none", defaultHeader )

                Unordered ->
                    ( "none", defaultHeader )
    in
    Html.th
        [ Attrs.id ("col-" ++ String.fromInt idx)
        , Attrs.attribute "aria-sort" ariaSort
        ]
        [ content ]


viewRowCells : Model -> Record -> List (Html Msg)
viewRowCells model record =
    List.filterMap
        (\( colName, col ) ->
            Dict.get colName record
                |> Maybe.map
                    (\value ->
                        Html.td
                            []
                            [ Html.span
                                [ Attrs.style "display" "block"
                                , Attrs.style "max-width" "320px"
                                , Attrs.style "overflow" "hidden"
                                , Attrs.style "text-overflow" "ellipsis"
                                , Attrs.style "white-space" "nowrap"
                                ]
                                [ Views.renderValue model.mountPath col value
                                ]
                            ]
                    )
        )
        model.columns


viewActions : Model -> Record -> Html Msg
viewActions model record =
    Html.td []
        [ recordId model.table record
            |> Maybe.map
                (\id ->
                    Html.a
                        [ Attrs.class "button button-clear button-small"
                        , Attrs.href
                            (MountPath.path model.mountPath
                                (Url.absolute [ model.table.name, id ] [])
                            )
                        ]
                        [ Html.text "View" ]
                )
            |> Maybe.withDefault (Html.text "")
        ]


toggleSearchButton : Model -> Html Msg
toggleSearchButton model =
    Html.button
        [ Attrs.class "toggle-button"
        , Attrs.class "button-clear"
        , Attrs.classList [ ( "open", model.searchOpen || Search.isBlank model.search ) ]
        , Events.onClick ToggleSearchOpen
        ]
        [ Html.i [ Attrs.class "icono-play" ] []
        , if model.searchOpen || Search.isBlank model.search then
            Html.text "Hide"

          else
            Html.text "Show"
        , Html.text " Filters"
        ]


viewPageSelect : Model -> Html Msg
viewPageSelect model =
    case model.total of
        Just total ->
            Html.div
                [ Attrs.class "record-count-info" ]
                [ let
                    totalPagesCount =
                        ceiling (toFloat total / toFloat model.recordsPerPage)
                  in
                  Html.select
                    [ Attrs.class "page-select"
                    , Attrs.name "page-number"
                    , Events.on "change"
                        (Decode.at [ "target", "value" ] Decode.string
                            |> Decode.andThen
                                (\value ->
                                    case String.toInt value of
                                        Just pageNum ->
                                            Decode.succeed (JumpToPage pageNum)

                                        Nothing ->
                                            Decode.fail "Invalid page number"
                                )
                        )
                    ]
                    (List.range 1 totalPagesCount
                        |> List.map
                            (\pageNum ->
                                Html.option
                                    [ Attrs.value (String.fromInt pageNum)
                                    , Attrs.selected
                                        (pageNum == currentPage model model.scrollPosition)
                                    ]
                                    [ Html.text
                                        (String.concat
                                            [ "Page "
                                            , String.fromInt pageNum
                                            , " - from "
                                            , String.fromInt (((pageNum - 1) * model.recordsPerPage) + 1)
                                            ]
                                        )
                                    ]
                            )
                    )
                , Html.span
                    [ Attrs.class "total-records" ]
                    [ Html.text
                        (model.total
                            |> Maybe.map (\num -> String.fromInt num ++ " records")
                            |> Maybe.withDefault ""
                        )
                    ]
                ]

        _ ->
            Html.text ""


modalDialog : String -> List (Html Msg) -> List (Html Msg) -> Html Msg
modalDialog title content actions =
    Html.div
        [ Attrs.class "modal-background"
        , Attrs.class "upload-preview"
        ]
        [ Html.div
            [ Attrs.class "modal-dialog" ]
            (List.concat
                [ Html.h2 [] [ Html.text title ] :: content
                , [ Html.div [ Attrs.class "actions" ] actions ]
                ]
            )
        ]


uploadPreview : Model -> File -> List CsvRow -> Html Msg
uploadPreview model file records =
    let
        fieldNames =
            List.map Tuple.first model.columns

        ( toUpdate, toCreate ) =
            List.partition .persisted records

        content =
            [ case model.parent of
                Just _ ->
                    Html.p
                        []
                        [ Html.text "Upload records for " ]

                Nothing ->
                    Html.text ""
            , Html.div
                [ Attrs.class "csv-records-preview" ]
                [ toCreate
                    |> previewUploadTable model.table "Records to create" fieldNames
                , toUpdate
                    |> previewUploadTable model.table "Records to update" fieldNames
                ]
            , Markdown.toHtml [ Attrs.class "disclaimer" ] <|
                "Please review the following changes: "
                    ++ (List.filterMap identity
                            [ previewUploadCopy "created" toCreate
                            , previewUploadCopy "updated" toUpdate
                            ]
                            |> String.toSentence
                       )
                    ++ ".\n This action cannot be undone."
            ]

        actions =
            [ Html.button
                [ Attrs.class "button"
                , Attrs.class "button-clear"
                , Events.onClick CsvUploadCanceled
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Attrs.class "button button"
                , Events.onClick (CsvUploadAccepted file)
                ]
                [ Html.text "Save" ]
            ]
    in
    modalDialog "CSV Upload" content actions


previewUploadCopy : String -> List CsvRow -> Maybe String
previewUploadCopy action records =
    if List.isEmpty records then
        Nothing

    else
        Just
            (String.concat
                [ "**"
                , String.fromInt (List.length records)
                , String.surround " "
                    (case records of
                        _ :: [] ->
                            "record"

                        _ ->
                            "records"
                    )
                , "** will be "
                , action
                ]
            )


previewUploadTable : Table -> String -> List String -> List CsvRow -> Html Msg
previewUploadTable table action fieldNames records =
    if List.isEmpty records then
        Html.text ""

    else
        Html.div
            []
            [ Html.h4
                []
                [ Html.text action
                , Html.b []
                    [ Html.text " ("
                    , Html.text (List.length records |> String.fromInt)
                    , Html.text ")"
                    ]
                ]
            , previewTable table fieldNames records
            ]


badSchemaPreview : Table -> CsvUpload -> Html Msg
badSchemaPreview table { extraColumns, missingColumns, headers, records } =
    let
        displayColumns =
            List.map (String.surround "**") >> String.toSentence

        content =
            [ Html.p []
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
                            |> String.join ", and it "
                       )
                    ++ "."
                  )
                    |> Markdown.toHtml []
                , Html.p [] [ Html.text "Please update the CSV and try again." ]
                ]
            , Html.div
                [ Attrs.class "csv-records-preview" ]
                [ previewTable table headers records ]
            ]

        actions =
            [ Html.button
                [ Attrs.class "button button"
                , Events.onClick CsvUploadCanceled
                ]
                [ Html.text "Ok" ]
            ]
    in
    modalDialog "Wrong columns" content actions


uploadWithErrorsPreview : Model -> List CsvRow -> Html Msg
uploadWithErrorsPreview model records =
    let
        fieldNames =
            List.map Tuple.first model.columns

        content =
            [ Html.p []
                [ Html.text "There where some errors validating the records. "
                , Html.br [] []
                , Html.text "Please update the CSV and try again."
                , Html.br [] []
                ]
            , Html.div
                [ Attrs.class "csv-records-preview" ]
                [ previewTable model.table fieldNames (List.filter (hasErrors model.table) records) ]
            ]

        actions =
            [ Html.button
                [ Attrs.class "button button"
                , Events.onClick CsvUploadCanceled
                ]
                [ Html.text "Ok" ]
            ]
    in
    modalDialog "Validation failed" content actions


uploadWithoutPreview : File -> Html Msg
uploadWithoutPreview file =
    let
        fileSizeMB =
            toFloat (File.size file) / 1024 / 1024

        fileSizeText =
            String.fromFloat (toFloat (round (fileSizeMB * 100)) / 100) ++ " MB"

        content =
            [ Html.p []
                [ Html.text "The file is too large to preview ("
                , Html.text fileSizeText
                , Html.text ")."
                ]
            , Html.p []
                [ Html.text "Would you like to upload it without preview?" ]
            ]

        actions =
            [ Html.button
                [ Attrs.class "button"
                , Attrs.class "button-clear"
                , Events.onClick CsvUploadCanceled
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Attrs.class "button button"
                , Events.onClick (CsvUploadAccepted file)
                ]
                [ Html.text "Upload" ]
            ]
    in
    modalDialog "Large File" content actions


previewTable : Table -> List String -> List CsvRow -> Html Msg
previewTable table fieldNames records =
    Html.table
        []
        [ Html.thead
            []
            (List.map
                (\name -> Html.th [] [ Html.text name ])
                ("Row No." :: fieldNames)
            )
        , Html.tbody
            []
            (List.map
                (\row ->
                    Html.tr
                        [ if hasErrors table row then
                            Attrs.class "with-errors"

                          else
                            Attrs.class ""
                        ]
                        (Html.td [] [ Html.text (String.fromInt row.rowNum) ]
                            :: List.map (previewListCell row.record) fieldNames
                        )
                )
                records
            )
        ]


previewListCell : Dict String String -> String -> Html Msg
previewListCell record fieldName =
    Html.td
        []
        (Dict.get fieldName record
            |> Maybe.map (\value -> [ Html.span [] [ Html.text value ] ])
            |> Maybe.withDefault [ Html.text "" ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown
        (Decode.andThen
            (\key ->
                case key of
                    27 ->
                        Decode.succeed CsvUploadCanceled

                    13 ->
                        case model.uploadState of
                            BadCsvSchema _ ->
                                Decode.succeed CsvUploadCanceled

                            UploadWithErrors _ ->
                                Decode.succeed CsvUploadCanceled

                            UploadWithPreview file _ ->
                                Decode.succeed (CsvUploadAccepted file)

                            UploadWithoutPreview file ->
                                Decode.succeed (CsvUploadAccepted file)

                            Idle ->
                                Decode.fail "upload not ready"

                            FetchingIds ->
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


orderToQueryParams : SortOrder -> List QueryParameter
orderToQueryParams order =
    case order of
        Asc column ->
            [ Url.string "order" <| column ++ "." ++ "asc" ]

        Desc column ->
            [ Url.string "order" <| column ++ "." ++ "desc" ]

        Unordered ->
            []


parentReference : Table -> Maybe { tableName : String, id : String } -> Maybe ( String, String )
parentReference table =
    Maybe.andThen
        (\parent ->
            Dict.toList table.columns
                |> List.filterMap
                    (\( colName, col ) ->
                        case col.foreignKey of
                            Just foreignKey ->
                                if foreignKey.tableName == parent.tableName then
                                    Just ( colName, parent.id )

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )
                |> List.head
        )


isColumnVisible : ( String, Column ) -> Bool
isColumnVisible ( _, column ) =
    case column.columnType of
        -- TextCol/UuidCol are ordinary scalar values (Postgres `text` is the
        -- idiomatic type for names/titles/emails, not just long-form
        -- content) and are exactly what a listing needs to identify a row
        -- — hiding them unconditionally left every table whose name/title
        -- column happens to be `text` showing nothing but a raw numeric id.
        -- Cell values are truncated with an ellipsis (see viewRowCells) so a
        -- genuinely long text value can't blow out the row.
        TextCol ->
            True

        UuidCol ->
            True

        JsonCol ->
            False

        ObjectCol ->
            False

        ArrayCol _ ->
            False

        OtherCol _ _ ->
            False

        _ ->
            True


hasErrors : Table -> CsvRow -> Bool
hasErrors table row =
    Dict.toList table.columns
        |> List.any
            (\( colName, column ) ->
                case Dict.get colName row.record |> Maybe.withDefault "" of
                    "" ->
                        column.required

                    value ->
                        not (valueParsesAs column.columnType value)
            )


valueParsesAs : ColumnType -> String -> Bool
valueParsesAs columnType value =
    case columnType of
        IntegerCol ->
            String.toInt value /= Nothing

        FloatCol ->
            String.toFloat value /= Nothing

        BoolCol ->
            List.member (String.toLower value)
                [ "true", "false", "t", "f", "1", "0" ]

        _ ->
            -- Dates/timestamps/text/uuid/json/etc. are left to the server;
            -- PostgREST's own error for those is already reasonably clear,
            -- and FK existence is already validated separately by the
            -- id-existence pre-check (see CsvUploadLoaded/FetchingIds).
            True


recordId : Table -> Record -> Maybe String
recordId table record =
    Schema.tablePrimaryKeyName table
        |> Maybe.andThen (\pkName -> Dict.get pkName record)
        |> Maybe.andThen Schema.valueToString
