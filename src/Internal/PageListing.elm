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
    , update
    , view
    )

import Browser.Dom as Dom exposing (Viewport)
import Browser.Navigation as Nav
import Csv
import Dict
import File exposing (File)
import File.Select as Select
import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , div
        , h1
        , header
        , i
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
import Html.Events as Events exposing (on, onClick, onMouseDown, onMouseUp)
import Inflect as String
import Internal.Client exposing (selects)
import Internal.Cmd as AppCmd
import Internal.Download as Download exposing (Download, Format(..))
import Internal.Field as Field
import Internal.Schema exposing (Column, Constraint(..), Table)
import Internal.Search as Search exposing (Search)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG
import String.Extra as String
import Task
import Time
import Time.Extra as Time
import Url
import Url.Builder as Url exposing (QueryParameter)
import Utils.Task exposing (Error(..), attemptWithError)


type Page
    = Page (List Record)
    | Blank


type SortOrder
    = Asc String
    | Desc String
    | Unordered


type Msg
    = LoggedIn Client
    | Fetched (Result Error (List Record))
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
    | Downloaded Download
    | CsvUploadRequested
    | CsvUploadSelected File
    | CsvUploadLoaded String
    | CsvUploadPosted (Result Error Int)
    | ToggleSearchOpen
    | FetchFailed Error


type TextSelect
    = Enter
    | On
    | Off


type alias PageListing =
    { client : Client
    , key : Nav.Key
    , scrollPosition : Float
    , table : Table
    , pages : List Page
    , page : Int
    , order : SortOrder
    , search : Search
    , searchOpen : Bool
    , textSelect : TextSelect
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


init :
    { client : Client, table : Table }
    -> Url.Url
    -> Nav.Key
    -> ( PageListing, AppCmd.Cmd Msg )
init { client, table } url key =
    let
        order =
            Maybe.map parseQuery url.query
                |> Maybe.withDefault []
                |> (List.filter (Tuple.first >> (==) "order") >> List.head)
                |> Maybe.map (Tuple.second >> parseOrder)
                |> Maybe.withDefault Unordered

        listing =
            { client = client
            , key = key
            , page = 0
            , scrollPosition = 0
            , table = table
            , pages = []
            , order = order
            , search = Search.init table (url.query |> Maybe.withDefault "")
            , searchOpen = False
            , textSelect = Off
            }
    in
    ( listing, fetch listing )


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
fetch { search, page, table, order, client } =
    let
        pgOrder =
            Dict.toList table.columns
                |> List.filterMap (sortBy table.name order)

        params =
            [ PG.select (selects table)
            , PG.limit perPage
            , PG.offset (perPage * page)
            ]
    in
    Client.fetchRecordList
        { client = client
        , table = table
        , params = params ++ pgOrder ++ Search.toPGQuery search
        , expect = Client.expectRecordList Fetched table
        }


update : Msg -> PageListing -> ( PageListing, AppCmd.Cmd Msg )
update msg listing =
    case msg of
        LoggedIn client ->
            fetchListing
                { listing | client = client }

        Fetched (Ok records) ->
            ( { listing
                | page = listing.page + 1
                , pages =
                    case listing.pages of
                        Blank :: ps ->
                            if List.length records < perPage then
                                Blank :: Page records :: ps

                            else
                                Page records :: ps

                        _ ->
                            Page records :: listing.pages
              }
            , AppCmd.none
            )

        RecordLinkClicked tableName id ->
            ( listing
            , Url.absolute [ tableName, id ] []
                |> Nav.pushUrl listing.key
                |> AppCmd.wrap
            )

        Fetched (Err _) ->
            ( listing, AppCmd.none )

        ApplyFilters ->
            ( listing
            , Dom.setViewportOf listing.table.name 0 0
                |> Task.attempt (always Reload)
                |> AppCmd.wrap
            )

        Sort order ->
            ( { listing | order = order }
            , Dom.setViewportOf listing.table.name 0 0
                |> Task.attempt (always Reload)
                |> AppCmd.wrap
            )

        Reload ->
            ( listing
            , listingPath listing
                |> Nav.pushUrl listing.key
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

                Err err ->
                    ( listing
                    , Notification.error (Utils.Task.errorToString (DomError err))
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
            , Download.init format (listingPath listing)
                |> Download.fetch listing.client
                |> attemptWithError FetchFailed Downloaded
                |> AppCmd.wrap
            )

        Downloaded download ->
            ( listing
            , Download.save listing.table.name download
                |> AppCmd.wrap
            )

        CsvUploadRequested ->
            ( listing
            , Select.file [ "text/csv" ] CsvUploadSelected
                |> AppCmd.wrap
            )

        CsvUploadSelected file ->
            ( listing
            , Task.perform CsvUploadLoaded (File.toString file)
                |> AppCmd.wrap
            )

        CsvUploadLoaded string ->
            case Csv.parse string of
                Ok { headers, records } ->
                    let
                        json =
                            Encode.list
                                (List.zip headers
                                    >> Dict.fromList
                                    >> Encode.dict identity Encode.string
                                )
                                records
                    in
                    ( listing
                    , Client.upsert
                        { client = listing.client
                        , path = Url.absolute [ listing.table.name ] []
                        , value = json
                        , expect =
                            Result.map (always (List.length records))
                                >> CsvUploadPosted
                        }
                    )

                Err err ->
                    ( listing
                    , Notification.error (Utils.Task.errorToString (ParserError err))
                    )

        CsvUploadPosted (Ok count) ->
            ( listing
            , AppCmd.batch
                [ Dom.setViewportOf listing.table.name 0 0
                    |> Task.attempt (always Reload)
                    |> AppCmd.wrap
                , Notification.confirm
                    (String.fromInt count ++ " records where saved.")
                ]
            )

        CsvUploadPosted (Err err) ->
            ( listing
            , Notification.error (Utils.Task.errorToString err)
            )

        FetchFailed err ->
            ( listing
            , Notification.error (Utils.Task.errorToString err)
            )


listingPath : PageListing -> String
listingPath { order, table, search } =
    let
        queryParams =
            orderToQueryParams order

        baseUrl =
            Url.absolute [ table.name ]
                (orderToQueryParams order)

        filterQuery =
            Search.toPGQuery search |> PG.toQueryString

        joinChar =
            if List.isEmpty queryParams then
                "?"

            else
                "&"
    in
    [ baseUrl, filterQuery ]
        |> List.filterMap String.nonBlank
        |> String.join joinChar


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



-- View


view : PageListing -> Html Msg
view listing =
    let
        fields =
            Dict.toList listing.table.columns
                |> List.sortWith Field.compareTuple
                |> List.map Tuple.first

        body =
            tableHeading listing fields
                :: pagesFold listing fields [] 0 listing.pages
    in
    section
        [ class "resources-listing" ]
        [ listHeader listing.table.name
        , div
            [ id listing.table.name
            , class "resources-listing-results"
            , case listing.pages of
                Blank :: _ ->
                    class ""

                _ ->
                    Events.on "scroll" (Decode.succeed Scrolled)
            ]
            [ Html.table [] body
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
                    , button
                        [ class "button-clear"
                        , onClick CsvUploadRequested
                        ]
                        [ text "Upload CSV" ]
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
            , Html.map SearchChanged <|
                Search.view (isSearchVisible listing) listing.search
            ]
        ]


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


listHeader : String -> Html Msg
listHeader tableName =
    header []
        [ h1 [] [ text <| String.humanize tableName ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ tableName, "new" ] []
                ]
                [ text "New Record" ]
            ]
        ]


tableHeading : PageListing -> List String -> Html Msg
tableHeading listing fields =
    thead []
        [ tr [] <| List.map (tableHeader listing) fields ]


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
                            viewPage listing fields pageNum resources

                        Blank ->
                            text ""
            in
            pagesFold listing fields (elem :: acc) (pageNum + 1) rest


viewPage : PageListing -> List String -> Int -> List Record -> Html Msg
viewPage listing fields pageNum records =
    tbody [ id <| pageId pageNum ] <|
        List.map (row listing fields) records


row : PageListing -> List String -> Record -> Html Msg
row { table, textSelect } names record =
    let
        cell fieldName =
            Dict.get fieldName record.fields
                |> Maybe.map
                    (\field ->
                        td
                            []
                            [ field
                                |> Field.toHtml (clickRecord textSelect)
                                    table.name
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



-- Http interactions


pageId : Int -> String
pageId pageNum =
    "page-" ++ String.fromInt pageNum


perPage : Int
perPage =
    50


sortBy : String -> SortOrder -> ( String, Column ) -> Maybe PG.Param
sortBy tableName sort ( name, { constraint } ) =
    let
        colName =
            case constraint of
                ForeignKey params ->
                    params.labelColumnName
                        |> Maybe.map
                            (\columnName ->
                                String.join "_"
                                    [ tableName
                                    , params.tableName
                                    , columnName
                                    ]
                            )
                        |> Maybe.withDefault name

                _ ->
                    name
    in
    case sort of
        Asc f ->
            if f == name then
                Just <| PG.order [ PG.asc colName |> PG.nullslast ]

            else
                Nothing

        Desc f ->
            if f == name then
                Just <| PG.order [ PG.desc colName |> PG.nullslast ]

            else
                Nothing

        Unordered ->
            Nothing



-- Url parsing


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
        [ table, "asc" ] ->
            Asc table

        [ table, "desc" ] ->
            Desc table

        _ ->
            Unordered


orderToQueryParams : SortOrder -> List QueryParameter
orderToQueryParams order =
    case order of
        Asc column ->
            [ Url.string "order" <| column ++ "." ++ "asc" ]

        Desc column ->
            [ Url.string "order" <| column ++ "." ++ "desc" ]

        Unordered ->
            []
