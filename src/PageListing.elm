module PageListing exposing
    ( Msg
    , PageListing
    , ascendingBy
    , descendingBy
    , fetch
    , hideSearch
    , init
    , isSearchVisible
    , mapMsg
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
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Notification
import Postgrest.Client as PG
import Postgrest.Download as Download exposing (Download, Format(..))
import Postgrest.Field as Field
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema exposing (Column, Constraint(..), Table)
import Postgrest.Upload as Upload
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import Search exposing (Search)
import String.Extra as String
import Task exposing (Task)
import Time
import Time.Extra as Time
import Url
import Url.Builder as Url exposing (QueryParameter)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Page
    = Page (List Record)
    | Blank


type SortOrder
    = Asc String
    | Desc String
    | Unordered


type Msg
    = RecordLinkClicked String String
    | Fetched (List Record)
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
    | CsvFileRequested
    | CsvFileSelected File
    | CsvFileLoaded String
    | CsvFilePosted Int
    | ToggleSearchOpen
    | NotificationChanged Notification.Msg
    | FetchFailed Error


type TextSelect
    = Enter
    | On
    | Off


type alias PageListing =
    { resourcesName : String
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


init : String -> Maybe String -> Table -> PageListing
init resourcesName rawQuery table =
    let
        query =
            Maybe.map parseQuery rawQuery |> Maybe.withDefault []

        order =
            query
                |> (List.filter (Tuple.first >> (==) "order") >> List.head)
                |> Maybe.map (Tuple.second >> parseOrder)
                |> Maybe.withDefault Unordered
    in
    { resourcesName = resourcesName
    , page = 0
    , scrollPosition = 0
    , table = table
    , pages = []
    , order = order
    , search = Search.init table (rawQuery |> Maybe.withDefault "")
    , searchOpen = False
    , textSelect = Off
    }


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        FetchFailed err ->
            OuterMsg.RequestFailed err

        NotificationChanged innerMsg ->
            OuterMsg.NotificationChanged innerMsg

        _ ->
            OuterMsg.Pass


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


fetch : Client a -> PageListing -> Cmd Msg
fetch client listing =
    fetchTask client listing
        |> attemptWithError FetchFailed Fetched


fetchTask : Client a -> PageListing -> Task Error (List Record)
fetchTask client listing =
    let
        { search, resourcesName, page, table, order } =
            listing

        pgOrder =
            Dict.toList table.columns
                |> List.filterMap (sortBy resourcesName order)

        params =
            [ PG.select <| Client.selects table
            , PG.limit perPage
            , PG.offset (perPage * page)
            ]
    in
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            Client.fetchMany client table
                |> PG.setParams (params ++ pgOrder ++ Search.toPGQuery search)
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError


update : Client { a | key : Nav.Key } -> Msg -> PageListing -> ( PageListing, Cmd Msg )
update client msg listing =
    case msg of
        RecordLinkClicked resourcesName id ->
            ( listing
            , Nav.pushUrl client.key <| Url.absolute [ resourcesName, id ] []
            )

        Fetched records ->
            let
                pages =
                    case listing.pages of
                        Blank :: ps ->
                            if List.length records < perPage then
                                Blank :: Page records :: ps

                            else
                                Page records :: ps

                        _ ->
                            Page records :: listing.pages
            in
            ( { listing | page = listing.page + 1, pages = pages }
            , Cmd.none
            )

        ApplyFilters ->
            ( listing
            , Dom.setViewportOf listing.resourcesName 0 0
                |> Task.attempt (always Reload)
            )

        Sort order ->
            ( { listing | order = order }
            , Dom.setViewportOf listing.resourcesName 0 0
                |> Task.attempt (always Reload)
            )

        Reload ->
            ( listing, listingPath listing |> Nav.pushUrl client.key )

        Scrolled ->
            ( listing
            , Dom.getViewportOf listing.resourcesName
                |> Task.attempt ScrollInfo
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
                                , Cmd.none
                                )

                            _ ->
                                fetchListing client
                                    { listing
                                        | scrollPosition = viewport.viewport.y
                                        , pages = Blank :: listing.pages
                                    }

                    else
                        ( { listing | scrollPosition = viewport.viewport.y }
                        , Cmd.none
                        )

                Err _ ->
                    ( listing, Cmd.none )

        SearchChanged searchMsg ->
            Search.update searchMsg listing.search
                |> Tuple.mapFirst (\search -> { listing | search = search })
                |> Tuple.mapSecond (searchChanged searchMsg)

        ToggleSearchOpen ->
            ( { listing | searchOpen = not listing.searchOpen }, Cmd.none )

        SelectEnter ->
            ( { listing | textSelect = Enter }, Cmd.none )

        SelectOff ->
            ( { listing | textSelect = Off }, Cmd.none )

        SelectOn ->
            ( { listing | textSelect = On }, Cmd.none )

        DownloadRequested format ->
            ( listing
            , Download.init format (listingPath listing)
                |> Download.fetch client
                |> attemptWithError FetchFailed Downloaded
            )

        Downloaded download ->
            ( listing, Download.save listing.resourcesName download )

        CsvFileRequested ->
            ( listing, Select.file [ "text/csv" ] CsvFileSelected )

        CsvFileSelected file ->
            ( listing
            , Task.perform CsvFileLoaded (File.toString file)
            )

        CsvFileLoaded string ->
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

                        path =
                            Url.absolute [ listing.resourcesName ] []
                    in
                    ( listing
                    , Upload.post client path json
                        |> attemptWithError FetchFailed
                            (\_ -> CsvFilePosted (List.length records))
                    )

                Err _ ->
                    ( listing, Cmd.none )

        CsvFilePosted count ->
            ( listing
            , Cmd.batch
                [ Dom.setViewportOf listing.resourcesName 0 0
                    |> Task.attempt (always Reload)
                , (String.fromInt count ++ " records where saved.")
                    |> Notification.confirm
                    |> Task.perform NotificationChanged
                ]
            )

        NotificationChanged _ ->
            ( listing, Cmd.none )

        FetchFailed _ ->
            ( listing, Cmd.none )


listingPath : PageListing -> String
listingPath { order, resourcesName, search } =
    let
        queryParams =
            orderToQueryParams order

        baseUrl =
            Url.absolute [ resourcesName ]
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


fetchListing : Client a -> PageListing -> ( PageListing, Cmd Msg )
fetchListing client listing =
    ( listing, fetch client listing )


scrollingDown : Viewport -> PageListing -> Bool
scrollingDown { viewport } { scrollPosition } =
    scrollPosition < viewport.y


closeToBottom : Viewport -> Bool
closeToBottom { scene, viewport } =
    scene.height - viewport.y < (viewport.height * 2)


searchChanged : Search.Msg -> Cmd Search.Msg -> Cmd Msg
searchChanged msg cmd =
    if Search.isApplyMsg msg then
        Time.now |> Task.perform (always ApplyFilters)

    else
        Cmd.map SearchChanged cmd



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
        [ listHeader listing.resourcesName
        , div
            [ id listing.resourcesName
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
                        , onClick CsvFileRequested
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
listHeader resourcesName =
    header []
        [ h1 [] [ text <| String.humanize resourcesName ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ resourcesName, "new" ] []
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
row { resourcesName, textSelect } names record =
    let
        cell fieldName =
            Dict.get fieldName record.fields
                |> Maybe.map
                    (\field ->
                        td [] [ Field.toHtml (clickRecord textSelect) resourcesName field ]
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
            |> clickRecord textSelect resourcesName
        ]
        (List.filterMap cell names)


clickRecord : TextSelect -> String -> String -> Html.Attribute Msg
clickRecord textSelect resourcesName id =
    let
        msg =
            if textSelect == On then
                SelectOff

            else
                RecordLinkClicked resourcesName id
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
sortBy resourcesName sort ( name, { constraint } ) =
    let
        colName =
            case constraint of
                ForeignKey params ->
                    params.labelColumnName
                        |> Maybe.map
                            (\columnName ->
                                String.join "_"
                                    [ resourcesName
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