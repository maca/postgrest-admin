module Main exposing (Error, main)

import Basics.Extra exposing (flip)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Dict
import Field exposing (Field)
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , autocomplete
        , checked
        , class
        , disabled
        , for
        , href
        , id
        , novalidate
        , step
        , target
        , type_
        , value
        )
import Html.Events as Events exposing (onClick, onInput, onSubmit)
import Http
import Inflect as String
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Postgrest.Client as PG
import PrimaryKey exposing (PrimaryKey)
import Record exposing (Record)
import Result
import Schema exposing (Schema)
import Schema.Definition as Definition exposing (Column(..), Definition)
import Set
import String.Extra as String
import Task
import Time.Extra as Time
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser)
import Value exposing (Value(..))


type Msg
    = SchemaFetched (Result Error Schema)
    | ListingFetched (Result Error (List Record))
    | RecordFetched (Result Error Record)
    | RecordSaved (Result Error Record)
    | RecordNew Record
    | RecordLinkClicked String String
    | InputChanged String Field
    | FormSubmitted
    | MessageDismissed
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failure (Result Error Never)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | BadSchema String


type Route
    = Listing (Maybe (List Record)) String
    | Edit (Maybe Record) EditionParams
    | New (Maybe Record) CreationParams
    | Root
    | NotFound


type alias RecordParams a =
    { a
        | resourcesName : String
        , changed : Bool
    }


type alias EditionParams =
    RecordParams { id : String }


type alias CreationParams =
    RecordParams {}


type Message
    = Confirmation String
    | Error String


type alias Model =
    { route : Route
    , key : Nav.Key
    , schema : Schema
    , host : String
    , jwt : PG.JWT
    , message : Maybe Message
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        jwt =
            PG.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoidG9kb191c2VyIn0.gm7S31FmVXlluCKr2ZBXBolkei2n06gNGJaw1IUJBEk"

        host =
            "http://localhost:3000"

        schema =
            Dict.fromList []

        model =
            { route = getRoute url
            , key = key
            , schema = schema
            , host = host
            , jwt = jwt
            , message = Nothing
            }
    in
    ( model, getSchema host )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFetched result ->
            case result of
                Ok schema ->
                    urlChanged { model | schema = schema }

                Err _ ->
                    ( model, Cmd.none )

        ListingFetched result ->
            case result of
                Ok records ->
                    case model.route of
                        Listing _ name ->
                            ( { model | route = Listing (Just <| records) name }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        RecordFetched result ->
            case result of
                Ok record ->
                    recordFetched record model

                Err _ ->
                    ( model, Cmd.none )

        RecordSaved result ->
            case ( result, model.route ) of
                ( Ok record, Edit _ _ ) ->
                    confirmation "Update succeed" model
                        |> recordFetched record

                ( Ok record, New _ { resourcesName } ) ->
                    let
                        url =
                            Url.absolute
                                [ resourcesName
                                , Record.id record |> Maybe.withDefault ""
                                ]
                                []
                    in
                    ( confirmation "Creation succeed" model
                    , Nav.pushUrl model.key <| url
                    )

                ( Err _, _ ) ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RecordNew record ->
            case model.route of
                New _ resourcesName ->
                    ( { model | route = New (Just record) resourcesName }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RecordLinkClicked resourcesName id ->
            ( model
            , Nav.pushUrl model.key <| Url.absolute [ resourcesName, id ] []
            )

        InputChanged name field ->
            case model.route of
                Edit (Just record) params ->
                    ( updateRecord Edit params name field record model
                    , Cmd.none
                    )

                New (Just record) params ->
                    ( updateRecord New params name field record model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FormSubmitted ->
            case model.route of
                Edit (Just record) params ->
                    ( { model | route = Edit (Just record) params }
                    , saveRecord params model record
                    )

                New (Just record) params ->
                    ( { model | route = New (Just record) params }
                    , createRecord params model record
                    )

                _ ->
                    ( model, Cmd.none )

        MessageDismissed ->
            ( { model | message = Nothing }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlChanged { model | route = getRoute url }

        Failure _ ->
            ( model, Cmd.none )


urlChanged : Model -> ( Model, Cmd Msg )
urlChanged model =
    case model.route of
        Listing Nothing resourcesName ->
            ( { model | message = Nothing }
            , fetchRecords resourcesName model
            )

        Edit _ params ->
            ( model, fetchRecord params model )

        New Nothing { resourcesName } ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    ( model
                    , Task.succeed (Definition.toRecord definition)
                        |> Task.perform RecordNew
                    )

                Nothing ->
                    ( model, fail <| BadSchema resourcesName )

        _ ->
            ( model, Cmd.none )


recordFetched : Record -> Model -> ( Model, Cmd Msg )
recordFetched record model =
    case model.route of
        Edit _ params ->
            ( { model | route = Edit (Just <| record) params }, Cmd.none )

        _ ->
            ( model, Cmd.none )


error : String -> Model -> Model
error message model =
    { model | message = Just <| Error message }


confirmation : String -> Model -> Model
confirmation message model =
    { model | message = Just <| Confirmation message }


updateRecord :
    (Maybe Record -> RecordParams a -> Route)
    -> RecordParams a
    -> String
    -> Field
    -> Record
    -> Model
    -> Model
updateRecord route params name field record model =
    let
        mrecord =
            Just <| Dict.insert name field record
    in
    { model
        | route = route mrecord { params | changed = True }
        , message = Nothing
    }



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Admin"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ div
        [ class "main-container" ]
        [ sideMenu model
        , div
            [ class "main-area" ]
            [ displayMessage model, displayMainContent model ]
        ]
    ]


sideMenu : Model -> Html Msg
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul [] (Dict.keys model.schema |> List.sort |> List.map menuItem) ]


menuItem : String -> Html Msg
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


displayMainContent : Model -> Html Msg
displayMainContent model =
    case model.route of
        Root ->
            text ""

        Listing maybeRecords name ->
            displayListing name maybeRecords model

        Edit mrecord params ->
            displayForm params mrecord model

        New mrecord params ->
            displayForm params mrecord model

        NotFound ->
            notFound


displayListing : String -> Maybe (List Record) -> Model -> Html Msg
displayListing resourcesName mrecords { schema } =
    case ( Dict.get resourcesName schema, mrecords ) of
        ( Just definition, Just records ) ->
            let
                fieldNames =
                    Dict.toList definition
                        |> List.sortWith sortColumns
                        |> List.map Tuple.first

                toHeader =
                    String.humanize >> text >> List.singleton >> th []
            in
            section
                []
                [ displayListHeader resourcesName
                , table []
                    [ thead [] [ tr [] <| List.map toHeader fieldNames ]
                    , tbody [] <|
                        List.map (displayRow resourcesName fieldNames) records
                    ]
                ]

        _ ->
            loading


displayListHeader : String -> Html Msg
displayListHeader resourcesName =
    header []
        [ h1 [] [ text <| String.humanize resourcesName ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ resourcesName, "new" ] []
                ]
                [ text <| "New " ++ String.singularize resourcesName ]
            ]
        ]


displayForm : RecordParams a -> Maybe Record -> Model -> Html Msg
displayForm { changed, resourcesName } mrecord model =
    case mrecord of
        Just record ->
            section
                []
                [ h1 []
                    [ recordLabel record
                        |> Maybe.withDefault "New"
                        |> (++) (String.humanize resourcesName ++ " - ")
                        |> text
                    ]
                , recordForm changed resourcesName record model
                ]

        Nothing ->
            notFound


recordForm : Bool -> String -> Record -> Model -> Html Msg
recordForm changed resourcesName record { schema } =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                fields =
                    Dict.toList record
                        |> List.sortWith sortFields
                        |> List.map valueInput
            in
            form
                [ class "resource-form"
                , autocomplete False
                , onSubmit FormSubmitted
                , novalidate True
                ]
                [ fieldset [] fields
                , fieldset []
                    [ button [ disabled (not changed) ] [ text "Save" ] ]
                ]

        Nothing ->
            notFound


valueInput : ( String, Field ) -> Html Msg
valueInput ( name, { value } as field ) =
    case value of
        PString maybe ->
            formInput [] "text" name field maybe

        PFloat maybe ->
            Maybe.map String.fromFloat maybe
                |> formInput [] "number" name field

        PInt maybe ->
            Maybe.map String.fromInt maybe
                |> formInput [] "number" name field

        PBool maybe ->
            let
                attrs =
                    Maybe.map (checked >> List.singleton) maybe
                        |> Maybe.withDefault []
            in
            formInput attrs "checkbox" name field Nothing

        PTime maybe ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) maybe
                |> formInput [] "datetime-local" name field

        _ ->
            text ""


displayMessage : Model -> Html Msg
displayMessage { message } =
    case message of
        Just (Error msg) ->
            displayMessageHelp "error" msg

        Just (Confirmation msg) ->
            displayMessageHelp "confirmation" msg

        Nothing ->
            text ""


displayMessageHelp : String -> String -> Html Msg
displayMessageHelp messageType message =
    div [ class "message", class messageType ]
        [ div []
            [ i [ class "icono-cross", onClick MessageDismissed ] []
            ]
        , p [] [ text message ]
        ]


recordLabel : Record -> Maybe String
recordLabel record =
    let
        mlabel =
            List.filterMap (recordLabelHelp record) recordIdentifiers
                |> List.head
    in
    case mlabel of
        Just _ ->
            mlabel

        Nothing ->
            Record.primaryKey record |> Maybe.map PrimaryKey.toString


recordLabelHelp : Record -> String -> Maybe String
recordLabelHelp record fieldName =
    case Dict.get fieldName record |> Maybe.map .value of
        Just (PString label) ->
            label

        _ ->
            Nothing


displayRow : String -> List String -> Record -> Html Msg
displayRow resourcesName names record =
    let
        toTd =
            displayValue resourcesName >> List.singleton >> td []

        id =
            Record.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , clickRecord resourcesName id
            ]


clickRecord : String -> String -> Html.Attribute Msg
clickRecord resourcesName id =
    let
        msg =
            RecordLinkClicked resourcesName id
    in
    Events.custom "click" <|
        Decode.map (EventConfig True True) (Decode.succeed msg)


displayValue : String -> Field -> Html Msg
displayValue resourcesName { value } =
    case value of
        PFloat (Just float) ->
            text <| String.fromFloat float

        PInt (Just int) ->
            text <| String.fromInt int

        PString (Just string) ->
            text string

        PBool (Just True) ->
            text "true"

        PBool (Just False) ->
            text "false"

        PTime (Just time) ->
            text <| Time.format time

        PForeignKey ( res, _ ) mlabel (Just primaryKey) ->
            recordLink res primaryKey mlabel

        PPrimaryKey (Just primaryKey) ->
            recordLink resourcesName primaryKey Nothing

        BadValue _ ->
            text "?"

        _ ->
            text "-"


formInput :
    List (Html.Attribute Msg)
    -> String
    -> String
    -> Field
    -> Maybe String
    -> Html Msg
formInput attributes t name field mstring =
    let
        input_ =
            Html.input
                (attributes
                    ++ [ onInput <| (InputChanged name << Field.update field)
                       , id name
                       , type_ t
                       , value <| Maybe.withDefault "" mstring
                       ]
                )
                []
    in
    div []
        [ label [ for name ] [ text <| String.humanize name ]
        , input_
        ]


recordLink : String -> PrimaryKey -> Maybe String -> Html Msg
recordLink resourcesName primaryKey mtext =
    let
        id =
            PrimaryKey.toString primaryKey
    in
    a
        [ href <| Url.absolute [ resourcesName, id ] []
        , target "_self"
        , clickRecord resourcesName id
        ]
        [ text <| Maybe.withDefault id mtext ]


notFound : Html Msg
notFound =
    text "Not found"


loading : Html Msg
loading =
    text ""


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]


sortColumns : ( String, Column ) -> ( String, Column ) -> Order
sortColumns ( name, Column _ val ) ( name_, Column _ val_ ) =
    sortValues ( name, val ) ( name_, val_ )


sortFields : ( String, Field ) -> ( String, Field ) -> Order
sortFields ( name, field ) ( name_, field_ ) =
    sortValues ( name, field.value ) ( name_, field_.value )


sortValues : ( String, Value ) -> ( String, Value ) -> Order
sortValues ( name, a ) ( _, b ) =
    case ( a, b ) of
        ( PPrimaryKey _, _ ) ->
            LT

        ( _, PPrimaryKey _ ) ->
            GT

        ( PForeignKey _ _ _, _ ) ->
            LT

        ( _, PForeignKey _ _ _ ) ->
            GT

        ( PString _, _ ) ->
            recordIdentifiers
                |> List.indexedMap (flip Tuple.pair)
                |> Dict.fromList
                |> Dict.get name
                |> Maybe.map (toFloat >> flip compare (1 / 0))
                |> Maybe.withDefault GT

        _ ->
            EQ



-- Subscriptions and Commands


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


fail : Error -> Cmd Msg
fail msg =
    Task.fail msg |> Task.attempt Failure


getSchema : String -> Cmd Msg
getSchema host =
    Http.get
        { url = host
        , expect = Http.expectString (SchemaFetched << decodeSchema)
        }


decodeSchema : Result Http.Error String -> Result Error Schema
decodeSchema result =
    Result.mapError HttpError result
        |> Result.andThen
            (Decode.decodeString Schema.decoder >> Result.mapError DecodeError)



-- Http interactions


fetchRecords : String -> Model -> Cmd Msg
fetchRecords resourcesName ({ schema, jwt } as model) =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                params =
                    [ PG.select <| selects schema definition ]
            in
            recordEndpoint resourcesName model definition
                |> PG.getMany
                |> PG.setParams params
                |> PG.toCmd jwt (ListingFetched << Result.mapError PGError)

        Nothing ->
            fail <| BadSchema resourcesName


fetchRecord : EditionParams -> Model -> Cmd Msg
fetchRecord { resourcesName, id } ({ schema, jwt } as model) =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                pkName =
                    Definition.primaryKeyName definition
                        |> Maybe.withDefault ""

                selectParams =
                    PG.select <| selects schema definition

                params =
                    [ selectParams
                    , PG.param pkName <| PG.eq <| PG.string id
                    ]
            in
            recordEndpoint resourcesName model definition
                |> PG.getOne
                |> PG.setParams params
                |> PG.toCmd jwt (RecordFetched << Result.mapError PGError)

        _ ->
            fail <| BadSchema resourcesName


createRecord : CreationParams -> Model -> Record -> Cmd Msg
createRecord { resourcesName } ({ schema, jwt } as model) record =
    case ( Dict.get resourcesName schema, Record.primaryKeyName record ) of
        ( Just definition, Just pkName ) ->
            let
                endpoint =
                    recordEndpoint resourcesName model definition
            in
            Record.encode record
                |> PG.postOne endpoint
                |> PG.setParams
                    [ PG.select <| selects schema definition ]
                |> PG.toCmd jwt
                    (RecordSaved << Result.mapError PGError)

        _ ->
            fail <| BadSchema resourcesName


saveRecord : EditionParams -> Model -> Record -> Cmd Msg
saveRecord { resourcesName, id } ({ schema, jwt } as model) record =
    case ( Dict.get resourcesName schema, Record.primaryKeyName record ) of
        ( Just definition, Just pkName ) ->
            let
                endpoint =
                    recordEndpoint resourcesName model definition

                pk =
                    PG.primaryKey ( pkName, PG.string )
            in
            Record.encode record
                |> PG.patchByPrimaryKey endpoint pk id
                |> PG.setParams
                    [ PG.select <| selects schema definition ]
                |> PG.toCmd jwt
                    (RecordSaved << Result.mapError PGError)

        _ ->
            fail <| BadSchema resourcesName


selects : Schema -> Definition -> List PG.Selectable
selects schema definition =
    let
        mapFun name =
            Dict.keys
                >> Set.fromList
                >> Set.intersect (recordIdentifiers |> Set.fromList)
                >> Set.toList
                >> PG.attributes
                >> PG.resource name

        resources ( name, _ ) =
            Dict.get name schema |> Maybe.map (mapFun name)

        filteMapFun (Column _ val) =
            Value.foreignKeyReference val
                |> Maybe.andThen resources
    in
    Dict.values definition
        |> List.filterMap filteMapFun
        |> (++) (Dict.keys definition |> List.map PG.attribute)


recordEndpoint : String -> Model -> Definition -> PG.Endpoint Record
recordEndpoint resourcesName { host } definition =
    Record.decoder recordIdentifiers definition
        |> PG.endpoint (Url.crossOrigin host [ resourcesName ] [])



-- Routes


getRoute : Url -> Route
getRoute url =
    Parser.parse routeParser url |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map routeParserHelp (Parser.string </> Parser.string)
        , Parser.map (Listing Nothing) Parser.string
        ]


routeParserHelp : String -> String -> Route
routeParserHelp resourcesName id =
    if id == "new" then
        New Nothing
            { resourcesName = resourcesName
            , changed = False
            }

    else
        Edit Nothing
            { resourcesName = resourcesName
            , changed = False
            , id = id
            }
