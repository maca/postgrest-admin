module PostgRestAdmin exposing
    ( Model, Msg
    , Config(..), Program, AppParams, configure
    , buildProgram, buildAppParams, applyConfiguration
    , appUrl
    , onLogin, onAuthFailed, onLogout, onExternalLogin
    , withHost, withLoginUrl, withJWT, withClientHeaders
    , withFlags
    , withMountPath, withRecordsPerPage
    , withMenuLinks, withFormFields
    , withTables, withTableAliases
    , withLoginBannerText
    )

{-|

@docs Model, Msg
@docs Config, Program, AppParams, configure
@docs buildProgram, buildAppParams, applyConfiguration
@docs appUrl


## Wiring

@docs onLogin, onAuthFailed, onLogout, onExternalLogin


## Client

@docs withHost, withLoginUrl, withJWT, withClientHeaders
@docs withFlags


## UI

@docs withMountPath, withRecordsPerPage
@docs withMenuLinks, withFormFields
@docs withTables, withTableAliases
@docs withLoginBannerText

-}

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Cmd as AppCmd exposing (AppCmd)
import Internal.PageDetail as PageDetail
import Internal.PageForm as PageForm
import Internal.PageListing as PageListing exposing (Model)
import Internal.Schema exposing (Schema)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Markdown
import PostgRestAdmin.Client as Client exposing (AuthScheme, Client, Error(..))
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import Postgrest.Client as PG
import Process
import String.Extra as String
import Task
import Url exposing (Protocol(..), Url)


{-| -}
type alias Model =
    { appUrl : AppUrl
    , query : Maybe String
    , baseUrl : { protocol : Protocol, host : String, port_ : Maybe Int }
    , key : Nav.Key
    , notification : Notification
    , authFormUrl : Url
    , authFormField : Field.Field Never
    , authFormStatus : AuthFormStatus
    , mountPath : MountPath
    , menuLinks : List ( String, String )
    , loginBannerText : Maybe String
    , recordsPerPage : Int
    , formFields : Dict String (List String)
    , configErrors : List String
    , listing : Maybe PageListing.Model
    , detail : Maybe PageDetail.Model
    , form : Maybe PageForm.Model
    , clientAttributes : ClientAttributes
    , client : Client
    }


type alias Params msg =
    { attributes : List (Model -> Model)
    , clientAttributes :
        List
            (ClientAttributes -> ClientAttributes)
    , onLogin : String -> Cmd msg
    , onAuthFailed : String -> Cmd msg
    , onExternalLogin :
        ({ path : String, accessToken : String }
         -> { path : String, accessToken : String }
        )
        -> Sub { path : String, accessToken : String }
    , onLogout : () -> Cmd msg
    , withFlags : Bool
    }


type alias ClientAttributes =
    { host : Url
    , authScheme : Client.AuthScheme
    , headers : List Http.Header
    , tables : List String
    , tableAliases : Dict String String
    }


type AuthFormStatus
    = Ready
    | Active
    | Submitting
    | Failure Client.Error


{-| -}
type Config msg
    = Config (Params msg) (List String)


{-| -}
type Msg
    = AuthFieldsChanged (Field.Msg Never)
    | AuthFormSubmitted
    | GotToken (Result Client.Error String)
    | SchemaFetched (Result Client.Error Schema)
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | NotificationDismiss
    | NotificationConfirm String
    | NotificationAlert String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoadPage
    | LoggedIn { path : String, accessToken : String }
    | LoggedOut
    | AuthRequired Error
    | NoOp


{-| -}
appUrl : Model -> AppUrl
appUrl model =
    model.appUrl


type Notification
    = Confirmation String
    | Error String
    | NoNotification


{-| Creates a default configuration.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configure
            |> PostgRestAdmin.host "http://localhost:3000"
            |> PostgRestAdmin.onLogin loginSuccess
            |> PostgRestAdmin.buildProgram

-}
configure : Config msg
configure =
    Config
        { attributes = []
        , clientAttributes = []
        , onLogin = always Cmd.none
        , onAuthFailed = always Cmd.none
        , onExternalLogin = always Sub.none
        , onLogout = always Cmd.none
        , withFlags = True
        }
        []


{-| An alias to elm's Platform.Program providing the type signature for a
PostgRestAdmin program.
-}
type alias Program =
    Platform.Program Decode.Value Model Msg


{-| Converts a Config into a Program.
-}
buildProgram : Config msg -> Program
buildProgram config =
    let
        params =
            buildAppParams
                { toInnerModel = identity
                , toOuterModel = always identity
                , toOuterMsg = identity
                , initModel = always identity
                }
                config
    in
    Browser.application
        { init = params.init
        , update = params.update
        , view = \model -> { title = "Admin", body = [ params.view model ] }
        , subscriptions = params.subscriptions
        , onUrlRequest = params.onUrlRequest
        , onUrlChange = params.onUrlChange
        }


{-| -}
type alias AppParams model outerMsg =
    { init : Decode.Value -> Url -> Nav.Key -> ( model, Cmd outerMsg )
    , view : model -> Html outerMsg
    , update : Msg -> model -> ( model, Cmd outerMsg )
    , subscriptions : model -> Sub outerMsg
    , onUrlRequest : Browser.UrlRequest -> outerMsg
    , onUrlChange : Url -> outerMsg
    }


{-| -}
buildAppParams :
    { toInnerModel : model -> Model
    , toOuterModel : Model -> model -> model
    , toOuterMsg : Msg -> outerMsg
    , initModel : Decode.Value -> Model -> model
    }
    -> Config msg
    -> AppParams model outerMsg
buildAppParams mappings ((Config confParams errs) as config) =
    { init =
        \value url key ->
            (case Decode.decodeValue (configDecoder config) value of
                Ok newConfig ->
                    newConfig

                Err err ->
                    Config confParams
                        (Decode.errorToString err :: errs)
            )
                |> initModel url key
                |> Tuple.mapFirst (mappings.initModel value)
                |> Tuple.mapSecond (Cmd.map mappings.toOuterMsg)
    , update =
        \msg outerModel ->
            update confParams msg (mappings.toInnerModel outerModel)
                |> Tuple.mapFirst (\innerModel -> mappings.toOuterModel innerModel outerModel)
                |> Tuple.mapSecond (Cmd.map mappings.toOuterMsg)
    , view =
        mappings.toInnerModel
            >> view
            >> Html.div []
            >> Html.map mappings.toOuterMsg
    , subscriptions =
        mappings.toInnerModel
            >> subscriptions confParams
            >> Sub.map mappings.toOuterMsg
    , onUrlRequest = mappings.toOuterMsg << LinkClicked
    , onUrlChange = mappings.toOuterMsg << UrlChanged
    }


initModel : Url -> Nav.Key -> Config msg -> ( Model, Cmd Msg )
initModel url key config =
    applyConfiguration config
        { appUrl = AppUrl.fromUrl url
        , query = url.query
        , baseUrl = { protocol = url.protocol, host = url.host, port_ = url.port_ }
        , key = key
        , notification = NoNotification
        , listing = Nothing
        , detail = Nothing
        , form = Nothing
        , configErrors = []
        , authFormField = authFormField
        , authFormStatus = Ready
        , authFormUrl = { defaultHost | path = "/rpc/login" }
        , mountPath = MountPath.fromString ""
        , menuLinks = []
        , loginBannerText = Nothing
        , recordsPerPage = 50
        , formFields = Dict.empty
        , clientAttributes =
            { host = defaultHost
            , authScheme = Client.unset
            , headers = []
            , tables = []
            , tableAliases = Dict.empty
            }
        , client = Client.init clientAttributes
        }


{-| -}
applyConfiguration : Config msg -> Model -> ( Model, Cmd Msg )
applyConfiguration (Config config errors) model =
    let
        clientAttrs =
            List.foldl (<|)
                model.clientAttributes
                config.clientAttributes

        model2 =
            List.foldl (<|)
                { model
                    | configErrors = errors
                    , client = Client.init clientAttrs
                    , clientAttributes = clientAttrs
                }
                config.attributes
    in
    ( model2
    , changeActionCmd model2
    )


defaultHost : Url.Url
defaultHost =
    { protocol = Http
    , host = "localhost"
    , port_ = Just 3000
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


clientAttributes :
    { host : Url
    , authScheme : AuthScheme
    , headers : List Http.Header
    , tables : List String
    , tableAliases : Dict String String
    }
clientAttributes =
    { host = defaultHost
    , authScheme = Client.unset
    , headers = []
    , tables = []
    , tableAliases = Dict.empty
    }


authFormField : Field.Field Never
authFormField =
    Field.group []
        [ Field.text
            [ Field.name "email"
            , Field.label "Login"
            , Field.required True
            ]
        , Field.password
            [ Field.name "password"
            , Field.label "Password"
            , Field.required True
            ]
        ]


requestToken : Model -> Cmd Msg
requestToken model =
    Task.attempt GotToken
        (Http.task
            { method = "POST"
            , headers = []
            , url = Url.toString model.authFormUrl
            , body =
                Http.jsonBody
                    (Parse.parse Parse.json model.authFormField
                        |> Result.withDefault Encode.null
                    )
            , resolver = Client.jsonResolver (Decode.field "token" Decode.string)
            , timeout = Nothing
            }
        )



-- UPDATE


update : Params msg -> Msg -> Model -> ( Model, Cmd Msg )
update config msg ({ client } as model) =
    case msg of
        AuthFieldsChanged toInnerMsg ->
            ( { model
                | authFormField = Field.update toInnerMsg model.authFormField
                , authFormStatus = Active
              }
            , Cmd.none
            )

        AuthFormSubmitted ->
            ( { model | authFormStatus = Submitting }
            , requestToken model
            )

        GotToken (Ok tokenStr) ->
            let
                updatedClient =
                    { client | authScheme = Client.Jwt (PG.jwt tokenStr) }
            in
            ( { model
                | client = updatedClient
                , authFormField = authFormField
              }
            , Cmd.batch
                [ config.onLogin tokenStr |> Cmd.map (always NoOp)
                , Client.fetchSchema updatedClient
                    |> Task.attempt SchemaFetched
                ]
            )

        GotToken (Err error) ->
            ( { model | authFormStatus = Failure error }
            , Cmd.none
            )

        SchemaFetched (Ok schema) ->
            let
                updatedModel =
                    { model | client = { client | schema = schema } }
            in
            ( updatedModel, changeActionCmd updatedModel )

        SchemaFetched (Err err) ->
            ( { model
                | client = Client.logout model.client
                , authFormStatus = Failure err
              }
            , Cmd.none
            )

        PageListingChanged childMsg ->
            case model.listing of
                Just listing ->
                    let
                        ( updatedListing, appCmd ) =
                            PageListing.update childMsg listing
                    in
                    ( { model | listing = Just updatedListing }
                    , mapCmd PageListingChanged appCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        PageDetailChanged childMsg ->
            case model.detail of
                Just detail ->
                    let
                        ( updatedDetail, cmd ) =
                            PageDetail.update childMsg detail
                    in
                    ( { model | detail = Just updatedDetail }
                    , mapCmd PageDetailChanged cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        PageFormChanged childMsg ->
            case model.form of
                Just form ->
                    let
                        ( updatedForm, cmd ) =
                            PageForm.update childMsg form
                    in
                    ( { model | form = Just updatedForm }
                    , mapCmd PageFormChanged cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        NotificationDismiss ->
            ( { model | notification = NoNotification }
            , Cmd.none
            )

        NotificationConfirm message ->
            ( { model | notification = Confirmation message }
            , Process.sleep 5000
                |> Task.perform (always NotificationDismiss)
            )

        NotificationAlert message ->
            ( { model | notification = Error message }
            , Cmd.none
            )

        LinkClicked (Browser.Internal url) ->
            ( model
            , Cmd.batch
                [ Nav.pushUrl model.key (Url.toString url)
                , Task.succeed NotificationDismiss
                    |> Task.perform identity
                ]
            )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                updated =
                    { model
                        | appUrl = AppUrl.fromUrl url
                        , query = url.query
                        , baseUrl =
                            { protocol = url.protocol
                            , host = url.host
                            , port_ = url.port_
                            }
                    }
            in
            if (currentUrl model).path /= url.path then
                ( updated
                , Task.succeed () |> Task.perform (always LoadPage)
                )

            else
                ( updated, Cmd.none )

        LoadPage ->
            initPageModels config model

        LoggedIn updateParams ->
            ( { model | client = Client.updateJwt updateParams.accessToken model.client }
            , Nav.pushUrl model.key updateParams.path
            )

        LoggedOut ->
            ( { model | client = Client.logout model.client }
            , Cmd.map (always NoOp) (config.onLogout ())
            )

        AuthRequired err ->
            ( { model
                | client = Client.logout model.client
                , authFormStatus = Failure err
                , notification = NoNotification
              }
            , Cmd.map (always NoOp)
                (config.onAuthFailed (urlToPath (currentUrl model)))
            )

        NoOp ->
            ( model, Cmd.none )


changeActionCmd : Model -> Cmd Msg
changeActionCmd model =
    if Client.toJwtString model.client == Nothing then
        Cmd.none

    else if Client.schemaIsLoaded model.client then
        case appPath model of
            [] ->
                resources model.client
                    |> List.head
                    |> Maybe.map
                        (MountPath.path model.mountPath >> Nav.pushUrl model.key)
                    |> Maybe.withDefault Cmd.none

            _ ->
                -- A non-root path here means the app just booted straight
                -- into a deep link (page load/reload), not a Browser.
                -- application `UrlChanged` (which only fires on *subsequent*
                -- navigation, never for the initial URL). Without this, the
                -- schema loads but the listing/detail page is never fetched
                -- and the content area stays empty forever.
                Task.succeed () |> Task.perform (always LoadPage)

    else
        Task.attempt SchemaFetched
            (Client.fetchSchema model.client)



-- VIEW


view : Model -> List (Html Msg)
view model =
    case model.configErrors of
        [] ->
            case model.client.authScheme of
                Client.Unset ->
                    [ viewAuthForm model ]

                _ ->
                    [ Html.div
                        []
                        [ Html.div
                            [ Attrs.class "main-container" ]
                            [ sideMenu model
                            , Html.div
                                [ Attrs.class "main-area" ]
                                [ viewNotification model.notification
                                , mainContent model
                                ]
                            ]
                        ]
                    ]

        errors ->
            [ Html.div
                [ Attrs.style "padding" "20px"
                , Attrs.style "font-family" "monospace"
                ]
                [ Html.h1 [] [ Html.text "Configuration Errors" ]
                , Html.ul
                    [ Attrs.style "color" "red"
                    , Attrs.style "line-height" "1.6"
                    ]
                    (List.map
                        (\error ->
                            Html.li [] [ Html.text error ]
                        )
                        errors
                    )
                ]
            ]


viewAuthForm : Model -> Html Msg
viewAuthForm model =
    Html.div
        [ Attrs.class "auth-modal overlay" ]
        [ Html.div
            [ Attrs.class "auth-form" ]
            [ errorMessage model.authFormStatus
            , Html.form
                [ Attrs.class "auth-form"
                , Events.onSubmit AuthFormSubmitted
                ]
                [ Field.toHtml AuthFieldsChanged model.authFormField
                , Html.button
                    [ Attrs.disabled
                        (Parse.parse Parse.json model.authFormField
                            |> Result.map (\_ -> False)
                            |> Result.withDefault True
                        )
                    ]
                    [ Html.text "Login" ]
                ]
            , case model.loginBannerText of
                Just text ->
                    Markdown.toHtml [ Attrs.class "login-banner" ] text

                Nothing ->
                    Html.text ""
            ]
        ]


errorMessage : AuthFormStatus -> Html Msg
errorMessage status =
    case status of
        Failure error ->
            Html.div
                [ Attrs.class "form-error-message" ]
                (case error of
                    Client.Forbidden ->
                        [ Html.text """You may have entered the wrong password, please try again.""" ]

                    Client.Unauthorized ->
                        [ Html.text "Please sign in to continue." ]

                    Client.BadStatus statusCode err ->
                        [ Html.text "The server responded with an error: "
                        , Html.pre []
                            [ Html.text
                                (err |> Maybe.withDefault (String.fromInt statusCode))
                            ]
                        ]

                    Client.NetworkError ->
                        [ Html.text """There was an issue reaching the server, please try again later.""" ]

                    _ ->
                        [ Html.text (Client.errorToString error) ]
                )

        _ ->
            Html.div
                [ Attrs.class "form-error-message"
                , Attrs.style "visibility" "hidden"
                ]
                []


sideMenu : Model -> Html Msg
sideMenu model =
    Html.div
        [ Attrs.class "side-menu" ]
        [ Html.aside
            [ Attrs.class "resources-menu" ]
            [ Html.ul [] (List.map (menuItem model.mountPath) (resources model.client))
            , Html.ul [] (List.map extraMenuItem model.menuLinks)
            ]
        , Html.div
            [ Attrs.class "account-management" ]
            [ Html.button
                [ Events.onClick LoggedOut
                , Attrs.class "button button-clear"
                ]
                [ Html.i [ Attrs.class "gg-log-out" ] []
                , Html.text "Logout"
                ]
            ]
        ]


menuItem : MountPath -> String -> Html Msg
menuItem mount name =
    Html.li
        []
        [ Html.a
            [ Attrs.href (path mount name) ]
            [ Html.text (String.toTitleCase (String.humanize name)) ]
        ]


extraMenuItem : ( String, String ) -> Html Msg
extraMenuItem ( linkText, url ) =
    Html.li
        []
        [ Html.a
            [ Attrs.href url ]
            [ Html.text (String.toTitleCase linkText) ]
        ]


viewNotification : Notification -> Html Msg
viewNotification notification =
    case notification of
        Confirmation text ->
            viewNotificationHelp "confirmation" text

        Error text ->
            viewNotificationHelp "error" text

        NoNotification ->
            Html.text ""


viewNotificationHelp : String -> String -> Html Msg
viewNotificationHelp notificationType message =
    Html.div
        [ Attrs.class "notification"
        , Attrs.class notificationType
        ]
        [ Html.pre [] [ Html.text message ]
        , Html.div [ Attrs.class "close" ]
            [ Html.i
                [ Attrs.class "icono-cross"
                , Events.onClick NotificationDismiss
                ]
                []
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    let
        mountSegments =
            MountPath.segments model.mountPath

        -- Remove mount path segments from the beginning
        pathAfterMount =
            List.drop (List.length mountSegments) model.appUrl.path
    in
    case pathAfterMount of
        [] ->
            Html.text ""

        [ _, "new" ] ->
            case model.form of
                Just form ->
                    Html.map PageFormChanged (PageForm.view form)

                Nothing ->
                    Html.text ""

        [ _, _, _, "new" ] ->
            case model.form of
                Just form ->
                    Html.map PageFormChanged (PageForm.view form)

                Nothing ->
                    Html.text ""

        [ _, _, "edit" ] ->
            case model.form of
                Just form ->
                    Html.map PageFormChanged (PageForm.view form)

                Nothing ->
                    Html.text ""

        [ _ ] ->
            case model.listing of
                Just listing ->
                    Html.map PageListingChanged (PageListing.view listing)

                Nothing ->
                    Html.text ""

        [ _, _ ] ->
            case model.detail of
                Just detail ->
                    Html.map PageDetailChanged (PageDetail.view detail)

                Nothing ->
                    Html.text ""

        [ _, _, _ ] ->
            case model.listing of
                Just listing ->
                    Html.map PageListingChanged (PageListing.view listing)

                Nothing ->
                    Html.text ""

        _ ->
            Html.text "Not found"



-- SUBSCRIPTIONS


subscriptions : Params msg -> Model -> Sub Msg
subscriptions config model =
    Sub.batch
        [ case model.listing of
            Just pageListing ->
                Sub.map PageListingChanged
                    (PageListing.subscriptions pageListing)

            Nothing ->
                Sub.none
        , Sub.map LoggedIn (config.onExternalLogin identity)
        ]



-- ROUTES


appPath : Model -> List String
appPath model =
    List.drop
        (List.length (MountPath.segments model.mountPath))
        model.appUrl.path


initPageModels : Params msg -> Model -> ( Model, Cmd Msg )
initPageModels config model =
    case appPath model of
        -- /posts/new
        [ resource, "new" ] ->
            case Dict.get resource model.client.schema of
                Just table ->
                    PageForm.init
                        { client = model.client
                        , navKey = model.key
                        , mountPath = model.mountPath
                        , parent = Nothing
                        , table = table
                        , id = Nothing
                        }
                        |> Tuple.mapFirst (\formModel -> { model | form = Just formModel })
                        |> Tuple.mapSecond (mapCmd PageFormChanged)

                Nothing ->
                    ( model, Cmd.none )

        -- /posts/1/comments/new
        [ parentResource, parentId, resource, "new" ] ->
            case Dict.get resource model.client.schema of
                Just table ->
                    PageForm.init
                        { client = model.client
                        , navKey = model.key
                        , mountPath = model.mountPath
                        , parent = Just { tableName = parentResource, id = parentId }
                        , table = table
                        , id = Nothing
                        }
                        |> Tuple.mapFirst (\formModel -> { model | form = Just formModel })
                        |> Tuple.mapSecond (mapCmd PageFormChanged)

                Nothing ->
                    ( model, Cmd.none )

        -- /posts/1/edit
        [ resource, id, "edit" ] ->
            case Dict.get resource model.client.schema of
                Just table ->
                    PageForm.init
                        { client = model.client
                        , navKey = model.key
                        , mountPath = model.mountPath
                        , parent = Nothing
                        , table = table
                        , id = Just id
                        }
                        |> Tuple.mapFirst (\formModel -> { model | form = Just formModel })
                        |> Tuple.mapSecond (mapCmd PageFormChanged)

                Nothing ->
                    ( model, Cmd.none )

        -- /posts
        [ resource ] ->
            case Dict.get resource model.client.schema of
                Just table ->
                    PageListing.init
                        { client = model.client
                        , mountPath = model.mountPath
                        , table = table
                        , parent = Nothing
                        , recordsPerPage = model.recordsPerPage
                        }
                        (currentUrl model)
                        model.key
                        |> Tuple.mapFirst (\listingModel -> { model | listing = Just listingModel })
                        |> Tuple.mapSecond (mapCmd PageListingChanged)

                Nothing ->
                    ( model, Cmd.none )

        -- /posts/1
        [ resource, id ] ->
            case Dict.get resource model.client.schema of
                Just table ->
                    PageDetail.init
                        { client = model.client
                        , mountPath = model.mountPath
                        , table = table
                        , id = id
                        }
                        model.key
                        |> Tuple.mapFirst (\detailModel -> { model | detail = Just detailModel })
                        |> Tuple.mapSecond (mapCmd PageDetailChanged)

                Nothing ->
                    ( model, Cmd.none )

        -- /posts/1/comments
        [ parentResource, parentId, resource ] ->
            case Dict.get resource model.client.schema of
                Just table ->
                    PageListing.init
                        { client = model.client
                        , mountPath = model.mountPath
                        , table = table
                        , parent = Just { tableName = parentResource, id = parentId }
                        , recordsPerPage = model.recordsPerPage
                        }
                        (currentUrl model)
                        model.key
                        |> Tuple.mapFirst (\listingModel -> { model | listing = Just listingModel })
                        |> Tuple.mapSecond (mapCmd PageListingChanged)

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- UTILS


resources : Client -> List String
resources client =
    if List.isEmpty client.tables then
        Dict.keys client.schema |> List.sort

    else
        client.tables


mapCmd : (a -> Msg) -> AppCmd a -> Cmd Msg
mapCmd tagger appCmd =
    case appCmd of
        AppCmd.ChildCmd cmd ->
            Cmd.map tagger cmd

        AppCmd.Batch cmds ->
            Cmd.batch (List.map (mapCmd tagger) cmds)

        AppCmd.ClientError error ->
            Cmd.batch
                [ Task.succeed (Client.errorToString error)
                    |> Task.perform NotificationAlert
                , case error of
                    Unauthorized ->
                        Task.succeed (AuthRequired error)
                            |> Task.perform identity

                    _ ->
                        Cmd.none
                ]

        AppCmd.Error error ->
            Task.succeed error
                |> Task.perform NotificationAlert

        AppCmd.NotificationConfirm message ->
            Task.succeed message
                |> Task.perform NotificationConfirm

        AppCmd.NotificationDismiss ->
            Task.succeed NotificationDismiss
                |> Task.perform identity


currentUrl : Model -> Url
currentUrl model =
    { protocol = model.baseUrl.protocol
    , host = model.baseUrl.host
    , port_ = model.baseUrl.port_
    , path = "/" ++ String.join "/" model.appUrl.path
    , query = model.query
    , fragment = model.appUrl.fragment
    }


urlToPath : Url -> String
urlToPath url =
    [ Just url.path, url.query ]
        |> List.filterMap identity
        |> String.join "?"



-- CONFIG


{-| -}
configDecoder : Config msg -> Decoder (Config msg)
configDecoder config =
    Decode.succeed config
        |> Decode.andThen hostDecoder
        |> Decode.andThen loginUrlDecoder
        |> Decode.andThen jwtDecoder
        |> Decode.andThen clientHeadersDecoder
        |> Decode.andThen mountPathDecoder
        |> Decode.andThen recordsPerPageDecoder
        |> Decode.andThen menuLinksDecoder
        |> Decode.andThen formFieldsDecoder
        |> Decode.andThen tablesDecoder
        |> Decode.andThen tableAliasesDecoder
        |> Decode.andThen loginBannerTextDecoder


{-| Callback triggered with a JWT string on successful login.
Typically used to persist the JWT to session storage.

    port loginSuccess : String -> Cmd msg

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.onLogin loginSuccess
            |> PostgRestAdmin.buildProgram

Then subscribe to the corresponding port.

    app.ports.loginSuccess.subscribe(jwt => {
      sessionStorage.setItem("jwt", jwt)
    });

-}
onLogin : (String -> Cmd msg) -> Config msg -> Config msg
onLogin f (Config conf errors) =
    Config { conf | onLogin = f } errors


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use this to perform external authentication.

    port authFailure : String -> Cmd msg

    port tokenReceiver :
        ({ path : String, accessToken : String } -> msg)
        -> Sub msg

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.onAuthFailed authFailure
            |> PostgRestAdmin.onExternalLogin tokenReceiver
            |> PostgRestAdmin.buildProgram

Then wire to the corresponding ports.

    app.ports.authFailure.subscribe(requestedPath => {
        authenticate(requestedPath).then((accessToken) => {
            app.ports.tokenReceiver.send({
                path : requestedPath,
                accessToken : accessToken
            })
        })
    });

-}
onAuthFailed : (String -> Cmd msg) -> Config msg -> Config msg
onAuthFailed f (Config conf errors) =
    Config { conf | onAuthFailed = f } errors


{-| Subscribe to receive a JWT and a redirect path when login with an external
provider.

See [onAuthFailed](#onAuthFailed).

-}
onExternalLogin :
    (({ path : String, accessToken : String }
      -> { path : String, accessToken : String }
     )
     -> Sub { path : String, accessToken : String }
    )
    -> Config msg
    -> Config msg
onExternalLogin sub (Config conf errors) =
    Config { conf | onExternalLogin = sub } errors


{-| Callback triggered when the user logs out.
You can use this to perform cleanup or external logout operations.

    port logout : () -> Cmd msg

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.onLogout logout
            |> PostgRestAdmin.buildProgram

Then subscribe to the corresponding port.

    app.ports.logout.subscribe(_ => {
        externalLogout()
    });

-}
onLogout : (() -> Cmd msg) -> Config msg -> Config msg
onLogout f (Config conf errors) =
    Config { conf | onLogout = f } errors


{-| Specify the PostgREST host.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withHost "http://localhost:3000"
            |> PostgRestAdmin.buildProgram

-}
withHost : String -> Config msg -> Config msg
withHost urlStr (Config conf errors) =
    case Url.fromString urlStr of
        Just u ->
            let
                clientAttrs =
                    (\attrs -> { attrs | host = u })
                        :: conf.clientAttributes

                -- If the app never calls `withLoginUrl`, the built-in login
                -- form keeps posting to `defaultHost` (localhost:3000)
                -- forever. Co-opt `authFormUrl` here too, but only while it
                -- still holds that untouched default, so an explicit
                -- `withLoginUrl` call always wins regardless of call order.
                attributes =
                    (\model ->
                        if model.authFormUrl == { defaultHost | path = "/rpc/login" } then
                            { model | authFormUrl = { u | path = "/rpc/login" } }

                        else
                            model
                    )
                        :: conf.attributes
            in
            Config
                { conf
                    | clientAttributes = clientAttrs
                    , attributes = attributes
                }
                errors

        Nothing ->
            Config conf (("`Config.host` was given an invalid URL: " ++ urlStr) :: errors)


{-| If set to False no attempt to decode and apply flags will be made.
Defaults to true.


    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withFlags False
            |> PostgRestAdmin.buildProgram

    -- Flags are ignored

-}
withFlags : Bool -> Config msg -> Config msg
withFlags decodesFlags (Config conf errors) =
    Config { conf | withFlags = decodesFlags } errors


hostDecoder : Config msg -> Decoder (Config msg)
hostDecoder config =
    Decode.maybe (Decode.field "host" Decode.string)
        |> Decode.map
            (Maybe.map (\urlStr -> withHost urlStr config)
                >> Maybe.withDefault config
            )


{-| Specify the login URL for form authentication.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withLoginUrl "http://localhost:3000/rpc/login"
            |> PostgRestAdmin.buildProgram

-}
withLoginUrl : String -> Config msg -> Config msg
withLoginUrl urlStr (Config conf errors) =
    case Url.fromString urlStr of
        Just u ->
            Config
                { conf
                    | attributes =
                        (\model -> { model | authFormUrl = u })
                            :: conf.attributes
                }
                errors

        Nothing ->
            Config conf (("`Config.loginUrl` was given an invalid URL: " ++ urlStr) :: errors)


loginUrlDecoder : Config msg -> Decoder (Config msg)
loginUrlDecoder config =
    Decode.maybe (Decode.field "loginUrl" Decode.string)
        |> Decode.map
            (Maybe.map (\urlStr -> withLoginUrl urlStr config)
                >> Maybe.withDefault config
            )


{-| Set a JWT to authenticate PostgREST requests. You can set an initial JWT
using this attribute.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withJWT "8abf3a...9ac36d"
            |> PostgRestAdmin.buildProgram

-}
withJWT : String -> Config msg -> Config msg
withJWT tokenStr (Config conf errors) =
    Config
        { conf
            | clientAttributes =
                (\attrs -> { attrs | authScheme = Client.jwt tokenStr })
                    :: conf.clientAttributes
        }
        errors


jwtDecoder : Config msg -> Decoder (Config msg)
jwtDecoder config =
    Decode.maybe (Decode.field "jwt" Decode.string)
        |> Decode.map
            (Maybe.map (\tokenStr -> withJWT tokenStr config)
                >> Maybe.withDefault config
            )


{-| Set default HTTP headers to be included in all Client requests. This is
useful for setting headers like `Accept-Profile` or `Content-Profile` when
working with PostgREST schemas.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withClientHeaders
                [ Http.header "Accept-Profile" "bluebox"
                , Http.header "Content-Profile" "bluebox"
                ]
            |> PostgRestAdmin.buildProgram

-}
withClientHeaders : List Http.Header -> Config msg -> Config msg
withClientHeaders headers (Config conf errors) =
    Config
        { conf
            | clientAttributes =
                (\attrs -> { attrs | headers = headers })
                    :: conf.clientAttributes
        }
        errors


clientHeadersDecoder : Config msg -> Decoder (Config msg)
clientHeadersDecoder config =
    Decode.maybe
        (Decode.field "clientHeaders" (Decode.dict Decode.string)
            |> Decode.map
                (\dict ->
                    Dict.toList dict
                        |> List.map (\( name, value ) -> Http.header name value)
                )
        )
        |> Decode.map
            (Maybe.map (\headers -> withClientHeaders headers config)
                >> Maybe.withDefault config
            )


{-| Specify a path prefix for all routes, in case the app is not mounted in the
root path.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withMountPath "/back-office"
            |> PostgRestAdmin.buildProgram

-}
withMountPath : String -> Config msg -> Config msg
withMountPath p (Config conf errors) =
    Config
        { conf
            | attributes =
                (\model -> { model | mountPath = MountPath.fromString p })
                    :: conf.attributes
        }
        errors


mountPathDecoder : Config msg -> Decoder (Config msg)
mountPathDecoder config =
    Decode.maybe (Decode.field "mountPath" Decode.string)
        |> Decode.map
            (Maybe.map (\p -> withMountPath p config)
                >> Maybe.withDefault config
            )


{-| Set the number of records to display per page in listing views.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withRecordsPerPage 100
            |> PostgRestAdmin.buildProgram

-}
withRecordsPerPage : Int -> Config msg -> Config msg
withRecordsPerPage count (Config conf errors) =
    Config
        { conf
            | attributes =
                (\model -> { model | recordsPerPage = count })
                    :: conf.attributes
        }
        errors


recordsPerPageDecoder : Config msg -> Decoder (Config msg)
recordsPerPageDecoder config =
    Decode.maybe (Decode.field "recordsPerPage" Decode.int)
        |> Decode.map
            (Maybe.map (\count -> withRecordsPerPage count config)
                >> Maybe.withDefault config
            )


{-| Pass a list of links to display in the side menu. The list consists of
tuples of the link text and a url.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withMenuLinks [ ( "Api Docs", "/api/docs" ) ]
            |> PostgRestAdmin.buildProgram

-}
withMenuLinks : List ( String, String ) -> Config msg -> Config msg
withMenuLinks links (Config conf errors) =
    Config
        { conf
            | attributes =
                (\model -> { model | menuLinks = links })
                    :: conf.attributes
        }
        errors


menuLinksDecoder : Config msg -> Decoder (Config msg)
menuLinksDecoder config =
    Decode.maybe
        (Decode.field "menuLinks"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "text" Decode.string)
                    (Decode.field "url" Decode.string)
                )
            )
        )
        |> Decode.map
            (Maybe.map (\links -> withMenuLinks links config)
                >> Maybe.withDefault config
            )


{-| Specify which fields should be present in the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withFormFields "posts" [ "id", "title", "content" ]
            |> PostgRestAdmin.buildProgram

-}
withFormFields : String -> List String -> Config msg -> Config msg
withFormFields tableName fields (Config conf errors) =
    Config
        { conf
            | attributes =
                (\model -> { model | formFields = Dict.insert tableName fields model.formFields })
                    :: conf.attributes
        }
        errors


formFieldsDecoder : Config msg -> Decoder (Config msg)
formFieldsDecoder config =
    Decode.maybe
        (Decode.field "formFields"
            (Decode.dict (Decode.list Decode.string))
        )
        |> Decode.map
            (Maybe.map (Dict.foldl withFormFields config)
                >> Maybe.withDefault config
            )


{-| Pass a list of table names to restrict the editable resources, also sets the
order of the left resources menu.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withTables [ "posts", "comments" ]
            |> PostgRestAdmin.buildProgram

-}
withTables : List String -> Config msg -> Config msg
withTables tableNames (Config conf errors) =
    Config
        { conf
            | clientAttributes =
                (\attrs -> { attrs | tables = tableNames })
                    :: conf.clientAttributes
        }
        errors


tablesDecoder : Config msg -> Decoder (Config msg)
tablesDecoder config =
    Decode.maybe (Decode.field "tables" (Decode.list Decode.string))
        |> Decode.map
            (Maybe.map (\tableNames -> withTables tableNames config)
                >> Maybe.withDefault config
            )


{-| Rename a table referenced in a foreign key. PostgREST OpenAPI generated docs
confuse tables with views when describing the foreign key for a resource,
because of this some links might be incorrectly generated.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withTableAliases
                (Dict.fromList [ ( "published_posts", "posts" ) ])
            |> PostgRestAdmin.buildProgram

-}
withTableAliases : Dict String String -> Config msg -> Config msg
withTableAliases aliases (Config conf errors) =
    Config
        { conf
            | clientAttributes =
                (\attrs -> { attrs | tableAliases = aliases })
                    :: conf.clientAttributes
        }
        errors


tableAliasesDecoder : Config msg -> Decoder (Config msg)
tableAliasesDecoder config =
    Decode.maybe (Decode.field "tableAliases" (Decode.dict Decode.string))
        |> Decode.map
            (Maybe.map (\aliases -> withTableAliases aliases config)
                >> Maybe.withDefault config
            )


{-| Display a banner text below the login form. The text is rendered as markdown.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withLoginBannerText "**Welcome!** Please login to continue."
            |> PostgRestAdmin.buildProgram

-}
withLoginBannerText : String -> Config msg -> Config msg
withLoginBannerText text (Config conf errors) =
    Config
        { conf
            | attributes =
                (\model -> { model | loginBannerText = Just text })
                    :: conf.attributes
        }
        errors


loginBannerTextDecoder : Config msg -> Decoder (Config msg)
loginBannerTextDecoder config =
    Decode.maybe (Decode.field "loginBannerText" Decode.string)
        |> Decode.map
            (Maybe.map (\text -> withLoginBannerText text config)
                >> Maybe.withDefault config
            )
