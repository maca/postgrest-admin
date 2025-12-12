module PostgRestAdmin exposing
    ( Program, application, Msg
    , host, mountPath, clientHeaders, recordsPerPage
    , loginUrl, jwt
    , onLogin, onAuthFailed, onLogout, onExternalLogin, loginBannerText
    , menuLinks, formFields, detailActions
    , tables, tableAliases
    , configDecoder, init, update, view
    )

{-|


# Init

@docs Program, application, Msg


# Program configuration


## Basics

@docs host, mountPath, clientHeaders, recordsPerPage, routes


## Auth

@docs loginUrl, jwt
@docs onLogin, onAuthFailed, onLogout, onExternalLogin, loginBannerText


## Customization

@docs menuLinks, formFields, detailActions
@docs tables, tableAliases


# Application mounting

@docs routes

-}

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
import Internal.Flag as Flag
import Internal.PageDetail as PageDetail
import Internal.PageForm as PageForm
import Internal.PageListing as PageListing exposing (Model)
import Internal.Schema exposing (Record, Schema)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Markdown
import PostgRestAdmin.Client as Client exposing (AuthScheme, Client, Error(..))
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import Postgrest.Client as PG
import Process
import String.Extra as String
import Task
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, s)



-- AUTH FORM TYPES


type AuthFormStatus
    = Ready
    | Active
    | Submitting
    | Failure Client.Error



-- PROGRAM


{-| An alias to elm's Platform.Program providing the type signature for a
PostgRestAdmin program.
-}
type alias Program msg =
    Platform.Program Decode.Value (Model msg) Msg


type alias InitParams msg =
    { client : Client
    , key : Nav.Key
    , config : Config msg
    }


type alias Model msg =
    { route : Route
    , key : Nav.Key
    , notification : Notification
    , error : Maybe String
    , client : Client
    , onLogin : String -> Cmd Msg
    , currentUrl : Url
    , config : Config msg
    , authFormUrl : Url
    , authFormJwtDecoder : Decoder String
    , authFormJwtEncoder : Dict String String -> Encode.Value
    , authFormField : Field.Field Never
    , authFormStatus : AuthFormStatus
    }


type Notification
    = Confirmation String
    | Error String
    | NoNotification


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
    | LoggedIn { path : String, accessToken : String }
    | LoggedOut
    | AuthRequired Error
    | NoOp


type Route
    = RouteRoot
    | RouteLoadingSchema Url
    | RouteListing PageListing.Model
    | RouteDetail PageDetail.Model
    | RouteForm PageForm.Model
    | RouteNotFound


{-| Creates a PostgRestAdmin application with the given configuration attributes.

Configuration is provided as a list of attributes. See the configuration functions
below for all available options.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.host "http://localhost:3000"
            , PostgRestAdmin.onLogin loginSuccess
            ]

-}
application : List (Attribute f m msg) -> Program msg
application attrs =
    Browser.application
        { init = init attrs
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : List (Attribute f m msg) -> Value -> Url -> Nav.Key -> ( Model msg, Cmd Msg )
init attrs flags url key =
    case
        Decode.decodeValue (configDecoder attrs) flags
            |> Result.map (initModel url key)
    of
        Ok model ->
            let
                ( route, cmd ) =
                    parseRoute url model
            in
            ( { model | route = route }, cmd )

        Err error ->
            let
                model =
                    initModel url key default
            in
            ( { model | error = Just (Decode.errorToString error) }
            , Cmd.none
            )


initModel : Url -> Nav.Key -> Config msg -> Model msg
initModel url key configRec =
    { route = RouteRoot
    , key = key
    , notification = NoNotification
    , error = Nothing
    , client = Client.init configRec.host configRec.authScheme configRec.clientHeaders
    , onLogin = configRec.onLogin >> Cmd.map (always NoOp)
    , currentUrl = url
    , config = configRec
    , authFormUrl = configRec.loginUrl
    , authFormJwtDecoder = Decode.field "token" Decode.string
    , authFormJwtEncoder = Encode.dict identity Encode.string
    , authFormField = authFormField
    , authFormStatus = Ready
    }



-- INIT


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


requestToken : Model msg -> Cmd Msg
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
            , resolver = Client.jsonResolver model.authFormJwtDecoder
            , timeout = Nothing
            }
        )



-- UPDATE


update : Msg -> Model msg -> ( Model msg, Cmd Msg )
update msg model =
    let
        client =
            model.client
    in
    case msg of
        AuthFieldsChanged innerMsg ->
            ( { model
                | authFormField = Field.update innerMsg model.authFormField
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
                [ model.onLogin tokenStr
                , Client.fetchSchema model.config updatedClient
                    |> Task.attempt SchemaFetched
                ]
            )

        GotToken (Err error) ->
            ( { model | authFormStatus = Failure error }
            , Cmd.none
            )

        SchemaFetched (Ok schema) ->
            let
                updatedClient =
                    { client | schema = schema }

                ( route, cmd ) =
                    routeCons model.currentUrl
                        { client = updatedClient
                        , key = model.key
                        , config = model.config
                        }
            in
            ( { model | client = updatedClient, route = route }
            , cmd
            )

        SchemaFetched (Err err) ->
            ( { model
                | client = Client.logout model.client
                , authFormStatus = Failure err
              }
            , Cmd.none
            )

        PageListingChanged childMsg ->
            case model.route of
                RouteListing listing ->
                    let
                        ( route, appCmd ) =
                            PageListing.update childMsg listing
                                |> Tuple.mapFirst RouteListing
                    in
                    ( { model | route = route }
                    , mapCmd PageListingChanged appCmd
                    )

                _ ->
                    ( model, Cmd.none )

        PageDetailChanged childMsg ->
            case model.route of
                RouteDetail prevDetail ->
                    let
                        ( detail, cmd ) =
                            PageDetail.update childMsg prevDetail
                    in
                    ( { model | route = RouteDetail detail }
                    , mapCmd PageDetailChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

        PageFormChanged childMsg ->
            case model.route of
                RouteForm prevForm ->
                    let
                        ( form, cmd ) =
                            PageForm.update childMsg prevForm
                    in
                    ( { model | route = RouteForm form }
                    , mapCmd PageFormChanged cmd
                    )

                _ ->
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
            if model.currentUrl.path /= url.path then
                let
                    ( route, cmd ) =
                        parseRoute url model
                in
                ( { model | route = route, currentUrl = url }, cmd )

            else
                ( { model | currentUrl = url }, Cmd.none )

        LoggedIn params ->
            ( { model | client = Client.updateJwt params.accessToken model.client }
            , Nav.pushUrl model.key params.path
            )

        LoggedOut ->
            ( { model | client = Client.logout model.client }
            , Cmd.map (always NoOp) (model.config.onLogout ())
            )

        AuthRequired err ->
            ( { model
                | client = Client.logout model.client
                , authFormStatus = Failure err
                , notification = NoNotification
              }
            , Cmd.map (always NoOp)
                (model.config.onAuthFailed (urlToPath model.currentUrl))
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model msg -> Browser.Document Msg
view model =
    { title = "Admin"
    , body =
        case model.error of
            Just error ->
                [ Html.h1 [] [ Html.text "Init failed" ]
                , Html.pre
                    [ Attrs.class "parse-errors" ]
                    [ Html.text error ]
                ]

            Nothing ->
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
    }


viewAuthForm : Model msg -> Html Msg
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
            , case model.config.loginBannerText of
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


sideMenu : Model msg -> Html Msg
sideMenu model =
    Html.div
        [ Attrs.class "side-menu" ]
        [ Html.aside
            [ Attrs.class "resources-menu" ]
            [ Html.ul [] (List.map (menuItem model.config.mountPath) (resources model))
            , Html.ul [] (List.map extraMenuItem model.config.menuLinks)
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
            [ Html.i [ Attrs.class "icono-cross", Events.onClick NotificationDismiss ] [] ]
        ]


mainContent : Model msg -> Html Msg
mainContent model =
    case model.route of
        RouteRoot ->
            Html.text ""

        RouteLoadingSchema _ ->
            Html.text "Not found"

        RouteListing listing ->
            Html.map PageListingChanged (PageListing.view listing)

        RouteDetail listing ->
            Html.map PageDetailChanged (PageDetail.view listing)

        RouteForm form ->
            Html.map PageFormChanged (PageForm.view form)

        RouteNotFound ->
            Html.text "Not found"



-- SUBSCRIPTIONS


subscriptions : Model msg -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.route of
            RouteListing pageListing ->
                Sub.map PageListingChanged
                    (PageListing.subscriptions pageListing)

            _ ->
                Sub.none
        , Sub.map LoggedIn (model.config.onExternalLogin identity)
        ]



-- ROUTES


parseRoute : Url -> Model msg -> ( Route, Cmd Msg )
parseRoute url model =
    if Client.schemaIsLoaded model.client then
        routeCons url
            { client = model.client
            , key = model.key
            , config = model.config
            }

    else
        ( RouteLoadingSchema url
        , Task.attempt SchemaFetched
            (Client.fetchSchema model.config model.client)
        )


routeCons : Url -> InitParams msg -> ( Route, Cmd Msg )
routeCons url params =
    Parser.parse
        (List.foldr (\p acc -> s p </> acc)
            (routeParser url params)
            (MountPath.segments params.config.mountPath)
        )
        url
        |> Maybe.withDefault ( RouteNotFound, Cmd.none )


routeParser :
    Url
    -> InitParams msg
    -> Parser (( Route, Cmd Msg ) -> a) a
routeParser url params =
    Parser.oneOf
        [ -- /
          Parser.map
            ( RouteRoot
            , resources params
                |> List.head
                |> Maybe.map
                    (\p ->
                        Nav.pushUrl params.key
                            (MountPath.path params.config.mountPath p)
                    )
                |> Maybe.withDefault Cmd.none
            )
            Parser.top

        -- /posts/new
        , Parser.map (\tableName -> initForm params Nothing tableName Nothing)
            (Parser.string </> s "new")

        -- /posts/edit
        , Parser.map (\tableName id -> initForm params Nothing tableName (Just id))
            (Parser.string </> Parser.string </> s "edit")

        -- /posts
        , Parser.map (initListing params url Nothing) Parser.string

        -- /posts/1
        , Parser.map (initDetail params)
            (Parser.string </> Parser.string)

        -- /posts/1/comments
        , Parser.map
            (\parentTable parentId ->
                Just { tableName = parentTable, id = parentId }
                    |> initListing params url
            )
            (Parser.string </> Parser.string </> Parser.string)

        -- /posts/1/comments/new
        , Parser.map
            (\parentTable parentId tableName ->
                initForm params
                    (Just { tableName = parentTable, id = parentId })
                    tableName
                    Nothing
            )
            (Parser.string </> Parser.string </> Parser.string </> s "new")
        ]


initListing :
    InitParams msg
    -> Url
    -> Maybe { tableName : String, id : String }
    -> String
    -> ( Route, Cmd Msg )
initListing params url parent tableName =
    case Dict.get tableName params.client.schema of
        Just table ->
            let
                listingParams =
                    { client = params.client
                    , mountPath = params.config.mountPath
                    , table = table
                    , parent = parent
                    , recordsPerPage = params.config.recordsPerPage
                    }
            in
            PageListing.init listingParams url params.key
                |> Tuple.mapFirst RouteListing
                |> Tuple.mapSecond (mapCmd PageListingChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initForm :
    InitParams msg
    -> Maybe { tableName : String, id : String }
    -> String
    -> Maybe String
    -> ( Route, Cmd Msg )
initForm model parent tableName id =
    case Dict.get tableName model.client.schema of
        Just table ->
            PageForm.init
                { client = model.client
                , navKey = model.key
                , mountPath = model.config.mountPath

                -- , fieldNames = Dict.get tableName config.formFields |> Maybe.withDefault []
                , parent = parent
                , table = table
                , id = id
                }
                |> Tuple.mapFirst RouteForm
                |> Tuple.mapSecond (mapCmd PageFormChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initDetail :
    InitParams msg
    -> String
    -> String
    -> ( Route, Cmd Msg )
initDetail model tableName id =
    case Dict.get tableName model.client.schema of
        Just table ->
            let
                detailParams =
                    { client = model.client
                    , mountPath = model.config.mountPath
                    , table = table
                    , id = id
                    , detailActions =
                        model.config.detailActions
                            |> Dict.get tableName
                            |> Maybe.withDefault []
                    }
            in
            PageDetail.init detailParams model.key
                |> Tuple.mapFirst RouteDetail
                |> Tuple.mapSecond (mapCmd PageDetailChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )



-- UTILS


resources : { a | client : Client, config : Config msg } -> List String
resources params =
    if List.isEmpty params.config.tables then
        Dict.keys params.client.schema |> List.sort

    else
        params.config.tables


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


urlToPath : Url -> String
urlToPath url =
    [ Just url.path, url.query ]
        |> List.filterMap identity
        |> String.join "?"



-- CONFIG
--


type Attribute flag model msg
    = Attribute (Decoder (Config msg -> Config msg))


type alias Config msg =
    { host : Url
    , mountPath : MountPath
    , loginUrl : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , detailActions : Dict String (List ( String, Record -> String -> String ))
    , tables : List String
    , menuLinks : List ( String, String )
    , menuActions : Dict String Url
    , onLogin : String -> Cmd msg
    , onAuthFailed : String -> Cmd msg
    , onExternalLogin :
        ({ path : String, accessToken : String }
         -> { path : String, accessToken : String }
        )
        -> Sub { path : String, accessToken : String }
    , onLogout : () -> Cmd msg
    , tableAliases : Dict String String
    , clientHeaders : List Http.Header
    , recordsPerPage : Int
    , loginBannerText : Maybe String
    }


configDecoder : List (Attribute flag model msg) -> Decoder (Config msg)
configDecoder =
    List.foldl (\(Attribute attr) -> Decode.map2 (<|) attr) (Decode.succeed default)
        >> Flag.string "host"
            (\urlStr conf ->
                Url.fromString urlStr
                    |> Maybe.map
                        (\u ->
                            Decode.succeed
                                { conf
                                    | host = u
                                    , loginUrl = { u | path = "/rpc/login" }
                                }
                        )
                    |> Maybe.withDefault
                        (Decode.fail "`Config.host` was given an invalid URL")
            )
        >> Flag.string "loginUrl"
            (\urlStr conf ->
                Url.fromString urlStr
                    |> Maybe.map (\u -> Decode.succeed { conf | loginUrl = u })
                    |> Maybe.withDefault
                        (Decode.fail "`Config.loginUrl` was given an invalid URL")
            )
        >> Flag.string "mountPath"
            (\p conf -> Decode.succeed { conf | mountPath = MountPath.fromString p })
        >> Flag.string "jwt"
            (\tokenStr conf -> Decode.succeed { conf | authScheme = Client.jwt tokenStr })
        >> Flag.stringListDict "formFields"
            (\fields conf ->
                Decode.succeed { conf | formFields = Dict.union fields conf.formFields }
            )
        >> Flag.stringList "tables"
            (\tableNames conf -> Decode.succeed { conf | tables = tableNames })
        >> Flag.stringDict "tableAliases"
            (\aliases conf -> Decode.succeed { conf | tableAliases = aliases })
        >> Flag.linksList "menuLinks"
            (\links conf -> Decode.succeed { conf | menuLinks = links })
        >> Flag.headersList "clientHeaders"
            (\headers conf -> Decode.succeed { conf | clientHeaders = headers })
        >> Flag.int "recordsPerPage"
            (\count conf -> Decode.succeed { conf | recordsPerPage = count })
        >> Flag.string "loginBannerText"
            (\text conf -> Decode.succeed { conf | loginBannerText = Just text })


default : Config msg
default =
    let
        defaultHost =
            { protocol = Http
            , host = "localhost"
            , port_ = Just 3000
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }
    in
    { authScheme = Client.unset
    , host = defaultHost
    , mountPath = MountPath.fromString ""
    , loginUrl = { defaultHost | path = "/rpc/login" }
    , formFields = Dict.empty
    , detailActions = Dict.empty
    , tables = []
    , menuLinks = []
    , menuActions = Dict.empty
    , onLogin = always Cmd.none
    , onAuthFailed = always Cmd.none
    , onExternalLogin = always Sub.none
    , onLogout = always Cmd.none
    , tableAliases = Dict.empty
    , clientHeaders = []
    , recordsPerPage = 50
    , loginBannerText = Nothing
    }


attrDecoder : (Config msg -> Config msg) -> Attribute flags model msg
attrDecoder f =
    Attribute (Decode.succeed f)


{-| Specify the PostgREST host.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.host "http://localhost:3000"
            ]

Alternatively the host can be specified using flags.
Program flags take precedence.

    Elm.Main.init { flags = { host = "http://localhost:3000" } }

-}
host : String -> Attribute flags model msg
host urlStr =
    case Url.fromString urlStr of
        Just u ->
            attrDecoder
                (\conf ->
                    { conf
                        | host = u
                        , loginUrl = { u | path = "/rpc/login" }
                    }
                )

        Nothing ->
            Attribute (Decode.fail "`Config.host` was given an invalid URL")


{-| Specify a path prefix for all routes, in case the app is not mounted in the
root path.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.mountPath "/back-office"
            ]

Alternatively the mount path can be specified using flags.
Program flags take precedence.

    Elm.Main.init { flags = { mountPath = "/back-office" } }

-}
mountPath : String -> Attribute flags model msg
mountPath p =
    attrDecoder (\conf -> { conf | mountPath = MountPath.fromString p })


{-| Specify the login URL for form authentication.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.loginUrl "http://localhost:3000/rpc/login"
            ]

Alternatively the login URL can be specified using flags.
Program flags take precedence.

    Elm.Main.init { flags = { loginUrl = "http://localhost:3000/rpc/login" } }

-}
loginUrl : String -> Attribute flags model msg
loginUrl urlStr =
    case Url.fromString urlStr of
        Just u ->
            attrDecoder (\conf -> { conf | loginUrl = u })

        Nothing ->
            Attribute (Decode.fail "`Config.loginUrl` was given an invalid URL")


{-| Set a JWT to authenticate PostgREST requests. You can set an initial JWT
using this attribute.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.jwt "8abf3a...9ac36d"
            ]

Alternatively the token can be passed using flags.
Program flags take precedence.

    Elm.Main.init
        { flags = { jwt = sessionStorage.getItem "jwt" }
        }

-}
jwt : String -> Attribute flags model msg
jwt tokenStr =
    attrDecoder (\conf -> { conf | authScheme = Client.jwt tokenStr })


{-| Callback triggered with a JWT string on successful login.
Typically used to persist the JWT to session storage.

    port loginSuccess : String -> Cmd msg

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.onLogin loginSuccess
            ]

Then subscribe to the corresponding port.

    app = Elm.Main.init({
      flags: { jwt: sessionStorage.getItem("jwt") }
    })

    app.ports.loginSuccess.subscribe(jwt => {
      sessionStorage.setItem("jwt", jwt)
    });

-}
onLogin : (String -> Cmd msg) -> Attribute flags model msg
onLogin f =
    attrDecoder (\conf -> { conf | onLogin = f })


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use this to perform external authentication.

    port authFailure : String -> Cmd msg

    port tokenReceiver :
        ({ path : String, accessToken : String } -> msg)
        -> Sub msg

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.onAuthFailed authFailure
            , PostgRestAdmin.onExternalLogin tokenReceiver
            ]

Then wire to the corresponding ports.

    app = Elm.Main.init()

    app.ports.authFailure.subscribe(requestedPath => {
        authenticate(requestedPath).then((accessToken) => {
            app.ports.tokenReceiver.send({
                path : requestedPath,
                accessToken : accessToken
            })
        })
    });

-}
onAuthFailed : (String -> Cmd msg) -> Attribute flags model msg
onAuthFailed f =
    attrDecoder (\conf -> { conf | onAuthFailed = f })


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
    -> Attribute flags model msg
onExternalLogin sub =
    attrDecoder (\conf -> { conf | onExternalLogin = sub })


{-| Callback triggered when the user logs out.
You can use this to perform cleanup or external logout operations.

    port logout : () -> Cmd msg

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.onLogout logout
            ]

Then subscribe to the corresponding port.

    app = Elm.Main.init()

    app.ports.logout.subscribe(_ => {
        externalLogout()
    });

-}
onLogout : (() -> Cmd msg) -> Attribute flags model msg
onLogout f =
    attrDecoder (\conf -> { conf | onLogout = f })


{-| Specify which fields should be present in the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.formFields "posts" [ "id", "title", "content" ]
            ]

Alternatively this parameter can be configured using flags.
Program flags take precedence.

    Elm.Main.init
        { flags = { formFields = { posts = [ "id", "title", "content" ] } }
        }

-}
formFields : String -> List String -> Attribute flags model msg
formFields tableName fields =
    attrDecoder
        (\conf ->
            { conf
                | formFields = Dict.insert tableName fields conf.formFields
            }
        )


{-| Specify action buttons to be shown in the detail page of a
record along with Edit and Delete buttons.

`detailActions` expects a table name and a list of tuples. The first element of
each tuple is the button text and the second is a function that takes a record
and an ID and returns a URL string.

    import Url.Builder as Url

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.detailActions "posts"
                [ ( "View Comments"
                  , \_ id -> Url.absolute [ "posts", id, "comments" ] []
                  )
                ]
            ]

-}
detailActions :
    String
    -> List ( String, Record -> String -> String )
    -> Attribute flags model msg
detailActions tableName actions =
    attrDecoder
        (\conf ->
            { conf
                | detailActions =
                    Dict.insert tableName actions conf.detailActions
            }
        )


{-| Pass a list of table names to restrict the editable resources, also sets the
order of the left resources menu.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.tables [ "posts", "comments" ]
            ]

Alternatively the tables can be specified using flags.
Program flags take precedence.

    Elm.Main.init { flags = { tables = [ "posts", "comments" ] } }

-}
tables : List String -> Attribute flags model msg
tables tableNames =
    attrDecoder (\conf -> { conf | tables = tableNames })


{-| Pass a list of links to display in the side menu. The list consists of
tuples of the link text and a url.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.menuLinks [ ( "Api Docs", "/api/docs" ) ]
            ]

Alternatively the menu links can be specified using flags.
Program flags take precedence.

    Elm.Main.init
        { flags =
            { menuLinks =
                [ { text = "Api Docs", url = "/api/docs" }
                ]
            }
        }

-}
menuLinks : List ( String, String ) -> Attribute flags model msg
menuLinks links =
    attrDecoder (\conf -> { conf | menuLinks = links })


{-| Set the number of records to display per page in listing views.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.recordsPerPage 100
            ]

Alternatively this can be specified using flags. Program flags take precedence.

-}
recordsPerPage : Int -> Attribute flags model msg
recordsPerPage count =
    attrDecoder (\conf -> { conf | recordsPerPage = count })


{-| Display a banner text below the login form. The text is rendered as markdown.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.loginBannerText "**Welcome!** Please login to continue."
            ]

Alternatively the banner text can be specified using flags.
Program flags take precedence.

    Elm.Main.init { flags = { loginBannerText = "**Welcome!** Please login to continue." } }

-}
loginBannerText : String -> Attribute flags model msg
loginBannerText text =
    attrDecoder (\conf -> { conf | loginBannerText = Just text })


{-| Set default HTTP headers to be included in all Client requests. This is
useful for setting headers like `Accept-Profile` or `Content-Profile` when
working with PostgREST schemas.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.clientHeaders
                [ Http.header "Accept-Profile" "bluebox"
                , Http.header "Content-Profile" "bluebox"
                ]
            ]

Alternatively headers can be specified using flags.
Program flags take precedence.

    Elm.Main.init({
        flags: {
            clientHeaders: {
                "Accept-Profile": "bluebox",
                "Content-Profile": "bluebox"
            }
        }
    })

-}
clientHeaders : List Http.Header -> Attribute flags model msg
clientHeaders headers =
    attrDecoder (\conf -> { conf | clientHeaders = headers })


{-| Rename a table referenced in a foreign key. PostgREST OpenAPI generated docs
confuse tables with views when describing the foreign key for a resource,
because of this some links might be incorrectly generated.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application
            [ PostgRestAdmin.tableAliases
                (Dict.fromList [ ( "published_posts", "posts" ) ])
            ]

Alternatively the table aliases can be specified using flags.
Program flags take precedence.

    Elm.Main.init({
        flags: {
            tableAliases: { "published_posts": "posts" }
        }
    })

-}
tableAliases : Dict String String -> Attribute flags model msg
tableAliases aliases =
    attrDecoder (\conf -> { conf | tableAliases = aliases })
