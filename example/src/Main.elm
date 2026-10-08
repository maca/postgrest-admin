port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import FormToolkit.Field as Field exposing (Field)
import FormToolkit.Parse as Parse
import FormToolkit.Value as Value
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import PostgRestAdmin
import Task
import Url exposing (Url)


port loggedIn : String -> Cmd msg


port loggedOut : () -> Cmd msg


type alias Model =
    { adminAppModel : PostgRestAdmin.Model
    , authFormFields : Field String
    , authError : Maybe String
    }


type SelectorMsg
    = AuthFieldsChanged (Field.Msg String)
    | AuthFormSubmitted
    | GotToken (Result Http.Error String)


type Msg
    = SelectorChanged SelectorMsg
    | AdminAppChanged PostgRestAdmin.Msg


main : Program Decode.Value Model Msg
main =
    let
        params =
            adminAppParams
                { formFields = Nothing
                , jwt = Nothing
                , apiHost = "http://localhost:9080"
                }
    in
    Browser.application
        { init = params.init
        , view =
            \model ->
                { title = "PostgREST Admin"
                , body =
                    [ Html.div
                        []
                        [ case
                            model.adminAppModel
                                |> PostgRestAdmin.appUrl
                                |> .path
                          of
                            [] ->
                                view model

                            _ ->
                                params.view model
                        ]
                    ]
                }
        , update =
            \msg ->
                case msg of
                    AdminAppChanged innerMsg ->
                        params.update innerMsg

                    SelectorChanged innerMsg ->
                        update innerMsg
        , subscriptions = params.subscriptions
        , onUrlRequest = params.onUrlRequest
        , onUrlChange = params.onUrlChange
        }


adminAppParams :
    { formFields : Maybe (Field String)
    , apiHost : String
    , jwt : Maybe String
    }
    -> PostgRestAdmin.AppParams Model Msg
adminAppParams params =
    PostgRestAdmin.configure
        |> PostgRestAdmin.withHost params.apiHost
        |> (params.jwt
                |> Maybe.map PostgRestAdmin.withJWT
                |> Maybe.withDefault identity
           )
        |> PostgRestAdmin.withClientHeaders
            [ Http.header "Accept-Profile" "bluebox"
            , Http.header "Content-Profile" "bluebox"
            ]
        |> PostgRestAdmin.onLogin loggedIn
        |> PostgRestAdmin.onLogout loggedOut
        |> PostgRestAdmin.buildAppParams
            { toInnerModel = .adminAppModel
            , toOuterModel = \app model -> { model | adminAppModel = app }
            , toOuterMsg = AdminAppChanged
            , initModel =
                \_ app ->
                    { adminAppModel = app
                    , authFormFields =
                        params.formFields
                            |> Maybe.withDefault
                                (authForm (hostParams app))
                    , authError = Nothing
                    }
            }


hostParams : PostgRestAdmin.Model -> { apiHost : String, authEndpointUrl : String }
hostParams app =
    let
        query =
            PostgRestAdmin.appUrl app |> .queryParameters

        getUrl name =
            Dict.get name query
                |> Maybe.andThen
                    (List.filterMap Url.fromString >> List.head)

        ( apiHost, authEndpointUrl ) =
            case
                ( getUrl "apiHost"
                , getUrl "authEndpoint"
                )
            of
                ( Just api, Just host ) ->
                    ( Url.toString api, Url.toString host )

                ( Just api, _ ) ->
                    ( Url.toString api
                    , Url.toString { api | path = "/rpc/login" }
                    )

                _ ->
                    ( "", "" )
    in
    { apiHost = apiHost
    , authEndpointUrl = authEndpointUrl
    }


update : SelectorMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthFieldsChanged fieldMsg ->
            let
                ( fields, _ ) =
                    model.authFormFields
                        |> Parse.parseUpdate formParser fieldMsg
            in
            ( { model | authFormFields = fields, authError = Nothing }
            , Cmd.none
            )

        AuthFormSubmitted ->
            case Parse.parse formParser model.authFormFields of
                Ok ( apiHost, Token token ) ->
                    let
                        ( adminModel, adminCmd ) =
                            PostgRestAdmin.applyConfiguration
                                (PostgRestAdmin.configure
                                    |> PostgRestAdmin.withJWT token
                                    |> PostgRestAdmin.withHost apiHost
                                )
                                model.adminAppModel
                    in
                    ( { model | adminAppModel = adminModel }
                    , Cmd.map AdminAppChanged adminCmd
                    )

                Ok ( apiHost, Post authEndpoint value ) ->
                    let
                        ( adminModel, _ ) =
                            PostgRestAdmin.applyConfiguration
                                (PostgRestAdmin.configure
                                    |> PostgRestAdmin.withHost apiHost
                                )
                                model.adminAppModel
                    in
                    ( { model | adminAppModel = adminModel }
                    , Http.request
                        { method = "POST"
                        , headers = []
                        , url = authEndpoint
                        , body = Http.jsonBody value
                        , expect =
                            Http.expectJson (SelectorChanged << GotToken)
                                (Decode.field "token" Decode.string)
                        , tracker = Nothing
                        , timeout = Nothing
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        GotToken (Ok token) ->
            let
                ( adminModel, adminCmd ) =
                    PostgRestAdmin.applyConfiguration
                        (PostgRestAdmin.configure
                            |> PostgRestAdmin.withJWT token
                        )
                        model.adminAppModel
            in
            ( { model | adminAppModel = adminModel, authError = Nothing }
            , Cmd.batch [ Cmd.map AdminAppChanged adminCmd, loggedIn token ]
            )

        GotToken (Err _) ->
            ( { model | authError = Just "Invalid email/password or JWT. Please try again." }
            , Cmd.none
            )


type AuthScheme
    = Post String Decode.Value
    | Token String


formParser : Parse.Parser String ( String, AuthScheme )
formParser =
    Parse.map2 Tuple.pair
        (Parse.field "hostUrl" Parse.string)
        (Parse.field "login-with" Parse.string
            |> Parse.andUpdate
                (\fields scheme ->
                    case scheme of
                        "jwt" ->
                            ( fields
                                |> Field.updateWithId "jwt"
                                    (Field.hidden False)
                                |> Field.updateWithId "credentials"
                                    (Field.hidden True)
                            , Parse.field "jwt" Parse.string
                                |> Parse.map Token
                            )

                        _ ->
                            ( fields
                                |> Field.updateWithId "jwt"
                                    (Field.hidden True)
                                |> Field.updateWithId "credentials"
                                    (Field.hidden False)
                            , Parse.map2 Post
                                (Parse.field "authUrl" Parse.string)
                                (Parse.field "login-credentials" Parse.json)
                            )
                )
        )


authForm : { apiHost : String, authEndpointUrl : String } -> Field String
authForm params =
    Field.group []
        [ Field.url
            [ Field.label "PostgREST API URL"
            , Field.identifier "hostUrl"
            , Field.stringValue params.apiHost
            , Field.required True
            ]
        , Field.group
            []
            [ Field.radio
                [ Field.label "Login with:"
                , Field.identifier "login-with"
                , Field.stringValue "post"
                , Field.options
                    [ ( "POST request to a JWT authentication endpoint", Value.string "post" )
                    , ( "Your own JWT", Value.string "jwt" )
                    ]
                ]
            , Field.group
                [ Field.name "credentials"
                , Field.identifier "credentials"
                ]
                [ Field.url
                    [ Field.label "Authentication endpoint URL"
                    , Field.identifier "authUrl"
                    , Field.stringValue params.authEndpointUrl
                    , Field.required True
                    ]
                , Field.group [ Field.identifier "login-credentials" ]
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
                ]
            , Field.textarea
                [ Field.label "JWT"
                , Field.identifier "jwt"
                , Field.required True
                , Field.hidden True
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ Attrs.class "auth-modal overlay" ]
        [ Html.div
            [ Attrs.class "auth-form" ]
            [ Html.form
                [ Attrs.class "auth-form"
                , Events.onSubmit (SelectorChanged AuthFormSubmitted)
                ]
                [ case model.authError of
                    Just message ->
                        Html.p [ Attrs.class "auth-error" ] [ Html.text message ]

                    Nothing ->
                        Html.text ""
                , model.authFormFields
                    |> Field.toHtml (SelectorChanged << AuthFieldsChanged)
                , Html.button
                    [ Attrs.disabled
                        (Parse.parse Parse.json model.authFormFields
                            |> Result.map (always False)
                            |> Result.withDefault True
                        )
                    ]
                    [ Html.text "Login" ]
                ]
            ]
        ]
