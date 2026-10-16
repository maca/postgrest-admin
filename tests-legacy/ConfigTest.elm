module ConfigTest exposing (suite)

import Dict
import Expect
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import PostgRestAdmin
import PostgRestAdmin.Client as Client
import PostgRestAdmin.MountPath as MountPath
import Test exposing (..)
import Url


decodeConfig : PostgRestAdmin.Config msg -> Encode.Value -> Result Decode.Error (PostgRestAdmin.Params msg)
decodeConfig config flags =
    Decode.decodeValue (PostgRestAdmin.configDecoder config) flags
        |> Result.map (\(PostgRestAdmin.Config c _) -> c)


suite : Test
suite =
    describe "PostgRestAdmin Config and Flags"
        [ hostTests
        , mountPathTests
        , loginUrlTests
        , tablesTests
        , tableAliasesTests
        , formFieldsTests
        , menuLinksTests
        , clientHeadersTests
        , jwtTests
        ]



-- HOST TESTS


hostTests : Test
hostTests =
    describe "host configuration"
        [ test "default host is http://localhost:3000" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:3000")
        , test "PostgRestAdmin.withHost sets the host" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withHost "http://localhost:9080"
                    )
                    (Encode.object [])
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/")
        , test "host flag sets the host" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure
                    (Encode.object
                        [ ( "host", Encode.string "http://localhost:9080" )
                        ]
                    )
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/")
        , test "host flag takes precedence over PostgRestAdmin.withHost" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withHost "http://localhost:3000"
                    )
                    (Encode.object
                        [ ( "host", Encode.string "http://localhost:9080" )
                        ]
                    )
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/")
        ]



-- LOGIN URL TESTS


loginUrlTests : Test
loginUrlTests =
    describe "loginUrl configuration"
        [ test "default loginUrl is http://localhost:3000/rpc/login" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:3000/rpc/login")
        , test "loginUrl derives from host" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withHost "http://localhost:9080"
                    )
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/login")
        , test "PostgRestAdmin.withLoginUrl overrides derived loginUrl" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withHost "http://localhost:9080"
                        |> PostgRestAdmin.withLoginUrl "http://localhost:9080/rpc/authenticate"
                    )
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/authenticate")
        , test "PostgRestAdmin.withLoginUrl can use different host than PostgRestAdmin.withHost" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withHost "http://localhost:3000"
                        |> PostgRestAdmin.withLoginUrl "http://auth.example.com/login"
                    )
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://auth.example.com/login")
        , test "loginUrl flag sets the login URL" <|
            \_ ->
                decodeConfig
                    PostgRestAdmin.configure
                    (Encode.object
                        [ ( "loginUrl", Encode.string "http://localhost:9080/rpc/login" )
                        ]
                    )
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/login")
        , test "loginUrl flag takes precedence over PostgRestAdmin.withLoginUrl" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withLoginUrl "http://localhost:3000/rpc/login"
                    )
                    (Encode.object
                        [ ( "loginUrl", Encode.string "http://localhost:9080/rpc/login" )
                        ]
                    )
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/login")
        ]



-- MOUNT PATH TESTS


mountPathTests : Test
mountPathTests =
    describe "mountPath configuration"
        [ test "default mountPath is empty string" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString ""))
        , test "PostgRestAdmin.withMountPath sets the mount path" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withMountPath "/admin"
                    )
                    (Encode.object [])
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString "/admin"))
        , test "mountPath flag sets the mount path" <|
            \_ ->
                decodeConfig
                    PostgRestAdmin.configure
                    (Encode.object
                        [ ( "mountPath", Encode.string "/admin" )
                        ]
                    )
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString "/admin"))
        , test "mountPath flag takes precedence over PostgRestAdmin.withMountPath" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withMountPath "/dashboard"
                    )
                    (Encode.object
                        [ ( "mountPath", Encode.string "/admin" )
                        ]
                    )
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString "/admin"))
        ]



-- TABLES TESTS


tablesTests : Test
tablesTests =
    describe "tables configuration"
        [ test "default tables is empty list" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .tables
                    |> Expect.equal (Ok [])
        , test "PostgRestAdmin.withTables sets the tables list" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withTables [ "users", "posts" ]
                    )
                    (Encode.object [])
                    |> Result.map .tables
                    |> Expect.equal (Ok [ "users", "posts" ])
        , test "tables flag sets the tables list" <|
            \_ ->
                decodeConfig
                    PostgRestAdmin.configure
                    (Encode.object
                        [ ( "tables"
                          , Encode.list Encode.string [ "users", "posts" ]
                          )
                        ]
                    )
                    |> Result.map .tables
                    |> Expect.equal (Ok [ "users", "posts" ])
        , test "tables flag takes precedence over PostgRestAdmin.withTables" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withTables [ "comments", "tags" ]
                    )
                    (Encode.object
                        [ ( "tables"
                          , Encode.list Encode.string [ "users", "posts" ]
                          )
                        ]
                    )
                    |> Result.map .tables
                    |> Expect.equal (Ok [ "users", "posts" ])
        ]



-- TABLE ALIASES TESTS


tableAliasesTests : Test
tableAliasesTests =
    describe "tableAliases configuration"
        [ test "default tableAliases is empty dict" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .tableAliases
                    |> Expect.equal (Ok Dict.empty)
        , test "PostgRestAdmin.withTableAliases sets the table aliases" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withTableAliases
                            (Dict.fromList
                                [ ( "published_posts", "posts" )
                                ]
                            )
                    )
                    (Encode.object [])
                    |> Result.map .tableAliases
                    |> Expect.equal
                        (Ok (Dict.fromList [ ( "published_posts", "posts" ) ]))
        , test "tableAliases flag sets the table aliases" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure
                    (Encode.object
                        [ ( "tableAliases"
                          , Encode.object
                                [ ( "published_posts", Encode.string "posts" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .tableAliases
                    |> Expect.equal
                        (Ok (Dict.fromList [ ( "published_posts", "posts" ) ]))
        , test "tableAliases flag takes precedence over PostgRestAdmin.withTableAliases" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withTableAliases
                            (Dict.fromList
                                [ ( "active_users", "users" )
                                ]
                            )
                    )
                    (Encode.object
                        [ ( "tableAliases"
                          , Encode.object
                                [ ( "published_posts", Encode.string "posts" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .tableAliases
                    |> Expect.equal
                        (Ok (Dict.fromList [ ( "published_posts", "posts" ) ]))
        ]



-- FORM FIELDS TESTS


formFieldsTests : Test
formFieldsTests =
    describe "formFields configuration"
        [ test "default formFields is empty dict" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .formFields
                    |> Expect.equal (Ok Dict.empty)
        , test "PostgRestAdmin.withFormFields sets the form fields" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withFormFields "users" [ "id", "name", "email" ]
                    )
                    (Encode.object [])
                    |> Result.map (.formFields >> Dict.get "users")
                    |> Expect.equal (Ok (Just [ "id", "name", "email" ]))
        , test "formFields flag sets the form fields" <|
            \_ ->
                decodeConfig
                    PostgRestAdmin.configure
                    (Encode.object
                        [ ( "formFields"
                          , Encode.object
                                [ ( "users"
                                  , Encode.list Encode.string
                                        [ "id", "name", "email" ]
                                  )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.formFields >> Dict.get "users")
                    |> Expect.equal (Ok (Just [ "id", "name", "email" ]))
        , test "formFields flag takes precedence over PostgRestAdmin.withFormFields" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withFormFields "users" [ "id", "username" ]
                    )
                    (Encode.object
                        [ ( "formFields"
                          , Encode.object
                                [ ( "users"
                                  , Encode.list Encode.string
                                        [ "id", "name", "email" ]
                                  )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.formFields >> Dict.get "users")
                    |> Expect.equal (Ok (Just [ "id", "name", "email" ]))
        ]



-- MENU LINKS TESTS


menuLinksTests : Test
menuLinksTests =
    describe "menuLinks configuration"
        [ test "default menuLinks is empty list" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .menuLinks
                    |> Expect.equal (Ok [])
        , test "PostgRestAdmin.withMenuLinks sets the menu links" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withMenuLinks
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                    )
                    (Encode.object [])
                    |> Result.map .menuLinks
                    |> Expect.equal
                        (Ok
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                        )
        , test "menuLinks flag sets the menu links" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure
                    (Encode.object
                        [ ( "menuLinks"
                          , Encode.list
                                (\( text, url ) ->
                                    Encode.object
                                        [ ( "text", Encode.string text )
                                        , ( "url", Encode.string url )
                                        ]
                                )
                                [ ( "Documentation", "/docs" )
                                , ( "API", "/api" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .menuLinks
                    |> Expect.equal
                        (Ok
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                        )
        , test "menuLinks flag takes precedence over PostgRestAdmin.withMenuLinks" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withMenuLinks
                            [ ( "Home", "/" )
                            ]
                    )
                    (Encode.object
                        [ ( "menuLinks"
                          , Encode.list
                                (\( text, url ) ->
                                    Encode.object
                                        [ ( "text", Encode.string text )
                                        , ( "url", Encode.string url )
                                        ]
                                )
                                [ ( "Documentation", "/docs" )
                                , ( "API", "/api" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .menuLinks
                    |> Expect.equal
                        (Ok
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                        )
        ]



-- CLIENT HEADERS TESTS


clientHeadersTests : Test
clientHeadersTests =
    describe "clientHeaders configuration"
        [ test "default clientHeaders is empty list" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .clientHeaders
                    |> Expect.equal (Ok [])
        , test "PostgRestAdmin.withClientHeaders sets the client headers" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withClientHeaders
                            [ Http.header "Accept-Profile" "bluebox"
                            , Http.header "Content-Profile" "bluebox"
                            ]
                    )
                    (Encode.object [])
                    |> Result.map (.clientHeaders >> List.length)
                    |> Expect.equal (Ok 2)
        , test "clientHeaders flag sets the client headers" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure
                    (Encode.object
                        [ ( "clientHeaders"
                          , Encode.object
                                [ ( "Accept-Profile", Encode.string "bluebox" )
                                , ( "Content-Profile", Encode.string "bluebox" )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.clientHeaders >> List.length)
                    |> Expect.equal (Ok 2)
        , test "clientHeaders flag takes precedence over PostgRestAdmin.withClientHeaders" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withClientHeaders
                            [ Http.header "Accept-Profile" "public"
                            , Http.header "Content-Profile" "public"
                            ]
                    )
                    (Encode.object
                        [ ( "clientHeaders"
                          , Encode.object
                                [ ( "Accept-Profile", Encode.string "bluebox" )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.clientHeaders >> List.length)
                    |> Expect.equal (Ok 1)
        ]



-- JWT TESTS


jwtTests : Test
jwtTests =
    describe "jwt configuration"
        [ test "default authScheme is unset" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure (Encode.object [])
                    |> Result.map .authScheme
                    |> Expect.equal (Ok Client.unset)
        , test "PostgRestAdmin.withJWT sets the JWT token in authScheme" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure
                        |> PostgRestAdmin.withJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
                    )
                    (Encode.object [])
                    |> Result.map .authScheme
                    |> Expect.equal
                        (Ok (Client.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
        , test "jwt flag sets the JWT token in authScheme" <|
            \_ ->
                decodeConfig PostgRestAdmin.configure
                    (Encode.object
                        [ ( "jwt", Encode.string "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" )
                        ]
                    )
                    |> Result.map .authScheme
                    |> Expect.equal
                        (Ok (Client.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
        , test "jwt flag takes precedence over PostgRestAdmin.withJWT" <|
            \_ ->
                decodeConfig
                    (PostgRestAdmin.configure |> PostgRestAdmin.withJWT "old-token")
                    (Encode.object
                        [ ( "jwt", Encode.string "new-token-from-flag" )
                        ]
                    )
                    |> Result.map .authScheme
                    |> Expect.equal
                        (Ok (Client.jwt "new-token-from-flag"))
        ]
