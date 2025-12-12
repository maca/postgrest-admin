port module Main exposing (main)

import FormToolkit.Field as Field
import Http
import PostgRestAdmin


port loggedIn : String -> Cmd msg


port loggedOut : () -> Cmd msg


main : PostgRestAdmin.Program Never
main =
    PostgRestAdmin.application
        [ PostgRestAdmin.onLogin loggedIn
        , PostgRestAdmin.onLogout loggedOut
        ]
