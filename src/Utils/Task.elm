module Utils.Task exposing
    ( Error(..)
    , attemptWithError
    , errorToString
    , fail
    , handleJsonResponse
    , handleResponse
    , toError
    )

import Browser.Dom
import Http
import Json.Decode as Decode exposing (Decoder)
import Parser
import Postgrest.Client as PG
import Task exposing (Task)


type Error
    = DomError Browser.Dom.Error
    | ParserError (List Parser.DeadEnd)
    | HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | BadSchema String
    | AutocompleteError String
    | RequestError String
    | NoError
    | AuthError


attemptWithError : (error -> msg) -> (a -> msg) -> Task error a -> Cmd msg
attemptWithError failure success task =
    let
        tagger result =
            case result of
                Ok a ->
                    success a

                Err err ->
                    failure err
    in
    Task.attempt tagger task


fail : (Error -> msg) -> Error -> Cmd msg
fail tagger err =
    Task.fail err
        |> attemptWithError tagger tagger


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder =
    handleResponse
        (\body ->
            case Decode.decodeString decoder body of
                Err err ->
                    Err (DecodeError err)

                Ok result ->
                    Ok result
        )


handleResponse : (body -> Result Error a) -> Http.Response body -> Result Error a
handleResponse toResult response =
    case response of
        Http.BadUrl_ url ->
            Err <| HttpError (Http.BadUrl url)

        Http.Timeout_ ->
            Err <| HttpError Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err <| HttpError (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err <| HttpError Http.NetworkError

        Http.GoodStatus_ _ body ->
            toResult body


toError : Result x a -> Maybe x
toError result =
    case result of
        Err err ->
            Just err

        _ ->
            Nothing


errorToString : Error -> String
errorToString error =
    case error of
        DomError (Browser.Dom.NotFound err) ->
            err

        ParserError err ->
            Parser.deadEndsToString err

        PGError PG.Timeout ->
            "Timeout"

        PGError (PG.BadUrl msg) ->
            "Bad url:" ++ msg

        PGError PG.NetworkError ->
            "Network error"

        PGError (PG.BadBody msg) ->
            "Postgres bad body:" ++ msg

        PGError (PG.BadStatus statusCode msg { message }) ->
            "Bad status "
                ++ (statusCode |> String.fromInt)
                ++ ": "
                ++ msg
                ++ "."
                ++ (message
                        |> Maybe.map (\text -> " " ++ text)
                        |> Maybe.withDefault ""
                   )

        HttpError httpError ->
            case httpError of
                Http.BadUrl msg ->
                    msg

                Http.Timeout ->
                    "Request Timeout"

                Http.NetworkError ->
                    "Network Error"

                Http.BadStatus status ->
                    "Bad status: " ++ (status |> String.fromInt)

                Http.BadBody msg ->
                    msg

        DecodeError err ->
            Decode.errorToString err

        BadSchema msg ->
            msg

        RequestError msg ->
            msg

        NoError ->
            "No Error"

        AuthError ->
            "Auth Error"

        AutocompleteError msg ->
            msg
