module Internal.Http exposing
    ( Error(..)
    , Response(..)
    , errorToString
    , handleJsonResponse
    , handleResponse
    , toError
    )

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, Value)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | RequestError String
    | ExpectedRecord
    | ExpectedRecordList
    | NoError
    | AuthError


type alias Count =
    { from : Int
    , to : Int
    , total : Int
    }


type Response
    = One Value
    | Many Count (List Value)
    | None


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder =
    handleResponse
        (\_ body ->
            case Decode.decodeString decoder body of
                Err err ->
                    Err (DecodeError err)

                Ok result ->
                    Ok result
        )


handleResponse :
    (Dict String String -> body -> Result Error a)
    -> Http.Response body
    -> Result Error a
handleResponse toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (HttpError (Http.BadUrl url))

        Http.Timeout_ ->
            Err (HttpError Http.Timeout)

        Http.BadStatus_ { statusCode } _ ->
            case statusCode of
                401 ->
                    Err AuthError

                _ ->
                    Err (HttpError (Http.BadStatus statusCode))

        Http.NetworkError_ ->
            Err (HttpError Http.NetworkError)

        Http.GoodStatus_ { headers } body ->
            toResult headers body


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
        HttpError httpError ->
            case httpError of
                Http.BadUrl msg ->
                    "Invalid URL:" ++ msg

                Http.Timeout ->
                    "The requested timed out. Please try again."

                Http.NetworkError ->
                    "Network Error: do you have an internet connection?"

                Http.BadStatus status ->
                    "Bad status: " ++ (status |> String.fromInt)

                Http.BadBody msg ->
                    "Response error: " ++ msg

        DecodeError err ->
            Decode.errorToString err

        RequestError msg ->
            msg

        ExpectedRecord ->
            "Expected a record"

        ExpectedRecordList ->
            "Expected a list of records"

        NoError ->
            "No Error"

        AuthError ->
            "Auth Error"
