module Internal.Value exposing
    ( Value(..)
    , encode
    , isNothing
    , isTrue
    , toString
    , updateWithString
    )

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import String.Extra as String
import Time


type Value
    = PFloat (Maybe Float)
    | PInt (Maybe Int)
    | PString (Maybe String)
    | PText (Maybe String)
    | PEnum (Maybe String) (List String)
    | PBool (Maybe Bool)
    | PTime (Maybe Time.Posix)
    | PDate (Maybe Time.Posix)
    | PJson (Maybe String)
    | Unknown Decode.Value


encode : Value -> Encode.Value
encode value =
    let
        encodeWith encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
    case value of
        PString maybe ->
            encodeWith Encode.string maybe

        PText maybe ->
            encodeWith Encode.string maybe

        PEnum maybe _ ->
            encodeWith Encode.string maybe

        PFloat maybe ->
            encodeWith Encode.float maybe

        PInt maybe ->
            encodeWith Encode.int maybe

        PBool maybe ->
            encodeWith Encode.bool maybe

        PTime maybe ->
            encodeWith Encode.string (Maybe.map Iso8601.fromTime maybe)

        PDate maybe ->
            encodeWith Encode.string (Maybe.map Iso8601.fromTime maybe)

        PJson jsonValue ->
            jsonValue
                |> Maybe.andThen
                    (Decode.decodeString Decode.value >> Result.toMaybe)
                |> Maybe.withDefault Encode.null

        Unknown _ ->
            Encode.null


isNothing : Value -> Bool
isNothing value =
    case value of
        PString (Just _) ->
            False

        PText (Just _) ->
            False

        PEnum (Just _) _ ->
            False

        PFloat (Just _) ->
            False

        PInt (Just _) ->
            False

        PBool (Just _) ->
            False

        PTime (Just _) ->
            False

        PDate (Just _) ->
            False

        _ ->
            True


updateWithString : String -> Value -> Value
updateWithString string value =
    case value of
        PString _ ->
            PString (String.nonBlank string)

        PText _ ->
            PText (String.nonBlank string)

        PEnum _ opts ->
            PEnum (String.nonBlank string) opts

        PFloat _ ->
            PFloat (String.toFloat string)

        PInt _ ->
            PInt (String.toInt string)

        PBool prev ->
            PBool (Maybe.map not prev)

        PTime _ ->
            let
                string_ =
                    if String.length string == 16 then
                        string ++ ":00"

                    else
                        string
            in
            PTime (Result.toMaybe (Iso8601.toTime string_))

        PDate _ ->
            PDate (Result.toMaybe (Iso8601.toTime string))

        PJson _ ->
            PJson (String.nonBlank string)

        Unknown _ ->
            value


isTrue : Value -> Maybe Bool
isTrue value =
    case value of
        PBool mbool ->
            mbool

        _ ->
            Nothing


toString : Value -> Maybe String
toString value =
    case value of
        PString maybe ->
            maybe

        PText maybe ->
            maybe

        PEnum maybe _ ->
            maybe

        PFloat maybe ->
            Maybe.map String.fromFloat maybe

        PInt maybe ->
            Maybe.map String.fromInt maybe

        PBool (Just True) ->
            Just "true"

        PBool (Just False) ->
            Just "false"

        PBool Nothing ->
            Nothing

        PTime maybe ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) maybe

        PDate maybe ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 10) maybe

        PJson maybe ->
            maybe

        Unknown _ ->
            Nothing
