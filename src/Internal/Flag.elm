module Internal.Flag exposing (custom, string, stringDict)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


string : String -> (String -> c -> Decoder c) -> (Decoder c -> Decoder c)
string =
    custom Decode.string


stringDict :
    String
    -> (Dict String (List String) -> c -> Decoder c)
    -> (Decoder c -> Decoder c)
stringDict =
    custom (Decode.dict (Decode.list Decode.string))


custom : Decoder a -> String -> (a -> c -> Decoder c) -> (Decoder c -> Decoder c)
custom decoder flagName updateFunc =
    Decode.andThen
        (\a ->
            Decode.map (Maybe.withDefault a)
                (Decode.maybe
                    (Decode.field flagName decoder
                        |> Decode.andThen (\str -> updateFunc str a)
                    )
                )
        )
