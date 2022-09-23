module PostgRestAdmin.Record exposing
    ( Record
    , fromTable
    , Value(..)
    , value
    , id
    , location
    , label
    , errors
    , hasErrors
    , decoder
    , encode
    , getTable
    , tableName
    )

{-|


# Record

@docs Record
@docs fromTable


# Record Values

@docs Value
@docs value


# Properties

@docs id
@docs location
@docs label


# Errors

@docs errors
@docs hasErrors


# Decode/Encode

@docs decoder
@docs encode


# Table

@docs getTable
@docs tableName

-}

import Dict exposing (Dict)
import Internal.Record as Record
import Internal.Schema exposing (Table)
import Internal.Value exposing (Value(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time


{-| A Record.
-}
type alias Record =
    Record.Record


{-| -}
type Value
    = RFloat Float
    | RInt Int
    | RString String
    | RBool Bool
    | RPosix Time.Posix
    | RValue Decode.Value


{-| Create a blank record from a [Table](PostgRestAdmin.Client#Table).
-}
fromTable : Table -> Record
fromTable =
    Record.fromTable


{-| Get the [Table](PostgRestAdmin.Client#Table) for the Record.
-}
getTable : Record -> Table
getTable =
    Record.getTable


{-| Get the Record id.
-}
id : Record -> Maybe String
id =
    Record.id


{-| -}
value : String -> Record -> Maybe Value
value fieldName { fields } =
    Dict.get fieldName fields
        |> Maybe.andThen
            (\field ->
                case field.value of
                    PFloat (Just val) ->
                        Just (RFloat val)

                    PInt (Just val) ->
                        Just (RInt val)

                    PString (Just val) ->
                        Just (RString val)

                    PText (Just val) ->
                        Just (RString val)

                    PEnum (Just val) _ ->
                        Just (RString val)

                    PBool (Just val) ->
                        Just (RBool val)

                    PTime (Just val) ->
                        Just (RPosix val)

                    PDate (Just val) ->
                        Just (RPosix val)

                    PJson (Just val) ->
                        Just (RString val)

                    Unknown val ->
                        Just (RValue val)

                    _ ->
                        Nothing
            )


{-| Obtain the location for a record

    tableName record == "posts"
    -- True
    id record == Just 1
    -- True
    location record == Just "/posts?id=eq.1"
    -- True

-}
location : Record -> Maybe String
location =
    Record.location


{-| Get the Record [Table](PostgRestAdmin.Client#Table) name.
-}
tableName : Record -> String
tableName =
    Record.tableName


{-| Check if the Record has errors after
[saving](PostgRestAdmin.Client#saveRecord).
-}
hasErrors : Record -> Bool
hasErrors =
    Record.hasErrors


{-| Get the Record errors after [saving](PostgRestAdmin.Client#saveRecord).
-}
errors : Record -> Dict String (Maybe String)
errors =
    Record.errors


{-| Encode a Record.
-}
encode : Record -> Encode.Value
encode =
    Record.encode


{-| A decoder for a Record.
-}
decoder : Table -> Decoder Record
decoder =
    Record.decoder


{-| Obtain a label identifying the record, could be the title or name attribute.
-}
label : Record -> Maybe String
label =
    Record.label
