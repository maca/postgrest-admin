module Form.Record exposing
    ( Record
    , changed
    , errors
    , fromResource
    , hasErrors
    , id
    , primaryKey
    , primaryKeyName
    , setError
    , toResource
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Form.Input as Input exposing (Input)
import Postgrest.Client as PG
import Postgrest.PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Regex exposing (Regex)


type alias Record =
    Dict String Input


toResource : Record -> Resource
toResource record =
    Dict.map (\_ input -> Input.toField input) record


fromResource : Resource -> Record
fromResource resource =
    Dict.map (\_ input -> Input.fromField input) resource


changed : Record -> Bool
changed record =
    Dict.values record |> List.any (.changed << Input.toField)


errors : Record -> Dict String (Maybe String)
errors record =
    toResource record |> Resource.errors


hasErrors : Record -> Bool
hasErrors record =
    toResource record |> Resource.hasErrors


id : Record -> Maybe String
id record =
    toResource record |> Resource.id


primaryKey : Record -> Maybe PrimaryKey
primaryKey record =
    toResource record |> Resource.primaryKey


primaryKeyName : Record -> Maybe String
primaryKeyName record =
    toResource record |> Resource.primaryKeyName


setError : PG.PostgrestErrorJSON -> Record -> Record
setError error resource =
    let
        mapFun columnName key input =
            if key == columnName then
                Input.setError error input

            else
                input
    in
    error.message
        |> Maybe.andThen extractColumnName
        |> Maybe.map (mapFun >> flip Dict.map resource)
        |> Maybe.withDefault resource


extractColumnName : String -> Maybe String
extractColumnName string =
    Regex.find columnRegex string
        |> List.head
        |> Maybe.andThen (.submatches >> List.head)
        |> Maybe.withDefault Nothing


columnRegex : Regex
columnRegex =
    Regex.fromString "column \"(\\w+)\""
        |> Maybe.withDefault Regex.never