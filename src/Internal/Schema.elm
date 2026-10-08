module Internal.Schema exposing
    ( Schema, Column, ColumnType(..)
    , Reference
    , Table, label
    , tablePrimaryKeyName
    , tablePrimaryKeyColumns
    , hasSingleColumnPrimaryKey
    , recordIdFromRecord
    , recordIdFilters
    , tableToSortedColumnList
    , decoder, valueDecoder
    , Record, Value(..)
    , buildParentReference, buildReferences
    , valueToHumanString, valueToString
    )

{-|

@docs Schema, Column, ColumnType
@docs Constraint, ForeignKeyParams, Reference
@docs Table, columnNames, label
@docs tablePrimaryKeyName
@docs tableToSortedColumnList
@docs decoder, valueDecoder
@docs Record, Value
@docs buildParentReference, buildReferences

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Regex exposing (Regex)
import Url


type ColumnType
    = IntegerCol
    | FloatCol
    | StringCol
    | TextCol
    | BoolCol
    | TimestampWithoutTimezomeCol
    | TimestampCol
    | TimeWithoutTimezoneCol
    | TimeCol
    | DateCol
    | UuidCol
    | JsonCol
    | ObjectCol
    | ArrayCol ColumnType
    | OtherCol String (Maybe String)


type alias Column =
    { primaryKey : Bool
    , foreignKey : Maybe Reference
    , required : Bool
    , value : Value
    , columnType : ColumnType
    , options : List Value
    }


type alias Reference =
    { tableName : String
    , foreignKey : String
    , labelColumn : Maybe String
    }


type alias Table =
    { referencedBy : List Reference
    , name : String
    , columns : Dict String Column
    }


type Value
    = String String
    | Bool Bool
    | Int Int
    | Float Float
    | Ref { tableName : String, primaryKey : String, label : Maybe String }
    | Blank


valueToHumanString : Value -> Maybe String
valueToHumanString value =
    case value of
        Ref ref ->
            List.filterMap identity [ ref.label, Just ref.primaryKey ]
                |> String.join " - "
                |> Just

        _ ->
            valueToString value


valueToString : Value -> Maybe String
valueToString value =
    case value of
        String str ->
            Just str

        Bool True ->
            Just "true"

        Bool False ->
            Just "false"

        Int int ->
            Just (String.fromInt int)

        Float float ->
            Just (String.fromFloat float)

        Ref ref ->
            Just ref.primaryKey

        Blank ->
            Nothing


type alias Record =
    Dict String Value


type alias Schema =
    Dict String Table


tableToSortedColumnList : Table -> List ( String, Column )
tableToSortedColumnList table =
    table.columns
        |> Dict.toList
        |> List.sortBy
            (\( _, column ) ->
                if column.primaryKey then
                    0

                else if column.foreignKey /= Nothing then
                    1

                else
                    2
            )


decoder :
    { a | tables : List String, tableAliases : Dict String String }
    -> Decode.Decoder Schema
decoder params =
    let
        columnDecoder definitions requiredCols tableName columnName =
            Decode.at [ "definitions", tableName, "properties", columnName ]
                (Decode.field "type" Decode.string
                    |> Decode.andThen
                        (\type_ ->
                            Decode.maybe (Decode.field "format" Decode.string)
                                |> Decode.map (columnType type_)
                        )
                    |> Decode.andThen
                        (\type_ ->
                            Decode.map3
                                (\value options description ->
                                    ( columnName
                                    , { primaryKey =
                                            description
                                                |> Maybe.map (String.contains "Primary Key")
                                                |> Maybe.withDefault False
                                      , foreignKey =
                                            description
                                                |> Maybe.andThen
                                                    (constraintsFromDescription params.tableAliases
                                                        (definitions |> Dict.map (always .properties))
                                                    )
                                      , required = List.member columnName requiredCols
                                      , value = value
                                      , options = options
                                      , columnType = type_
                                      }
                                    )
                                )
                                (Decode.oneOf
                                    [ Decode.field "default" valueDecoder
                                    , Decode.succeed Blank
                                    ]
                                )
                                (Decode.oneOf
                                    [ Decode.field "enum" (Decode.list valueDecoder)
                                    , Decode.succeed []
                                    ]
                                )
                                (Decode.maybe (Decode.field "description" Decode.string))
                        )
                )
    in
    Decode.field "definitions"
        (Decode.dict
            (Decode.map2 (\p r -> { properties = p, requiredCols = r })
                (Decode.field "properties"
                    (Decode.dict (Decode.succeed ()) |> Decode.map Dict.keys)
                )
                (Decode.oneOf
                    [ Decode.field "required" (Decode.list Decode.string)
                    , Decode.succeed []
                    ]
                )
            )
            |> Decode.map
                (Dict.filter
                    (\k _ -> List.isEmpty params.tables || List.member k params.tables)
                )
        )
        |> Decode.andThen
            (\definitions ->
                Dict.toList definitions
                    |> List.foldl
                        (\( tableName, { properties, requiredCols } ) ->
                            Decode.map2 (::)
                                ((properties
                                    |> List.foldl
                                        (\columnName ->
                                            Decode.map2 (::)
                                                (columnDecoder definitions requiredCols tableName columnName)
                                        )
                                        (Decode.succeed [])
                                 )
                                    |> Decode.map
                                        (Dict.fromList
                                            >> Table [] tableName
                                            >> Tuple.pair tableName
                                        )
                                )
                        )
                        (Decode.succeed [])
                    |> Decode.map Dict.fromList
            )
        |> Decode.map
            (\schema ->
                Dict.map
                    (\_ table ->
                        { table | referencedBy = buildReferencedBy schema table }
                    )
                    schema
            )


{-| Every column making up the primary key, in a stable order.

Composite keys return all of them; tables with no key return `[]`.

-}
tablePrimaryKeyColumns : Table -> List ( String, Column )
tablePrimaryKeyColumns { columns } =
    columns
        |> Dict.filter (\_ column -> column.primaryKey)
        |> Dict.toList
        |> List.sortBy Tuple.first


{-| The single primary-key column, if the table has exactly one.

A composite key deliberately returns `Nothing`: there is no one column
name that identifies a row, so callers must use `tablePrimaryKeyColumns`
(or the id helpers below) instead of silently addressing rows by the
first key column and mismatching.

-}
tablePrimaryKey : Table -> Maybe ( String, Column )
tablePrimaryKey table =
    case tablePrimaryKeyColumns table of
        [ single ] ->
            Just single

        _ ->
            Nothing


tablePrimaryKeyName : Table -> Maybe String
tablePrimaryKeyName table =
    tablePrimaryKey table
        |> Maybe.map Tuple.first


{-| Whether the table is addressable by a single id segment in a URL.

-}
hasSingleColumnPrimaryKey : Table -> Bool
hasSingleColumnPrimaryKey =
    tablePrimaryKey >> (/=) Nothing


{-| Encode a record's primary key into a single URL path segment.

Single-column keys keep the historical plain form (`1172675`) so existing
links and bookmarks stay valid. Composite keys become an `k=v` list joined
by commas (`film_id=11,person_id=42`); the values are percent-encoded so
that a value containing a comma or an `=` cannot forge extra pairs.

-}
recordIdFromRecord : Table -> Record -> Maybe String
recordIdFromRecord table record =
    let
        keys =
            tablePrimaryKeyColumns table
    in
    if List.isEmpty keys then
        Nothing

    else if List.length keys == 1 then
        List.head keys
            |> Maybe.andThen (\( name, _ ) -> Dict.get name record)
            |> Maybe.andThen valueToString

    else
        keys
            |> List.foldl
                (\( name, _ ) acc ->
                    acc
                        |> Maybe.andThen
                            (\pairs ->
                                Dict.get name record
                                    |> Maybe.andThen valueToString
                                    |> Maybe.map
                                        (\value ->
                                            pairs ++ [ name ++ "=" ++ Url.percentEncode value ]
                                        )
                            )
                )
                (Just [])
            |> Maybe.map (String.join ",")


{-| The inverse of `recordIdFromRecord`: turn a path segment back into
`(column, value)` pairs suitable for `key=eq.value` PostgREST filters.

Accepts both the plain single-key form and the `k=v,...` composite form, so
it is safe to feed directly from a user-supplied URL segment. The table is
needed to name the column in the legacy single-key case.

-}
recordIdFilters : Table -> String -> List ( String, String )
recordIdFilters table segment =
    let
        decoded =
            Url.percentDecode segment |> Maybe.withDefault segment

        pairs =
            if String.contains "=" decoded then
                decoded
                    |> String.split ","
                    |> List.filterMap
                        (\pair ->
                            case String.split "=" pair of
                                [ key, value ] ->
                                    Just ( key, Url.percentDecode value |> Maybe.withDefault value )

                                _ ->
                                    Nothing
                        )

            else
                []
    in
    if List.isEmpty pairs then
        -- Legacy single-column form: `/<resource>/1172675`.
        tablePrimaryKeyName table
            |> Maybe.map (\name -> [ ( name, decoded ) ])
            |> Maybe.withDefault []

    else
        pairs


columnType : String -> Maybe String -> ColumnType
columnType type_ format =
    case ( type_, format ) of
        ( _, Just "array" ) ->
            ArrayCol (columnType type_ Nothing)

        ( "integer", _ ) ->
            IntegerCol

        ( "number", _ ) ->
            FloatCol

        ( "string", Just "timestamp without time zone" ) ->
            TimestampWithoutTimezomeCol

        ( "string", Just "timestamp with time zone" ) ->
            TimestampCol

        ( "string", Just "times without time zone" ) ->
            TimeWithoutTimezoneCol

        ( "string", Just "times with time zone" ) ->
            TimeCol

        ( "string", Just "date" ) ->
            DateCol

        ( "string", Just "uuid" ) ->
            UuidCol

        ( "string", Just "json" ) ->
            JsonCol

        ( "string", Just "text" ) ->
            TextCol

        ( "string", Just "bytea" ) ->
            OtherCol type_ format

        ( "string", Just "tsvector" ) ->
            OtherCol type_ format

        ( "string", _ ) ->
            StringCol

        ( "boolean", _ ) ->
            BoolCol

        ( "object", _ ) ->
            ObjectCol

        _ ->
            OtherCol type_ format


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.map String
        , Decode.bool |> Decode.map Bool
        , Decode.int |> Decode.map Int
        , Decode.float |> Decode.map Float
        , Decode.null Blank
        ]


constraintsFromDescription : Dict String String -> Dict String (List String) -> String -> Maybe Reference
constraintsFromDescription tableAliases colNames description =
    case extractForeignKey description of
        [ tableName, primaryKeyName ] ->
            let
                foreignTable =
                    Dict.get tableName tableAliases
                        |> Maybe.withDefault tableName
            in
            Just
                { tableName = foreignTable
                , foreignKey = primaryKeyName
                , labelColumn =
                    Dict.get foreignTable colNames
                        |> Maybe.andThen
                            (\requiredCols ->
                                List.filter
                                    (\n -> List.member n requiredCols)
                                    identifiers
                                    |> List.head
                            )
                }

        _ ->
            Nothing


foreignKeyRegex : Regex
foreignKeyRegex =
    Regex.fromString "fk table='(\\w+)' column='(\\w+)'"
        |> Maybe.withDefault Regex.never


extractForeignKey : String -> List String
extractForeignKey description =
    Regex.find foreignKeyRegex description
        |> List.concatMap .submatches
        |> List.filterMap identity


label : Table -> Maybe String
label table =
    List.filterMap
        (\fieldName ->
            if Dict.member fieldName table.columns then
                Just fieldName

            else
                Nothing
        )
        identifiers
        |> List.head



-- REFERENCES


buildReferencedBy : Schema -> Table -> List Reference
buildReferencedBy schema table =
    Dict.foldl
        (\_ otherTable acc ->
            Dict.foldl
                (\columnName column columnsAcc ->
                    case column.foreignKey of
                        Just foreignKey ->
                            if foreignKey.tableName == table.name then
                                { foreignKey = columnName
                                , tableName = otherTable.name
                                , labelColumn = foreignKey.labelColumn
                                }
                                    :: columnsAcc

                            else
                                columnsAcc

                        Nothing ->
                            columnsAcc
                )
                acc
                otherTable.columns
        )
        []
        schema


buildReferences : Table -> Dict String Reference
buildReferences table =
    Dict.foldl
        (\colName column acc ->
            case column.foreignKey of
                Just foreignKey ->
                    Dict.insert colName foreignKey acc

                Nothing ->
                    acc
        )
        Dict.empty
        table.columns


buildParentReference :
    Schema
    -> Table
    ->
        ({ id : String, tableName : String }
         ->
            Maybe
                { parentTable : Table
                , parentPrimaryKey : String
                , parentId : String
                , parentLabelColumn : String
                }
        )
buildParentReference schema table parent =
    Maybe.map2
        (\parentTable ref ->
            Maybe.map
                (\labelColumn ->
                    { parentTable = parentTable
                    , parentPrimaryKey = ref.foreignKey
                    , parentId = parent.id
                    , parentLabelColumn = labelColumn
                    }
                )
                ref.labelColumn
        )
        (Dict.get parent.tableName schema)
        (buildReferences table
            |> Dict.values
            |> List.filter (\f -> parent.tableName == f.tableName)
            |> List.head
        )
        |> Maybe.andThen identity



-- To refactor


identifiers : List String
identifiers =
    [ "title"
    , "name"
    , "full_name"
    , "email"
    , "first_name"
    , "last_name"
    , "city"
    , "country"
    ]
