module PostgRestAdmin.Client exposing
    ( Client, init
    , Count
    , fetchRecord, fetchRecords, saveRecord, deleteRecord, count, fetchParentLabel
    , jsonRequest, bytesRequest, chunk
    , Error(..), errorToString
    , DbConstraintError, DbErrorKind(..), dbErrorKind, dbErrorMessage
    , AuthScheme(..), jwt, unset, toJwtString, authHeader, updateJwt, logout
    , fetchSchema, schemaIsLoaded
    , associationJoin
    , jsonResolver, resetToken, task
    )

{-| PostgREST client for Elm applications.


# Client

@docs Client, init


# Requests

@docs Count
@docs fetchRecord, fetchRecords, saveRecord, deleteRecord, count, fetchParentLabel
@docs jsonRequest, bytesRequest, chunk


# Errors

@docs Error, errorToString
@docs DbConstraintError, DbErrorKind, dbErrorKind, dbErrorMessage


# Auth

@docs AuthScheme, jwt, unset, toJwtString, authHeader, updateJwt, logout


# Schema

@docs fetchSchema, schemaIsLoaded
@docs associationJoin

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Http
import Internal.Schema as Schema exposing (Column, Table, Value(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import Postgrest.Client as PG exposing (Selectable)
import Regex
import String.Extra as String
import Task exposing (Task)
import Url exposing (Url)



-- TYPES


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int (Maybe String)
    | ConstraintViolation DbConstraintError
    | BadHeader String
    | DecodeError Decode.Error
    | RequestError String
    | Forbidden
    | Unauthorized


{-| A database-level rejection, parsed out of PostgREST's JSON error body.

PostgREST forwards PostgreSQL's SQLSTATE as `code` and, for constraint
violations, the offending column and constraint in `details`: a Postgres
`column "zip_code" of relation "store"` style sentence in `details` plus a
structured `column`/`constraint` pair when the violation is attributable to
a single field. Carrying those separately is what lets the UI say which
field is wrong instead of dumping the raw response.

-}
type alias DbConstraintError =
    { code : String
    , message : String
    , detail : Maybe String
    , hint : Maybe String
    , table : Maybe String
    , column : Maybe String
    , constraint : Maybe String
    }


{-| What kind of constraint the database rejected, derived from the SQLSTATE.
-}
type DbErrorKind
    = ForeignKeyReference
    | NotNull
    | Unique
    | Check
    | OtherConstraint
    | NonConstraintError


{-| Classify a SQLSTATE into the handful of cases worth wording differently.
-}
dbErrorKind : String -> DbErrorKind
dbErrorKind code =
    case code of
        "23503" ->
            ForeignKeyReference

        "23502" ->
            NotNull

        "23505" ->
            Unique

        "23514" ->
            Check

        _ ->
            if String.startsWith "23" code then
                OtherConstraint

            else
                NonConstraintError


{-| A short, field-aware explanation of a database rejection.

Names the offending column ("Zip code: ...") whenever the rejection is
attributable to one input, so the user knows what to fix instead of reading
a SQLSTATE. A foreign-key rejection is worded by direction, because the
same SQLSTATE covers two very different situations:

  - writing a value with no matching parent row
    (`... is not present in table "zip_code_info"`) → point at the field;
  - deleting a row some other table still references
    (`... is still referenced from table "inventory"`) → point at the
    referencing table, since the fix is on the other side of the relation.

-}
dbErrorMessage : DbConstraintError -> String
dbErrorMessage dbErr =
    let
        { code, message, detail, column, table, constraint } =
            dbErr

        detailText =
            Maybe.withDefault "" detail

        fieldSubject =
            column
                |> Maybe.map (\col -> String.humanize col ++ ": ")
                |> Maybe.withDefault ""

        tableSubject =
            table
                |> Maybe.map (\name -> String.humanize name ++ ": ")
                |> Maybe.withDefault ""
    in
    case dbErrorKind code of
        ForeignKeyReference ->
            if String.contains "still referenced" detailText then
                tableSubject ++ "other records still reference this one. Delete those first."

            else
                fieldSubject ++ "no matching record exists — pick an existing value."

        NotNull ->
            fieldSubject ++ "this field is required."

        Unique ->
            fieldSubject ++ "this value is already taken by another record."

        Check ->
            fieldSubject ++ "this value is not allowed by the database."

        OtherConstraint ->
            fieldSubject
                ++ "the database rejected this value"
                ++ (constraint
                        |> Maybe.map (\name -> " (" ++ name ++ ")")
                        |> Maybe.withDefault ""
                   )
                ++ "."

        NonConstraintError ->
            message


{-| PostgREST reports a constraint violation as a JSON body with the
SQLSTATE in `code`, a sentence in `message`, and the offending value in
`details`:

    {"code":"23503",
     "details":"Key (zip_code)=(99999999) is not present in table \"zip_code_info\".",
     "hint":null,
     "message":"insert or update on table \"store\" violates foreign key constraint \"store_zip_code_fkey\""}

It does *not* hand over the column or constraint as separate fields, so
they are recovered from those two sentences: `Key (col)=(value)` from
`details`, and `column "c" of relation "t"` / `on table "t"` /
`constraint "n"` from `message`. Extraction is best-effort — a body that
matches nothing still yields an error carrying the raw SQLSTATE.

-}
dbConstraintErrorDecoder : Decoder DbConstraintError
dbConstraintErrorDecoder =
    Decode.map4
        (\code message detail hint ->
            { code = code
            , message = message
            , detail = detail
            , hint = hint
            , table =
                firstCapture "on table \"([^\"]+)\"" (Just message)
                    |> orElse (firstCapture "of relation \"([^\"]+)\"" (Just message))
            , column =
                firstCapture "Key \\(([^)]+)\\)=" detail
                    |> orElse (firstCapture "column \"([^\"]+)\"" (Just message))
            , constraint = firstCapture "constraint \"([^\"]+)\"" (Just message)
            }
        )
        (Decode.field "code" Decode.string)
        (Decode.oneOf [ Decode.field "message" Decode.string, Decode.succeed "" ])
        (Decode.maybe (Decode.field "details" Decode.string))
        (Decode.maybe (Decode.field "hint" Decode.string))


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback primary =
    case primary of
        Just _ ->
            primary

        Nothing ->
            fallback


{-| The first capture group of the first match of `pattern` in `subject`.
-}
firstCapture : String -> Maybe String -> Maybe String
firstCapture pattern subject =
    subject
        |> Maybe.andThen
            (\text ->
                Regex.fromString pattern
                    |> Maybe.withDefault Regex.never
                    |> (\regex -> Regex.findAtMost 1 regex text)
                    |> List.concatMap .submatches
                    |> List.filterMap identity
                    |> List.head
            )
            |> Maybe.andThen (String.trim >> (\trimmed -> (\s -> if String.isEmpty s then Nothing else Just s) trimmed))


{-| Turn a non-2xx response into an `Error`, preferring a database
constraint explanation when the body carries one.
-}
badStatusError : Int -> String -> Error
badStatusError status body =
    case Decode.decodeString dbConstraintErrorDecoder body of
        Ok dbErr ->
            if dbErrorKind dbErr.code == NonConstraintError then
                BadStatus status (Decode.decodeString badStatusDecoder body |> Result.withDefault Nothing)

            else
                ConstraintViolation dbErr

        Err _ ->
            BadStatus status (Decode.decodeString badStatusDecoder body |> Result.withDefault Nothing)


type alias Count =
    { from : Int
    , to : Int
    , total : Int
    }


type AuthScheme
    = Jwt PG.JWT
    | Unset


{-| Represents a client for a PostgREST instance, including authentication
params.

See [Config](PostgRestAdmin-Config) and
[Config.FormAuth](PostgRestAdmin-Config-FormAuth) for authentication
configuration options.

-}
type alias Client =
    { host : Url
    , authScheme : AuthScheme
    , schema : Schema.Schema
    , headers : List Http.Header
    , tables : List String
    , tableAliases : Dict String String
    }


{-| Obtain the JWT as a string.
-}
toJwtString : Client -> Maybe String
toJwtString { authScheme } =
    toJwt authScheme
        |> Maybe.map PG.jwtString


toJwt : AuthScheme -> Maybe PG.JWT
toJwt authScheme =
    case authScheme of
        Jwt token ->
            Just token

        Unset ->
            Nothing


{-| Generate an authorization header.
-}
authHeader : Client -> Maybe Http.Header
authHeader client =
    toJwtString client
        |> Maybe.map (\jwtStr -> Http.header "Authorization" ("Bearer " ++ jwtStr))



-- AUTH CONFIG


jwt : String -> AuthScheme
jwt tokenStr =
    Jwt (PG.jwt tokenStr)


unset : AuthScheme
unset =
    Unset


resetToken : Client -> Client
resetToken client =
    { client | authScheme = Unset }



-- CLIENT FUNCTIONS


init :
    { host : Url
    , authScheme : AuthScheme
    , headers : List Http.Header
    , tables : List String
    , tableAliases : Dict String String
    }
    -> Client
init params =
    { host = params.host
    , authScheme = params.authScheme
    , schema = Dict.empty
    , headers = params.headers
    , tables = params.tables
    , tableAliases = params.tableAliases
    }


updateJwt : String -> Client -> Client
updateJwt tokenStr client =
    { client | authScheme = updateAuthSchemeJwt tokenStr client.authScheme }


updateAuthSchemeJwt : String -> AuthScheme -> AuthScheme
updateAuthSchemeJwt tokenStr _ =
    Jwt (PG.jwt tokenStr)


logout : Client -> Client
logout client =
    { client
        | authScheme = clearAuthSchemeJwt client.authScheme
        , schema = Dict.empty
    }


clearAuthSchemeJwt : AuthScheme -> AuthScheme
clearAuthSchemeJwt _ =
    Unset


schemaIsLoaded : Client -> Bool
schemaIsLoaded { schema } =
    not (Dict.isEmpty schema)


fetchSchema : Client -> Task Error Schema.Schema
fetchSchema client =
    task
        { client = client
        , method = "GET"
        , headers = []
        , path = "/"
        , body = Http.emptyBody
        , resolver =
            jsonResolver
                (Schema.decoder
                    { tables = client.tables
                    , tableAliases = client.tableAliases
                    }
                )
        }


{-| Transform [Error](#Error) to an error explanation.
-}
errorToString : Error -> String
errorToString error =
    case error of
        BadUrl msg ->
            "Invalid URL:" ++ msg

        Timeout ->
            "The requested timed out. Please try again."

        NetworkError ->
            "Network Error: do you have an internet connection?"

        BadStatus status body ->
            "The server responded with an error: "
                ++ (body |> Maybe.withDefault (String.fromInt status))

        ConstraintViolation dbErr ->
            dbErrorMessage dbErr

        DecodeError err ->
            Decode.errorToString err

        RequestError msg ->
            msg

        Forbidden ->
            "Forbidden: you don't have permission to access this resource."

        Unauthorized ->
            "Unauthorized: please check your credentials."

        BadHeader headerName ->
            "The header \"" ++ headerName ++ "\" could not be read."


removeLeadingOrTrailingSlash : String -> String
removeLeadingOrTrailingSlash =
    Regex.replace
        (Maybe.withDefault Regex.never (Regex.fromString "^/|/$"))
        (always "")



-- VIEW


jsonResolver : Decode.Decoder a -> Http.Resolver Error a
jsonResolver decoder =
    Http.stringResolver
        (resolve
            (\_ body ->
                Decode.decodeString decoder body
                    |> Result.mapError DecodeError
            )
            badStatusError
        )


countResolver : Decode.Decoder a -> Http.Resolver Error ( a, Count )
countResolver decoder =
    Http.stringResolver
        (resolve
            (\{ headers } body ->
                case
                    Dict.get "content-range" headers
                        |> Maybe.map (Parser.run countParser)
                of
                    Just (Ok recordCount) ->
                        Decode.decodeString
                            (decoder |> Decode.map (\a -> ( a, recordCount )))
                            body
                            |> Result.mapError DecodeError

                    _ ->
                        Err (BadHeader "content-range")
            )
            badStatusError
        )


resolve :
    (Http.Metadata -> body -> Result Error value)
    -> (Int -> body -> Error)
    -> Http.Response body
    -> Result Error value
resolve fn parseError response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                401 ->
                    Err Unauthorized

                403 ->
                    Err Forbidden

                _ ->
                    Err (parseError statusCode body)

        Http.GoodStatus_ metadata body ->
            fn metadata body


badStatusDecoder : Decoder (Maybe String)
badStatusDecoder =
    Decode.map4
        (\code details hint message ->
            List.filterMap identity
                [ code
                , details
                , hint
                , message
                ]
                |> String.join "\n"
                |> Just
        )
        (Decode.maybe (Decode.field "code" Decode.string))
        (Decode.maybe (Decode.field "details" Decode.string))
        (Decode.maybe (Decode.field "hint" Decode.string))
        (Decode.maybe (Decode.field "message" Decode.string))


{-| Fetches a record for a given table.
Requires a decoder for the response and an `expect` function that returns a `Msg`.

    import Dict
    import Json.Decode as Decode
    import PostgRestAdmin.Cmd as AppCmd

    fetchOne : (Result Error Decode.Value -> msg) -> String -> String -> Client -> AppCmd.Cmd Msg
    fetchOne tagger tableName recordId client =
        case Dict.get tableName client.schema of
            Just table ->
                fetchRecord
                    { client = client
                    , table = table
                    , id = recordId
                    , decoder = Decode.value
                    , expect = tagger
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecord : { client : Client, table : Table, id : String } -> Task Error Schema.Record
fetchRecord { client, table, id } =
    let
        queryStr =
            PG.toQueryString
                (List.filterMap identity
                    [ Just (PG.select (selects table))
                    , Schema.recordIdFilters table id |> filtersToQuery
                    , Just (PG.limit 1)
                    ]
                )
    in
    task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ table.name ++ "?" ++ queryStr
        , body = Http.emptyBody
        , resolver = jsonResolver (recordDecoder table)
        }


filtersToQuery : List ( String, String ) -> Maybe PG.Param
filtersToQuery filters =
    let
        params =
            List.filterMap
                (\( column, value ) ->
                    if String.isEmpty column then
                        Nothing

                    else
                        Just (PG.param column (PG.eq (PG.string value)))
                )
                filters
    in
    if List.isEmpty params then
        Nothing

    else
        Just (PG.and params)


fetchRecords :
    { client : Client
    , table : Table
    , path : String
    }
    -> Task Error ( List Schema.Record, Count )
fetchRecords { client, table, path } =
    task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Prefer" "count=exact" ]
        , path = path
        , body = Http.emptyBody
        , resolver = countResolver (Decode.list (recordDecoder table))
        }


fetchParentLabel :
    Client
    ->
        { parentLabelColumn : String
        , parentPrimaryKey : String
        , parentId : String
        , parentTable : { b | name : String }
        }
    -> Task.Task Error String
fetchParentLabel client params =
    let
        selectedCols =
            PG.select
                [ PG.attribute params.parentLabelColumn ]

        queryString =
            PG.toQueryString
                [ selectedCols
                , PG.param params.parentPrimaryKey (PG.eq (PG.string params.parentId))
                , PG.limit 1
                ]
    in
    task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ params.parentTable.name ++ "?" ++ queryString
        , body = Http.emptyBody
        , resolver = jsonResolver (Decode.field params.parentLabelColumn Decode.string)
        }


saveRecord :
    { client : Client
    , body : Encode.Value
    , table : Table
    , id : Maybe String
    , decoder : Decode.Decoder a
    }
    -> Task.Task Error a
saveRecord { client, body, table, decoder, id } =
    let
        queryString =
            PG.toQueryString [ PG.select (selects table) ]

        idFilters =
            id
                |> Maybe.map (Schema.recordIdFilters table)
                |> Maybe.withDefault []
                |> List.map (\( column, value ) ->
                    Url.percentEncode column ++ "=eq." ++ Url.percentEncode value
                )

        ( path, method ) =
            if List.isEmpty idFilters then
                ( "/" ++ table.name ++ "?" ++ queryString
                , "POST"
                )

            else
                -- A PATCH without a filter would rewrite every row; refuse
                -- to fall back to POST when an id *was* given but could not
                -- be turned into a filter (e.g. a table with no single key).
                ( "/"
                    ++ table.name
                    ++ "?"
                    ++ String.join "&" idFilters
                    ++ "&"
                    ++ queryString
                , "PATCH"
                )
    in
    task
        { client = client
        , method = method
        , headers =
            [ Http.header "Prefer" "return=representation"
            , Http.header "Accept" "application/vnd.pgrst.object+json"
            ]
        , path = path
        , body = Http.jsonBody body
        , resolver = jsonResolver decoder
        }


{-| Deletes a record.
`expect` param requires a function that returns a `Msg`.

    import Dict
    import PostgRestAdmin.Cmd as AppCmd

    delete : (Result Error () -> Msg) -> String -> String -> Client -> AppCmd.Cmd Msg
    delete tagger tableName recordId client =
        case Dict.get tableName client.schema of
            Just table ->
                deleteRecord
                    { client = client
                    , table = table
                    , id = recordId
                    , expect = tagger
                    }

            Nothing ->
                AppCmd.none

-}
deleteRecord :
    { client : Client
    , table : Table
    , id : String
    }
    -> Task.Task Error ()
deleteRecord { client, table, id } =
    let
        filters =
            Schema.recordIdFilters table id
                |> List.map (\( column, value ) ->
                    Url.percentEncode column ++ "=eq." ++ Url.percentEncode value
                )

        queryStr =
            String.join "&" filters
    in
    if List.isEmpty filters then
        -- Deleting with no usable filter would wipe the whole table.
        Task.fail (RequestError "This table has no usable primary key, so a single record cannot be identified. Delete was not sent.")

    else
        task
            { client = client
            , method = "DELETE"
            , headers = []
            , path = "/" ++ table.name ++ "?" ++ queryStr
            , body = Http.emptyBody
            , resolver = jsonResolver (Decode.succeed ())
            }


{-| Perform a request
-}
jsonRequest :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , decoder : Decoder a
    }
    -> Task Error a
jsonRequest params =
    task
        { client = params.client
        , method = params.method
        , headers = params.headers
        , path = params.path
        , body = params.body
        , resolver = jsonResolver params.decoder
        }


task :
    { a
        | client : Client
        , method : String
        , headers : List Http.Header
        , path : String
        , body : Http.Body
        , resolver : Http.Resolver Error b
    }
    -> Task Error b
task { client, method, headers, path, body, resolver } =
    Http.task
        { method = method
        , headers =
            List.concat
                [ client.headers
                , authHeader client
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                , headers
                ]
        , url = endpoint client path
        , body = body
        , resolver = resolver
        , timeout = Nothing
        }


chunk : Client -> Table -> List String -> Task.Task Error (List String)
chunk client table ids =
    let
        keyAttrs =
            Schema.tablePrimaryKeyColumns table
                |> List.map Tuple.first

        rowFilters =
            ids
                |> List.filterMap
                    (\id ->
                        let
                            filters =
                                Schema.recordIdFilters table id
                                    |> List.map (\( column, value ) -> PG.param column (PG.eq (PG.string value)))
                        in
                        if List.isEmpty filters then
                            Nothing

                        else
                            Just (PG.and filters)
                    )
    in
    if List.isEmpty keyAttrs || List.isEmpty rowFilters then
        Task.succeed []

    else
        task
            { client = client
            , method = "GET"
            , headers = []
            , path =
                "/"
                    ++ table.name
                    ++ "?"
                    ++ PG.toQueryString
                        [ PG.or rowFilters
                        , PG.select (List.map PG.attribute keyAttrs)
                        ]
            , body = Http.emptyBody
            , resolver =
                jsonResolver
                    (Decode.list (Decode.dict Schema.valueDecoder)
                        |> Decode.map (List.filterMap (Schema.recordIdFromRecord table))
                    )
            }


idDecoder : Decoder String
idDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.float |> Decode.map String.fromFloat
        ]


bytesRequest :
    { a
        | client : Client
        , method : String
        , headers : List Http.Header
        , path : String
        , body : Http.Body
    }
    -> Task Error Bytes
bytesRequest { client, method, headers, path, body } =
    task
        { client = client
        , method = method
        , headers = headers
        , path = path
        , body = body
        , resolver = Http.bytesResolver (resolve (always Ok) (\status _ -> BadStatus status Nothing))
        }


count : { client : Client, path : String } -> Task Error Int
count { client, path } =
    task
        { client = client
        , method = "HEAD"
        , headers = [ Http.header "Prefer" "count=exact" ]
        , path = path
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (resolve
                    (\{ headers } _ ->
                        case
                            Dict.get "content-range" headers
                                |> Maybe.map (Parser.run countParser)
                        of
                            Just (Ok recordCount) ->
                                Ok recordCount.total

                            _ ->
                                Err (BadHeader "content-range")
                    )
                    badStatusError
                )
        }


endpoint : Client -> String -> String
endpoint { host } path =
    Url.toString
        { host
            | path =
                "/"
                    ++ ([ host.path, path ]
                            |> List.filterMap
                                (removeLeadingOrTrailingSlash >> String.nonBlank)
                            |> String.join "/"
                       )
        }



-- UTILS


selects : Table -> List Selectable
selects table =
    List.concat
        [ Dict.values table.columns
            |> List.filterMap associationJoin
        , Dict.keys table.columns
            |> List.map PG.attribute
        ]


associationJoin : Column -> Maybe Selectable
associationJoin { foreignKey } =
    case foreignKey of
        Just fk ->
            fk.labelColumn
                |> Maybe.map
                    (\n ->
                        PG.resource fk.tableName
                            (PG.attributes [ n, fk.foreignKey ])
                    )

        Nothing ->
            Nothing



-- DECODE


recordDecoder : Table -> Decode.Decoder Schema.Record
recordDecoder table =
    table.columns
        |> Dict.foldl
            (\name col ->
                Decode.map2
                    (Maybe.map (Dict.insert name) >> Maybe.withDefault identity)
                    (Decode.maybe (columnDecoder name col))
            )
            (Decode.succeed Dict.empty)


columnDecoder : String -> Schema.Column -> Decoder Value
columnDecoder name col =
    case col.foreignKey of
        Just ref ->
            Decode.oneOf
                [ Decode.field ref.tableName
                    (Decode.map2 Tuple.pair
                        (ref.labelColumn
                            |> Maybe.map (\c -> Decode.maybe (Decode.field c Decode.string))
                            |> Maybe.withDefault (Decode.succeed Nothing)
                        )
                        (Decode.field ref.foreignKey idDecoder)
                    )
                , Decode.field name idDecoder |> Decode.map (Tuple.pair Nothing)
                ]
                |> Decode.map
                    (\( label, id ) ->
                        Ref { tableName = ref.tableName, primaryKey = id, label = label }
                    )

        Nothing ->
            Decode.field name Schema.valueDecoder


countParser : Parser Count
countParser =
    Parser.succeed
        (\( from, to ) recordCount ->
            Count from to (Maybe.withDefault to recordCount)
        )
        |= Parser.oneOf
            [ Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.symbol "-"
                |= Parser.int
            , Parser.succeed ( 0, 0 )
                |. Parser.symbol "*"
            ]
        |. Parser.symbol "/"
        |= Parser.oneOf
            [ Parser.map Just Parser.int
            , Parser.map (always Nothing) (Parser.symbol "*")
            ]
