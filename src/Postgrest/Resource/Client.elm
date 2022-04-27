module Postgrest.Resource.Client exposing
    ( Client
    , create
    , fetchMany
    , fetchOne
    , jwtString
    , selects
    , update
    )

import Dict
import Postgrest.Client as PG exposing (Endpoint, Request, Selectable)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Schema exposing (Column, Constraint(..), Schema, Table)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import Url exposing (Url)
import Utils.Task exposing (Error(..))


type alias Client a =
    { a
        | schema : Schema
        , host : Url
        , authScheme : AuthScheme
    }


fetchOne : Client a -> Table -> String -> String -> Request Resource
fetchOne { host } table resourcesName id =
    let
        pkName =
            Resource.primaryKeyName table |> Maybe.withDefault ""
    in
    resourceEndpoint host resourcesName table
        |> PG.getOne
        |> PG.setParams
            [ PG.select <| selects table
            , PG.param pkName <| PG.eq <| PG.string id
            ]


fetchMany : Client a -> Table -> String -> Request (List Resource)
fetchMany { host } table resourcesName =
    resourceEndpoint host resourcesName table
        |> PG.getMany
        |> PG.setParams [ PG.select <| selects table ]


create : Client a -> Table -> String -> Resource -> Request Resource
create { host } table resourcesName resource =
    let
        endpoint =
            resourceEndpoint host resourcesName table
    in
    Resource.encode resource
        |> PG.postOne endpoint
        |> PG.setParams [ PG.select <| selects table ]


update : Client a -> Table -> String -> String -> Resource -> Request Resource
update { host } table resourcesName id resource =
    let
        pkName =
            Resource.primaryKeyName resource |> Maybe.withDefault ""

        endpoint =
            resourceEndpoint host resourcesName table

        pk =
            PG.primaryKey ( pkName, PG.string )
    in
    Resource.encode resource
        |> PG.patchByPrimaryKey endpoint pk id
        |> PG.setParams
            [ PG.select <| selects table ]


selects : Table -> List Selectable
selects table =
    Dict.values table
        |> List.filterMap associationJoin
        |> (++) (Dict.keys table |> List.map PG.attribute)


associationJoin : Column -> Maybe Selectable
associationJoin { constraint } =
    case constraint of
        ForeignKey { tableName, labelColumnName } ->
            labelColumnName
                |> Maybe.map
                    (\n -> PG.resource tableName (PG.attributes [ n, "id" ]))

        _ ->
            Nothing


resourceEndpoint : Url -> String -> Table -> Endpoint Resource
resourceEndpoint url resourcesName table =
    Resource.decoder table
        |> PG.endpoint ({ url | path = "/" ++ resourcesName } |> Url.toString)


jwtString : Client a -> Maybe String
jwtString { authScheme } =
    AuthScheme.toJwt authScheme
        |> Maybe.map PG.jwtString
