# Bug Dossier — postgrest-admin-preview

One section per reported bug. Each section records the issue, steps to reproduce,
supporting evidence (database records / schema / screenshots), the root cause,
and a minimal `elm` fix sketch. This dossier accompanies the Gherkin scenarios in
`example/features/*.feature` and the numbered summary in `README.md`; the
numbered bugs here match that list.

App under test: `example/` demo app (elm-watch dev server on `:9000`, PostgREST
on `:9080`, Postgres on `:5432`), logged in as `bluebox@example.com` / `password`.

---
## Bug 1 — JWT never persists to `sessionStorage`

### Issue
After a successful login, any full page reload drops the user back to a login
form and loses all app state. `static/index.html` subscribes to the `loggedIn`
port and calls `sessionStorage.setItem("jwt", jwt)`, but `sessionStorage`
stays empty after login — the port never fires with a token.

### Steps to reproduce
1. Open `http://localhost:9000/`, fill the root login form
   (API URL `http://localhost:9080`, auth endpoint `http://localhost:9080/rpc/login`,
   login `bluebox@example.com`, password `password`), click `LOGIN`.
2. You land on `/customer` and the sidebar appears — logged in.
3. Check `sessionStorage` (e.g. `sessionStorage.getItem("jwt")`) → `null`.
4. Reload the page → login form again, all state gone.

### Supporting evidence
- `example/static/index.html` wires `app.ports.loggedIn` to
  `sessionStorage.setItem("jwt", jwt)` — the mechanism is present and correct.
- The port is never invoked: `sessionStorage` remains empty after login.

### Root cause
`config.onLogin` (the callback `Main.elm` wires to the `loggedIn` port) is only
ever called from **one** place in the whole codebase: PostgRestAdmin.elm's own
`update` in `GotToken (Ok tokenStr)`:

```elm
GotToken (Ok tokenStr) ->
    ( { model | client = updatedClient, authFormField = authFormField }
    , Cmd.batch
        [ config.onLogin tokenStr |> Cmd.map (always NoOp)
        , Client.fetchSchema updatedClient |> Task.attempt SchemaFetched
        ]
    )
```

That `GotToken` is the result of the library's **own** built-in login form
submitting via `requestToken` — the form shown on any non-`/` path. The demo's
custom root form never goes through that code path at all. Its
`AuthFormSubmitted` handler performs the POST itself and, on success, calls:

```elm
PostgRestAdmin.applyConfiguration
    (PostgRestAdmin.configure
        |> PostgRestAdmin.withJWT token
        |> PostgRestAdmin.withHost apiHost
    )
    model.adminAppModel
```

`withJWT` only appends a `clientAttributes` transformer
(`\attrs -> { attrs | authScheme = Client.jwt tokenStr }`), and
`applyConfiguration` folds that into `model.client` directly — it never touches
`config.onLogin` or dispatches `GotToken` in any way. So logging in through the
form actually used on `/` can never make it to `sessionStorage`, no matter how
many times you do it.

### Possible fix
Fire `onLogin` on the successful token path of the custom form. The demo's
`AuthFormSubmitted`/`GotToken (Ok token)` handlers should invoke the same
callback the library's own form does:

```elm
GotToken (Ok token) ->
    let
        ( adminModel, adminCmd ) =
            PostgRestAdmin.applyConfiguration
                (PostgRestAdmin.configure |> PostgRestAdmin.withJWT token)
                model.adminAppModel
    in
    ( { model | adminAppModel = adminModel }
    , Cmd.batch
        [ PostgRestAdmin.onLogin token adminAppConfig |> Cmd.map AdminAppChanged
        , Cmd.map AdminAppChanged adminCmd
        ]
    )
```

(Or, cleaner: reuse the library's own form/`requestToken` path on `/` instead of
duplicating it, so `GotToken` — and therefore `onLogin` — is exercised exactly
as designed.)

---
## Bug 2 — Login form posts to hardcoded `localhost:3000`

### Issue
The library's built-in login form (shown on any path other than `/` while
logged out — including after a deep link or after clicking `LOGOUT`) posts to a
hardcoded `http://localhost:3000/rpc/login` instead of the configured host
`http://localhost:9080/rpc/login`. The POST fails with
`net::ERR_CONNECTION_REFUSED` (nothing listens on port 3000 — confirmed via
`nc -z localhost 3000` → CLOSED), so login is impossible from that view. There
is no link back to `/` in the UI; the only way out is typing
`http://localhost:9000/` manually in the address bar.

### Steps to reproduce
1. Log in on `/` as usual.
2. Click `LOGOUT` (or navigate to `http://localhost:9000/customer` while logged out).
3. The broken Login/Password-only form appears (no API-URL/host fields).
4. Fill Login/Password and click `LOGIN` → "There was an issue reaching the
   server, please try again later." — nothing listens on `localhost:3000`.

### Supporting evidence
- `nc -z localhost 3000` → connection refused (port 3000 closed).
- Live-verified: the deep-link and the `LOGOUT` button both reproduce this
  exactly; `LoggedOut` in PostgRestAdmin.elm only resets `model.client`, it
  never touches the URL path, so the non-`/` branch of Main.elm's view (and the
  same hardcoded form) is shown immediately.

### Root cause
`model.authFormUrl` — the URL the library's built-in login form posts to — is set
once at init:

```elm
init ... =
    ( { authFormUrl = { defaultHost | path = "/rpc/login" }
      , ...
      }
    , ...
    )

defaultHost : Url.Url
defaultHost =
    { protocol = Http
    , host = "localhost"
    , port_ = Just 3000
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }
```

It is only ever changed by `withLoginUrl`, which appends an `attributes`
transformer `(\model -> { model | authFormUrl = u })`. `withHost` — which the
demo *does* call — only prepends a `clientAttributes` transformer
`(\attrs -> { attrs | host = u })`, used for ordinary API requests, and never
touches `authFormUrl`:

```elm
withHost urlStr (Config conf errors) =
    case Url.fromString urlStr of
        Just u ->
            Config
                { conf
                    | clientAttributes =
                        (\attrs -> { attrs | host = u }) :: conf.clientAttributes
                }
                errors
        ...
```

Since the demo never calls `withLoginUrl`, the library's own login form
silently keeps posting to the library's example host forever.

### Possible fix
The demo should configure the login URL alongside the API host — either call
`withLoginUrl` with the same endpoint, or make `withHost` also set `authFormUrl`
when `authFormUrl` is still at its default. The least-surprise fix (library
side, since this is as much a footgun as a demo bug) is to make `withHost`
co-opt `authFormUrl` if it still points at `defaultHost`:

```elm
withHost urlStr (Config conf errors) =
    case Url.fromString urlStr of
        Just u ->
            let
                attrs =
                    (\attrs -> { attrs | host = u })
                        :: conf.clientAttributes
                attributes =
                    (\model ->
                        if model.authFormUrl == { defaultHost | path = "/rpc/login" } then
                            { model | authFormUrl = { u | path = "/rpc/login" } }
                        else
                            model
                    )
                        :: conf.attributes
            in
            Config { conf | clientAttributes = attrs, attributes = attributes } errors
        Nothing -> ...
```

The demo-side equivalent is simply wiring `withLoginUrl "http://localhost:9080/rpc/login"`
in `Main.elm` when it configures the app.

---
## Bug 3 — Wrong credentials fail completely silently

### Issue
Entering a wrong password (or an invalid "Your own JWT" token) on the root
login form (`/`) fails completely silently: no message, no shake, nothing
changes on screen. A user who mistypes their password has no way to tell the
click even registered.

### Steps to reproduce
1. Open `http://localhost:9000/`.
2. Fill API URL `http://localhost:9080`, auth endpoint `http://localhost:9080/rpc/login`,
   login `bluebox@example.com`, password `wrong-password`.
3. Click `LOGIN` → nothing happens; no error text appears.
   (Screenshot of the resulting state: `/tmp/wrong-login.png`.)

### Supporting evidence
- The POST to `/rpc/login` correctly returns 401 (verified against PostgREST).
- Screenshot `/tmp/wrong-login.png` shows the form unchanged with no error.

### Root cause
`Main.elm`'s `GotToken (Err _)` handler discards the error outright:

```elm
GotToken (Err _) ->
    ( model, Cmd.none )
```

The "Your own JWT" mode has the same problem one level up: submitting a garbage
token calls `PostgRestAdmin.applyConfiguration`, which dispatches a schema-fetch
request that also comes back 401. That failure *is* tracked in the library's
model (`SchemaFetched (Err err) -> { model | client = Client.logout
model.client, authFormStatus = Failure err }`), but Main.elm's custom view
(shown on `/`) never reads `authFormStatus` at all — it has its own separate,
error-blind `AuthFormSubmitted`/`GotToken` handling instead of reusing the
library's.

This is squarely a **demo** bug, not a library one: PostgRestAdmin.elm's own
default login view already does this correctly:

```elm
viewAuthForm model =
    Html.div [ Attrs.class "auth-modal overlay" ]
        [ Html.div [ Attrs.class "auth-form" ]
            [ errorMessage model.authFormStatus
            , ...
            ]
        ]

errorMessage status =
    case status of
        Failure error ->
            Html.div [ Attrs.class "form-error-message" ]
                (case error of
                    Client.Forbidden ->
                        [ Html.text """You may have entered the wrong password, please try again.""" ]
                    ...
                )
```

### Possible fix
Route the error into a visible message on the custom form. The simplest fix is
to stop discarding the error and surface it, e.g. track a small
`loginError : Maybe String` in Main.elm's model:

```elm
GotToken (Err err) ->
    ( { model | loginError = Just "Login failed. Please check your credentials." }
    , Cmd.none
    )
```

…and render it above the form (mirroring the library's `errorMessage`). The
library already classifies `Client.Forbidden` vs `Client.Unauthorized` etc.;
the demo should reuse that classification instead of the raw `Http.Error` so
the message matches the library's wording.

---
## Bug 4 — No database constraint violation is ever translated into a field-level message

### Issue
Every write error — NOT NULL, malformed value, foreign key, duplicate key — is
surfaced to the user as a raw Postgres dump: SQLSTATE codes, table names, column
names and internal constraint names, verbatim, with nothing connecting them
back to the form field that was actually wrong. Three concrete instances
(NOT NULL, malformed `22P02`, foreign key `23503`), plus the delete `23503` and
duplicate-key `23505` scenarios, are captured in `database_constraints.feature`.

### Steps to reproduce
- **NOT NULL** (`store`): open Store → NEW STORE → fill Phone only → SAVE →
  `The server responded with an error: 23502 ... null value in column
  \"store_id\" of relation \"store\" violates not-null constraint`.
- **Malformed value** (CSV): upload a CSV with a non-numeric `store_id` →
  `The server responded with an error: 22P02 invalid input syntax for type
  bigint: "not_a_number"`.
- **Foreign key** (`store`): set Zip code to nonexistent `99999999` → PATCH
  `/store?store_id=eq.18` → 409 → `Key (zip_code)=(99999999) is not present in
  table "zip_code_info" ... violates foreign key constraint
  "store_zip_code_fkey"`.
- **Delete FK** (Film "The Swan", id 1172675): DELETE → confirm → `23503 Key
  (film_id)=(1172675) is still referenced from table "inventory" ... violates
  foreign key constraint "inventory_film_id_fkey"`.
- **Duplicate key** (Person): NEW PERSON → Name only → SAVE → `23505 Key
  (person_id)=(1) already exists ... violates unique constraint "person_pkey"`.

### Supporting evidence
- `curl` reproductions: PATCH `/store?store_id=eq.18` with bad zip → HTTP 409
  body `{"code":"23503","details":"Key (zip_code)=(99999999) is not present in
  table \"zip_code_info\".","hint":null,"message":"insert or update on table
  \"store\" violates foreign key constraint \"store_zip_code_fkey\"}"`.
- Live-verified this session: delete-FK and duplicate-key paths both show the
  same raw-dump pattern.

### Root cause
`Client.resolve` maps any non-401/403 bad status to `BadStatus`, and
`errorToString` renders the raw body verbatim:

```elm
resolve fn parseError response =
    case response of
        ...
        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                401 -> Err Unauthorized
                403 -> Err Forbidden
                _   -> Err (BadStatus statusCode (parseError body))

errorToString error =
    case error of
        BadStatus status body ->
            "The server responded with an error: "
                ++ (body |> Maybe.withDefault (String.fromInt status))
        ...
```

`badStatusDecoder` just extracts `code / details / hint / message` and joins
them with newlines — no SQLSTATE mapping, no column-name→field-name resolution:

```elm
badStatusDecoder =
    Decode.map4
        (\code details hint message ->
            List.filterMap identity [ code, details, hint, message ]
                |> String.join "\n" |> Just
        )
        ...
```

There is no handling anywhere in `src/` for any specific SQLSTATE (no reference
to 23502, 23503, 23505, 22P02 or similar) and no mapping from a constraint or
column name back to the form field that produced it.

### Possible fix
Translate SQLSTATE `code`s into friendly, field-targeted messages before
rendering. Add a classifier keyed on the PostgREST error `code` and use the
`details` (which names the offending column) to point at the field:

```elm
type alias ApiError =
    { code : String
    , details : Maybe String
    , hint : Maybe String
    , message : Maybe String
    }

friendlyError : ApiError -> String
friendlyError { code, details, message } =
    case code of
        "23502" -> "The field '" ++ columnFromDetails details ++ "' needs a value."
        "23503" -> "This record is still referenced elsewhere; remove those references first."
        "23505" -> "This value is already taken. Choose a different one."
        "22P02" -> "That column had a badly formatted value."
        _       -> message |> Maybe.withDefault "The server rejected the request."

-- and in errorToString / BadStatus, render friendlyError instead of the raw body.
```

The `details` field carries `Key (column)=(value)...` / `null value in column
"store_id"`, which can be parsed back to the form's field label. The cleanest
fix keeps the raw body for debugging but renders the friendly text to the user.

---
## Bug 5 — False "Expecting a STRING" error after a successful save

### Issue
Saving any record whose primary key is numeric (i.e. almost every table in this
schema) shows a false error overlay after a **successful** save:
`Problem with the value at json['store_id']: 18 - Expecting a STRING`. The write
itself succeeds and the field IS persisted (confirmed by reloading the detail
page), but the app cannot tell the user the save worked — it looks like it
failed.

### Steps to reproduce
1. Open Store → VIEW on store id 18 → EDIT → change Phone → SAVE.
2. Raw error overlay appears: `Problem with the value at json['store_id']: 18 -
   Expecting a STRING`.
3. Reload `/store/18` → the edit was actually persisted.

### Supporting evidence
- PostgREST returns `store_id` as a JSON number (`18`), not a string.
- Every table in this schema has an integer primary key; no UUID/string keys
  exist (checked via the OpenAPI document) — so this is not a Store quirk, it
  breaks the "confirm the save" step for editing literally any record.

### Root cause
`Internal/PageForm.elm`, `Submitted`, reads the id back out of the save
response with a decoder hardcoded to `Decode.string`, regardless of the primary
key's actual column type:

```elm
Submitted ->
    ( { model | form = Field.validate model.form |> Field.touch }
    , case Parse.parse Parse.json model.form of
        Ok jsonBody ->
            Client.saveRecord
                { client = model.client
                , id = model.id
                , body = jsonBody
                , table = model.table
                , decoder =
                    case Schema.tablePrimaryKeyName model.table of
                        Just pkName ->
                            Decode.field pkName Decode.string   -- <-- always string

                                Nothing ->
                                    Decode.fail "Coudn't figure the field id"
                }
                |> Task.attempt Saved
                |> AppCmd.wrap
        ...
    )
```

### Possible fix
Decode the id by its actual column type. The schema knows each column's type
(`Schema.columnType` / the OpenAPI type), so the decoder should be built from
that rather than hardcoding `Decode.string`:

```elm
idDecoder : Schema.Column -> Decode.Decoder String
idDecoder col =
    case col.type of
        Schema.Int  -> Decode.int |> Decode.map String.fromInt
        Schema.Float-> Decode.float |> Decode.map String.fromFloat
        _           -> Decode.string
```

then use `idDecoder` (derived from the pk column's type) in place of
`Decode.field pkName Decode.string`. This makes the "confirm the save" step
work for integer, float, and string keys alike.

---
## Bug 6 — Creating a record impossible on tables whose primary key has no database default/identity

### Issue
On any table whose primary key column has no database default or identity
generation, the "New record" form omits the PK field and saving always fails
with `23502 null value in column "store_id" of relation "store" violates
not-null constraint`. Affected: Customer, Film, Store, Production Company,
Release Type and Zip Code Info (confirmed via `information_schema`), not just
Store.

### Steps to reproduce
1. Open Store → NEW STORE → fill Phone → SAVE.
2. `The server responded with an error: 23502 ... null value in column
   \"store_id\" of relation \"store\" violates not-null constraint`.
3. Creating a Store (or Customer/Film/Production Company/Release Type/Zip Code
   Info) through the UI is not currently possible.

### Supporting evidence
- `information_schema.columns` (bluebox schema) for each table:
  - **Auto-generated keys** (`column_default` / `is_identity` present):
    `inventory_id`, `payment_id`, `film_genre.genre_id`, `person_id`,
    `rental_id`.
  - **Manually-assigned integer PKs, no default/identity**:
    `customer_id`, `film_id`, `production_company_id`, `release_type_id`,
    `store_id`, `zip_code_info.zip_code`.

### Root cause
`Internal/PageForm.elm`, `fieldFromColumn`, drops primary-key columns from every
create/edit form unconditionally, assuming the database always generates the
key:

```elm
fieldFromColumn ( name, column ) =
    let attrs = ... in
    if column.primaryKey then
        Nothing   -- <-- PK field always omitted
    else if column.foreignKey /= Nothing then
        Just (Field.strictAutocomplete attrs)
    else
        ...
```

This holds for serial/identity columns but not for manually-assigned PKs such
as Store's. With the field dropped, the POST body lacks `store_id`, so Postgres
rejects it.

### Possible fix
Only drop the PK field when the database actually generates the value. The
schema fetch already knows whether a column is auto-generated — keep the PK
field editable (or required) for tables without a generated key:

```elm
fieldFromColumn ( name, column ) =
    let attrs = ... in
    if column.primaryKey && column.autoGenerated then
        Nothing
    else
        Just ( ... field for the PK ... )
```

where `column.autoGenerated` is populated from the OpenAPI `x-postgrest` /
`default` / `identity` metadata during schema fetch. For manually-assigned PKs,
the form then submits the id the user typed (or pre-fills the next free value),
and creation works.

---
## Bug 7 — Tables without an exposed primary key get no "VIEW" links; delete/save/fetch don't guard a missing key

### Issue
For tables where PostgREST omits a `"Primary Key"` column description in its
schema — Staff, Language, Holiday — the app renders no "VIEW" links, so a
record can never be opened, edited, or confirmed-saved from the UI. Worse, the
low-level `deleteRecord`, `saveRecord` and `fetchRecord` functions in
`PostgRestAdmin/Client.elm` don't guard against a missing key: `deleteRecord`
would send a **filter-less** `DELETE` (i.e. delete every row in the table) if it
were ever reachable. It currently isn't (no "VIEW" link → no path to a Delete
button) — but that is an accident of the *other* bug, not a safety check.

### Steps to reproduce
1. Open Staff (or Language / Holiday) from the sidebar.
2. The listing shows rows but no "VIEW" links.
3. There is no way to open a record, edit it, or reach a Delete button.

### Supporting evidence
- PostgREST's OpenAPI description for `staff`, `language`, `holiday` has no
  `x-postgrest` primary-key column marker; `Schema.tablePrimaryKeyName` returns
  `Nothing` for them.
- `deleteRecord` builds its filter from a `Maybe` key:

```elm
deleteRecord { client, table, id } =
    let
        queryStr =
            PG.toQueryString
                (List.filterMap identity
                    [ Schema.tablePrimaryKeyName table
                        |> Maybe.map (\key -> PG.eq (PG.string id) |> PG.param key)
                    ]
                )
    in
    task { client = client, method = "DELETE", headers = [], path = "/" ++ table.name ++ "?" ++ queryStr, ... }
```

With `Nothing`, `queryStr` is empty → `DELETE /staff?` → deletes **every** row.

- `fetchRecord` has the same shape (select + `limit 1`, but no key filter), so it
  would return an arbitrary first row instead of the requested record.
- `saveRecord` *does* guard via `Maybe.map2 Tuple.pair (tablePrimaryKeyName table) id`
  — it returns early when the key is absent, so a save can't silently write
  without a filter; only the DELETE path is unsafe.

### Root cause
The library assumes every table exposes a usable primary key. `tablePrimaryKeyName`
returns `Maybe`, and the callers that `Maybe.map` it into a filter are written
with `List.filterMap identity` — which silently drops the filter when the key is
missing instead of refusing the operation. The UI separately relies on the key
being present to build "VIEW" links, so a missing key disables detail pages
entirely.

### Possible fix
Two changes: (a) refuse key-requiring operations when the key is missing, and
(b) only build "VIEW"/edit affordances when a key actually exists.

```elm
deleteRecord { client, table, id } =
    case Schema.tablePrimaryKeyName table of
        Just key ->
            task
                { client = client
                , method = "DELETE"
                , path = "/" ++ table.name ++ "?" ++ PG.toQueryString [ PG.eq (PG.string id) |> PG.param key ]
                , ...
                }
        Nothing ->
            Task.fail (RequestError "This table has no usable primary key; refusing to delete.")
```

And in `PageListing`/`PageDetail`, only emit VIEW / EDIT / DELETE controls when
`Schema.tablePrimaryKeyName table` is `Just` — so a missing-key table is
explicitly read-only instead of silently dangerous.

---
## Bug 8 — Tables with a composite primary key can only fetch by one half of the key

### Issue
Tables with a genuine composite primary key (film_cast, film_crew,
film_production_company) can only ever "VIEW"/fetch by **one** half of the key,
so different rows that share that one column collide onto the same detail page.

### Steps to reproduce (live-verified)
1. Open Film Cast from the sidebar.
2. Click "VIEW" on Harrison Ford's row → lands on `/film_cast/11`.
3. Click "VIEW" on Carrie Fisher's row → also lands on `/film_cast/11`
   (Mark Hamill / Luke Skywalker) — the wrong record.
4. The shared column here is `film_id` (11); `person_id` is never part of the
   lookup, so every cast member of film 11 maps to the same detail page.

### Supporting evidence
- Live: Harrison Ford and Carrie Fisher VIEW links both collide onto
  `/film_cast/11` (Mark Hamill/Luke Skywalker). Confirmed against the bluebox
  dataset (film 11 = a Star Wars title; person_id differs per cast member).
- The composite-PK detail fetch uses only one key column.

### Root cause
`Internal/Schema.elm`, `tablePrimaryKey`, keeps only the **first** primary-key
column, discarding the rest:

```elm
tablePrimaryKey { columns } =
    columns
        |> Dict.filter (\_ column -> column.primaryKey)
        |> Dict.toList
        |> List.head   -- <-- first PK column only
```

For film_cast the PK is `(film_id, person_id)`; this returns just `film_id`, so
`fetchRecord` filters `film_id = eq.11` and fetches whichever row PostgREST
returns first — the same "first row" for every cast member of that film.

### Possible fix
Use the full composite key for the detail lookup. The detail path must accept
all PK columns' values, not a single id. Concretely, keep all PK columns and
build an AND filter from them:

```elm
tablePrimaryKeyColumns table =
    table.columns
        |> Dict.filter (\_ column -> column.primaryKey)
        |> Dict.toList

-- detail fetch filter becomes:
PG.and
    (List.map
        (\(key, _) -> PG.param key (PG.eq (PG.string <| Dict.get key ids)))
        (tablePrimaryKeyColumns table)
    )
```

so `/film_cast?film_id=eq.11&person_id=eq.42` fetches the exact cast row. The
route/breadcrumb model also needs to carry the extra key column(s) rather than a
single `id`.

---
## Bug 9 — New/Edit form headings show a raw column name instead of a real title

### Issue
The heading of a New/Edit record form shows a raw column name — e.g. "title"
for Film, "email" for Staff — instead of a human title like "New Film".
`Internal/PageForm.elm` renders `Schema.label model.table |> Maybe.map Html.text`
into the `<h2>`, which is correct on a detail page (where a record is loaded and
`label` is used to fetch that record's *value*) but wrong on a form, where there
is no record value to fetch — so the raw column name leaks into the UI.

### Steps to reproduce
1. Open Film → NEW FILM.
2. The page heading reads literally "title".

### Root cause
`PageForm`'s view:

```elm
Html.h2 []
    [ Schema.label model.table
        |> Maybe.map Html.text
        |> Maybe.withDefault (Html.text "")
    ]
```

`Schema.label` returns the name of the column chosen as the identifying column
(e.g. `"title"` for Film). `PageDetail.elm` then fetches that column's *value*
for the heading; `PageForm` has no record to fetch from, so `label`'s raw
column name is shown as-is. Staff's New/Edit forms show "email" for the same
reason.

### Possible fix
On the form, render a literal "New <Table>" / "Edit <Table>" title from the
table name rather than the label column:

```elm
Html.h2 []
    [ Html.text
        (if model.isEdit then
            "Edit " ++ String.humanize model.table.name
         else
            "New " ++ String.humanize model.table.name
        )
    ]
```

(`String.humanize "film"` → "Film".) The detail page keeps its value-based
heading; only the form heading needs the change.

---
## Bug 10 — Record-label heuristic can't match multi-word columns (spaces vs underscores)

### Issue
The record-label heuristic (`identifiers` in `Internal/Schema.elm`) can never
match multi-word columns like `full_name`, `first_name`, `last_name` — the list
is written with spaces instead of underscores — so a later, worse match
(`email`) wins instead. Customer detail pages therefore show the customer's
email in the heading/breadcrumb rather than their full name.

### Steps to reproduce
1. Open Customer → VIEW on customer id 1717.
2. The heading/breadcrumb shows the email `juwanpowlowski@cremin.info`, not the
   full name `Dr. Deion Schiller`.

### Supporting evidence
- `Internal.Schema.label` returns the first entry in `identifiers` that is a
  real column on the table. The list is:

```elm
identifiers : List String
identifiers =
    [ "title"
    , "name"
    , "full name"     -- space-separated, can never match snake_case "full_name"
    , "email"
    , "first name"    -- can never match "first_name"
    , "last name"     -- can never match "last_name"
    , "city"
    , "country"
    ]
```

- `label` checks `Dict.member fieldName table.columns`; `"full name"` etc. are
  not real column names, so they never match, and `"email"` (a real Customer
  column) wins. This affects every table whose only good identifying column is
  a multi-word one, not just Customer.

### Root cause
The `identifiers` list uses the human-readable space-separated form, but the
`Dict.member` lookup compares against raw snake_case Postgres/PostgREST column
names. Space-separated entries are dead matches.

### Possible fix
Use snake_case in the identifiers list so the `Dict.member` lookup actually
matches:

```elm
identifiers : List String
identifiers =
    [ "title"
    , "name"
    , "full_name"    -- snake_case, matches real columns
    , "email"
    , "first_name"
    , "last_name"
    , "city"
    , "country"
    ]
```

The display can still humanize the matched column (`String.humanize` → "Full
name") at render time, but the lookup must use the column's real name.

---
## Bug 11 — CSV upload never validates rows client-side

### Issue
CSV upload never validates rows client-side before sending them to the server.
`hasErrors` is a stub that always returns `False`, so bad values (non-numeric
ids, wrong types, missing required columns) reach PostgREST raw — and even the
id-existence pre-check chokes on them with a raw Postgres error before any
preview is shown.

### Steps to reproduce
1. Open a resource (e.g. Store) → Upload CSV with the right headers but a
   non-numeric `store_id` in a row.
2. No client-side row error is flagged; the upload proceeds to the id-existence
   check, which fails with a raw Postgres error (`22P02 invalid input syntax
   for type bigint: "not_a_number"`) rather than a friendly preview listing the
   bad row.

### Root cause
`Internal/PageListing.elm` routes processed rows through `List.filter hasErrors`,
but `hasErrors` is a stub:

```elm
hasErrors _ =
    False
```

So the `[] -> UploadWithPreview ...` branch always wins (never
`UploadWithErrors`), and every row is treated as valid. The subsequent
`FetchingIds` step sends the raw rows to PostgREST, which rejects the bad one —
surfacing the constraint dump from bug #4 instead of a per-row validation
message. This is also the same gap behind the `22P02` scenario in
`database_constraints.feature`.

### Possible fix
Actually implement `hasErrors` against the CSV row's values using the table's
schema — check that required columns are present, that integer/float columns
parse, and that FK columns reference existing ids:

```elm
hasErrors : CsvRow -> Bool
hasErrors row =
    List.any
        (\col ->
            case Dict.get col.name row.values of
                Nothing -> col.required
                Just val -> not (parsesAs col.columnType val)
        )
        table.columns
```

With that in place, `List.filter hasErrors records` correctly splits valid vs
invalid rows, `UploadWithErrors` shows the per-row preview, and bad rows are
never posted to the server.

---
## Bug 12 — Enum filter defaults to "is one of" with nothing checked → filters to NULL

### Issue
A Postgres enum column's filter widget defaults to "is one of" with nothing
checked. Applying the filter in that state silently filters to `IS NULL` rows
instead of applying no filter or asking for a value.

### Steps to reproduce
1. Open Film → open the filter widget on the `rating` enum column.
2. Leave all enum choices unchecked (the default state).
3. Apply the filter → the listing shows only rows where `rating IS NULL`,
   hiding every rated film.

### Supporting evidence
- Source-only-confirmed (this scenario's live verification is blocked by the
  general `<select>`-dispatch limitation in this environment; the code path is
  unambiguous). Default filter state is created in `Internal/Filter.elm`:

```elm
fromColumn colName col =
    case col.columnType of
        StringCol ->
            let enumChoices = ... in
            if not (List.isEmpty enumChoices) then
                Just (oneOf colName enumChoices Set.empty)   -- nothing checked
            else
                Just (text colName equals "")
        ...
```

### Root cause
The default `OneOf` filter starts with an **empty** chosen set, and
`Operation.toPGQuery` maps an empty `OneOf` to `IS NULL`:

```elm
OneOf enum ->
    let chosen = Operand.chosen enum in
    if Set.isEmpty chosen then
        param PG.null          -- ?rating=is.null → only NULL rows
    else
        param <| PG.inList PG.string <| Set.toList chosen
```

So a user who opens the filter and applies it without picking any value gets a
"rating IS NULL" listing — the opposite of "no filter". `NoneOf` has a similar
footgun: an empty `NoneOf` renders `IN (all choices)` (everything except NULLs).

### Possible fix
Treat an empty enum filter as "no filter" rather than `IS NULL`. The cleanest
fix is to skip the filter entirely when `OneOf` has no chosen values:

```elm
OneOf enum ->
    let chosen = Operand.chosen enum in
    if Set.isEmpty chosen then
        Nothing   -- drop the filter from the query; don't emit IS NULL
    else
        Just (param <| PG.inList PG.string <| Set.toList chosen)
```

(`toPGQuery` already returns `Maybe PG.Param`; returning `Nothing` removes the
`is.null` clause.) Same for `NoneOf` when `chosen == choices`.

---
## Bug 13 — Float column's filter widget gets the CSS class "date" instead of "number"

### Issue
A float column's filter widget is rendered with the CSS class `date` instead of
`number` — a copy-paste slip. The widget still works as a number input, but it
carries the wrong styling hook (and any CSS/JS keyed on `filter.date` vs
`filter.number` mis-targets it).

### Steps to reproduce
1. Open a table with a float column, e.g. Film → open the search/filter UI.
2. Add a filter on a float column (Budget / Runtime / Popularity).
3. Inspect the filter element's class → `filter date` instead of `filter number`
   (compare with an integer column, which correctly gets `filter number`).

### Supporting evidence
- Source-only-confirmed. The bug is a straight copy-paste slip next to the
  correct `IntegerCol` branch.

### Root cause
`Internal/Search.elm`, `viewFilter`, tags the widget with the *kind* string, and
`FloatCol` was given `"date"`:

```elm
case column.columnType of
    ...
    IntegerCol ->
        intFilterInputs column.required name idx op
            |> inputs "number"

    FloatCol ->
        floatFilterInputs column.required name idx op
            |> inputs "date"   -- <-- copy-paste slip; should be "number"
    ...
```

`inputs kind` sets `class "filter", class kind`, so float filters render as
`class="filter date"`.

### Possible fix
One-word change:

```elm
FloatCol ->
    floatFilterInputs column.required name idx op
        |> inputs "number"
```

---
## Bug 14 — Column-sort controls have no accessible role or keyboard focus

### Issue
The listing's column-sort controls are plain `<span class="sort">` elements with
no `button`/`link` role and no `tabindex`. They do not appear in the
accessibility tree, cannot be reached with Tab, and have no exposed name or
role for assistive technology. They only work for sighted mouse users.

### Steps to reproduce
1. Open any listing (e.g. Customer).
2. Tab through the page — the column headers are never focused (they're spans).
3. A screen reader / a11y tree shows no sort control on the headers, despite
   the visual arrow.
4. (Mouse works: clicking the header text adds `?order=<column>.asc` and rows
   re-sort — confirmed live.)

### Root cause
`Internal/PageListing.elm`, `viewHeaderCell`, renders the clickable sort area as
a bare `span` with only `aria-sort` on it:

```elm
defaultHeader =
    Html.span
        [ Attrs.class "sort"
        , Attrs.attribute "aria-sort" "other"
        , Events.onClick <| Sort <| Asc name
        ]
        [ Html.text <| String.humanize name
        , Html.i [ Attrs.class "icono-play" ] []
        ]
```

A `span` has no implicit role, is not focusable, and `onClick` is mouse-only.
(`aria-sort` is also misplaced — it belongs on the `th`, not the inner span.)
Keyboard users and assistive tech get nothing.

### Possible fix
Make each header's sort area a real focusable control — a `<button>` inside the
`th`, with `aria-sort` moved to the `th`:

```elm
defaultHeader =
    Html.button
        [ Attrs.class "sort"
        , Attrs.type_ "button"
        , Events.onClick <| Sort <| Asc name
        , Attrs.attribute "aria-label" ("Sort by " ++ String.humanize name)
        ]
        [ Html.text <| String.humanize name
        , Html.i [ Attrs.class "icono-play" ] []
        ]

-- and in the <th>: Attrs.attribute "aria-sort" (ariaSortFor order name)
```

A `button` is natively focusable, activatable via Enter/Space, and exposed to
the accessibility tree — no `tabindex` or manual role wiring needed.

---
## Bug 15 — CSV "Wrong columns" message missing a space ("itshould")

### Issue
When a CSV upload has both missing columns *and* extra columns, the "Wrong
columns" error message reads `... , and itshould not contain the following
columns: ...` — the words "it" and "should" are glued together with no space,
because the fragments are joined with `", and it"` and the second fragment
starts with `"should"`.

### Steps to reproduce
1. Open a resource → Upload CSV with a wrong header set (one missing required
   column and one extra column).
2. Read the "Wrong columns" dialog → `The CSV file is missing the following
   columns: ... , and itshould not contain the following columns: ...`

### Root cause
`Internal/PageListing.elm` builds the sentence by `String.join ", and it"` over
two fragments, where the second fragment is `"should not contain ..."`:

```elm
[ if not (List.isEmpty missingColumns) then
    Just ("is missing the following columns: " ++ displayColumns missingColumns)
  else Nothing
, if not (List.isEmpty extraColumns) then
    Just ("should not contain the following columns: " ++ displayColumns extraColumns)
  else Nothing
]
|> String.join ", and it"
```

When both fragments exist, the join produces `...columns: X, and itshould not
contain...` — missing the space between "it" and "should". (Also a grammar
slip: "it" is a bare placeholder for a whole sentence.)

### Possible fix
Join with a trailing space, or restructure so the connector isn't glued to the
next fragment:

```elm
|> String.join ", and it "
-- yields "... , and it should not contain ..."
```

Better: keep a proper sentence separator and avoid concatenating a bare "it":

```elm
missingPart = "is missing the following columns: " ++ displayColumns missingColumns
extraPart   = "should not contain the following columns: " ++ displayColumns extraColumns
String.join ", and " [ missingPart, extraPart ] ++ "."
```

---
## Bug 16 — Deleting a referenced record shows a raw 23503 dump instead of a friendly message

### Issue
Confirming a delete on a record that other rows reference (FK RESTRICT) returns
a raw, untranslated Postgres error dump — `23503 / Key (film_id)=(1172675) is
still referenced from table "inventory"... inventory_film_id_fkey` — instead of
a friendly "This record is in use; N related rows prevent deletion."

### Steps to reproduce (live-verified)
1. Open Film "The Swan" (film_id 1172675) via the Inventory listing's linked
   cell → Film detail page.
2. Click DELETE → the confirmation modal lists 591 dependent Inventory rows.
3. Click DELETE (confirm) → the page shows the raw `23503` dump; the film still
   exists afterward (FK RESTRICT protected it).

### Supporting evidence
- Live: confirmed via a real DELETE click on film 1172675; the raw body
  `Key (film_id)=(1172675) is still referenced from table "inventory"` is shown
  verbatim.
- `Internal/PageDetail.elm` `Deleted (Err err)`:

```elm
Deleted (Err err) ->
    ( model, AppCmd.clientError err )
```

`clientError` → `Client.errorToString` renders the raw `code/details/hint/
message` body — the exact same untranslated path as bug #4's write errors.

### Root cause
This is the delete-flavored instance of **Bug 4's family**. The delete failure
is a 23503 FK violation, but `errorToString` has no SQLSTATE classifier, so it
dumps the raw PostgREST error body verbatim. No friendly message; no mention of
which rows block the delete or how to resolve it.

### Possible fix
The same `friendlyError` classifier from Bug 4, now keyed on `code = "23503"`:

```elm
friendlyError : Error -> String
friendlyError err =
    case err of
        BadStatus status metadata _ ->
            case metadata.code of
                "23503" ->
                    "This record is still referenced by other rows and can't be deleted. "
                        ++ "Remove or re-point those rows first."
                _ -> rawError err
        _ -> rawError err
```

With the classifier in `errorToString`, delete failures get a clear message
instead of the raw dump — and the confirmation modal can additionally list the
blocking rows (it already counts dependents, e.g. 591).

---
## Bug 17 — Creating a Person is impossible (sequence desync → duplicate key 23505)

### Issue
Every "New Person" save fails with `23505 ... Key (person_id)=(1) already
exists ... person_pkey`. The seed data inserted explicit `person_id` values
(TMDB ids 1–10) but never advanced the table's `person_id_seq`, so the
sequence is still at 1 and every insert collides with an already-seeded row.

### Steps to reproduce (live-verified)
1. Open Person → NEW PERSON.
2. Fill the required fields and click Save.
3. The save returns `23505 Key (person_id)=(1) already exists ... "person_pkey"`
   — the sequence's next value (1) collides with seeded person id 1.
4. Every attempt fails identically; a Person can never be created.

### Supporting evidence
- Live: every New Person save → `23505` duplicate-key dump (the raw
  `errorToString` path of bug #4, surfaced again on create).
- Seed data uses explicit TMDB `person_id` values 1..10; the identity sequence
  `person_id_seq` was never advanced (`last_value` still low), so
  `nextval('person_id_seq')` hands out values already present.
- Only Person is broken this way among the identity tables: rental/payment are
  empty (no id=1 collision), and `inventory_id` / `film_genre.genre_id` have no
  id=1 rows.

### Root cause
A data-seeding desync, not an app-code bug. Inserting explicit PK values into a
`GENERATED BY DEFAULT AS IDENTITY` / `serial` column does not bump the
underlying sequence. The app then relies on the server's default (`nextval`)
which reuses already-taken ids. Same practical symptom as bug #6's "no default"
tables, but a different root cause: here a default *exists* yet its sequence is
stale.

### Possible fix
Advance the sequence past the max seeded id (one-time seeding fix), or seed
without explicit ids so the sequence drives itself:

```sql
-- after seeding person with explicit ids 1..10:
SELECT setval('person_id_seq', max(person_id)) FROM person;
```

```sql
-- or seed without explicit PK so nextval assigns ids:
INSERT INTO person (full_name, ...) VALUES ('George Lucas', ...);
```

The app-side behavior is correct (it relies on the DB default); the fix is in
the seed data, not the Elm code.

---
