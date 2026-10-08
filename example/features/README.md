# Smoke-test stories

Gherkin scenarios exercising the `example/` demo app (elm-watch dev server on
`:9000`, PostgREST on `:9080`, Postgres on `:5432`), driven end-to-end through
a real browser against the `bluebox` demo dataset.

Every scenario describes the **expected** behavior. A scenario tagged `@bug`
describes what *should* happen and would fail if run against the app today;
an `# Actual behavior: ...` comment at the bottom of the scenario records
what actually happens instead. Untagged scenarios were verified to pass as
written.

- `authentication.feature` - logging in, session persistence, silent
  auth failures
- `resource_listing.feature` - browsing listings and record detail pages,
  including the delete-confirmation dialog
- `record_editing.feature` - creating and editing records, schema-driven
  fields (enums, dates, numbers, foreign-key autocomplete)
- `form_toolkit.feature` - the `maca/form-toolkit` dependency bugs
  (required date/time not enforced, numeric `min="0" max="0"`), split
  out of record_editing.feature
- `primary_key_detection.feature` - what happens when PostgREST doesn't
  expose a table's primary key ("staff", "language", "holiday"), and
  when a table's key is composite (film_cast, film_crew,
  film_production_company)
- `csv_upload.feature` - the bulk CSV import flow and its validation
- `filtering.feature` - schema-driven filter widgets
- `database_constraints.feature` - what a NOT NULL, malformed-value,
  foreign-key or duplicate-key violation actually shows the user,
  whether the violation comes from a save or a delete

Credentials: `bluebox@example.com` / `password`, against API host
`http://localhost:9080`.

Bug write-ups live in two dossiers, split by root-cause owner:
`BUG_DOSSIER.md` covers bugs in this repo's own code; the two
form-toolkit dependency bugs are written up in
`FORM_TOOLKIT_BUG_DOSSIER.md` and exercised in `form_toolkit.feature`.

A handful of scenarios (marked in their own comments) are confirmed by
reading the source and the live database/OpenAPI schema rather than by
clicking through the UI. Two different sessions hit two different
tooling obstacles here: an earlier pass got the browser automation stuck
outright ("No snapshot found for page 1" on every click/fill, surviving
a full browser restart); a later pass got everything else working but
could not make a genuine native `<select>` element's value change reach
the Elm application - confirmed with several input methods on more than
one `<select>` (a filter's column picker, the listing pagination
dropdown), so it's a general limitation of that pass's environment, not
a sign of a bug in this app. Neither is a bug in this app; both are
reported here only so the affected scenarios' confirmation method is
clear. Every other scenario, including several that depend on delete
confirmations, foreign-key validation and a live composite-primary-key
collision, was live-verified by actually clicking through the UI.

## Bug summary

Details and root causes are in each scenario's comment.

1. JWT never persists to `sessionStorage`. Fully traced: `config.onLogin`
   (wired to the `loggedIn` port) is only ever called from
   PostgRestAdmin.elm's own `GotToken (Ok tokenStr)` handler - which only
   fires for the library's *own* built-in login form. The demo's custom
   root form logs in via `applyConfiguration (configure |> withJWT
   token)` instead, which sets the client's JWT directly and never
   touches `onLogin` at all. The mechanism works; the form actually used
   on "/" just never triggers it.
2. The default (non-"/") login form posts to a hardcoded `localhost:3000`
   instead of the configured host - `withHost` never touches
   `authFormUrl`; only the separate, uncalled `withLoginUrl` does.
3. Wrong credentials - or an invalid "Your own JWT" token - fail
   completely silently on the root login form: no message, nothing
   changes on screen, for either login mode. (The library's own login
   view already handles this correctly via `errorMessage`/
   `AuthFormStatus`; the demo's custom form just doesn't use it.)
4. **No database constraint violation is ever translated into a
   field-level message** - not NOT NULL, not a malformed value, not a
   foreign key. Every write error takes the same single code path
   (`errorToString`/`BadStatus`) straight to the user as a raw Postgres
   dump: table names, column names, SQLSTATEs and internal constraint
   names, verbatim, with nothing connecting them back to the form field
   that was actually wrong. See `database_constraints.feature` for three
   concrete instances of this one gap.
5. Saving any record whose primary key is numeric (i.e. almost every
   table) shows a false "Expecting a STRING" error after a successful
   save.
6. Creating a record is impossible on any table whose primary key has no
   database default or identity generation: Customer, Film, Store,
   Production Company, Release Type and Zip Code Info are all affected,
   confirmed via `information_schema` - not just Store.
7. Tables where PostgREST omits a "Primary Key" column description
   (Staff, Language, Holiday) get no "VIEW" links and can never be
   edited or confirmed-saved at all. Worse: `deleteRecord`, `saveRecord`
   and `fetchRecord` in `PostgRestAdmin/Client.elm` don't guard against a
   missing key either - `deleteRecord` would send a filter-less `DELETE`
   (i.e. delete every row) if it were ever reachable. It currently isn't
   (no "VIEW" link, so no path to a Delete button) - but that's an
   accident of the *other* bug, not a safety check.
8. Tables with a genuine composite primary key (film_cast, film_crew,
   film_production_company) can only ever "VIEW"/fetch by one half of
   the key, so different rows sharing that one column collide onto the
   same detail page.
9. New/Edit form headings show a raw column name ("email", "title")
   instead of a real title.
10. The record-label heuristic (`identifiers` in `Internal/Schema.elm`)
    can never match multi-word columns like `full_name`, because it's
    written with spaces instead of underscores.
11. CSV upload never validates rows client-side (`hasErrors` is a stub
    that always returns `False`) - bad values reach the server raw, and
    even the id-existence pre-check chokes on them with a raw Postgres
    error before any preview is shown.
12. An enum filter defaults to "is one of" with nothing checked; applying
    it in that state silently filters to `IS NULL` rows instead of
    applying no filter or asking for a value.
13. A float column's filter widget gets the CSS class `"date"` instead
    of `"number"` - a copy-paste slip.
14. Column-sort controls have no accessible role or keyboard focus.
15. Minor text bug: the CSV "Wrong columns" message reads "...and
    itshould not contain..." (missing space).
16. Deleting a record that's still referenced by another table (e.g. a
    Film with Inventory copies) does correctly ask for confirmation and
    does correctly refuse the delete server-side - but the refusal is
    shown via the exact same untranslated path as bug #4: a raw
    `23503`/constraint-name Postgres dump, not "this film still has 591
    inventory copies, remove those first."
17. Creating a Person is impossible in practice: the seeded `bluebox`
    dataset was loaded with explicit, externally-sourced `person_id`
    values (TMDB ids) starting at 1 without ever advancing the
    underlying sequence, so every "New Person" save collides on an
    already-used low id with a raw `23505 duplicate key` dump (again via
    the same untranslated path as #4). Unlike bug #6, `person_id` *does*
    have real database identity/auto-generation - this is a data-seeding
    problem, not a missing-default problem, but it produces the same
    practical result: the form can never be saved.
