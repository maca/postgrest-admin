Feature: Primary key detection
  As a back-office user
  I want every resource to be viewable, editable and creatable
  So that I can manage all the data exposed by the API, not just some of it

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"

  # Internal.Schema detects a table's primary key by checking whether a
  # column's OpenAPI `description` contains the text "Primary Key" - a
  # string PostgREST only emits when it can resolve a real PRIMARY KEY
  # constraint. For "staff", "language" and "holiday" none of their
  # columns carry that description at all, so tablePrimaryKeyName always
  # returns Nothing for these three tables. Every scenario below is a
  # direct consequence of that.

  @bug
  Scenario: A listing lets the user open any record
    Given I open the "Staff" resource from the sidebar
    Then every row should have a "VIEW" link

    # Actual behavior: the Staff listing (1 record) and the Holiday
    # listing (374 records) show no "VIEW" link, and no action column at
    # all, on any row. Internal.PageListing.viewActions derives the link
    # from `Schema.tablePrimaryKeyName table`; with no key detected it
    # returns Html.text "" for every row.

  @bug
  Scenario: Creating a record confirms success to the user
    Given I open the "Staff" resource from the sidebar
    When I click "NEW STAFF"
    And I fill in all required fields
    And I click "SAVE"
    Then I should be shown the new staff member's detail page
    And I should not see an error message

    # Actual behavior: the POST succeeds (verified: a new row with a real
    # staff_id was inserted into the database), but the app shows a raw
    # error dialog: "Problem with the given value: { ...full JSON body... }
    # Coudn't figure the field id" (typo included). Internal.PageForm's
    # `Submitted` case builds the save decoder as
    # `case Schema.tablePrimaryKeyName model.table of Just pkName -> ...
    # Nothing -> Decode.fail "Coudn't figure the field id"` - so for a
    # table with no detected key, the request always fires and always
    # "fails" from the user's point of view, indistinguishable from a
    # real server error.

  @bug
  Scenario: Viewing a record with a composite primary key shows that record
    Given I open the "Film Cast" resource from the sidebar
    And several film_cast rows share the same film_id (e.g. film_id 11 has
      separate rows for Luke Skywalker, Han Solo and Princess Leia
      Organa, each a different person_id)
    When I click "VIEW" on the "Han Solo" row
    Then I should see the Han Solo cast record, not a different castmate

    # Actual behavior: live-verified this session by clicking through the
    # UI. On the Film Cast listing sorted by the "Film" column
    # (`?order=film_id.asc`), every row for film_id 11 (Star Wars) - Mark
    # Hamill/Luke Skywalker, Harrison Ford/Han Solo, Carrie
    # Fisher/Princess Leia, and others - was clicked via its own "VIEW"
    # link. Clicking VIEW on the Harrison Ford row landed on
    # "/film_cast/11" showing Person "Mark Hamill - 2" / Film character
    # "Luke Skywalker", not Harrison Ford/Han Solo. Clicking VIEW on the
    # Carrie Fisher row landed on the exact same "/film_cast/11" showing
    # the same Mark Hamill/Luke Skywalker record. Every row sharing that
    # film_id collides onto the identical detail page, confirmed with two
    # independent rows. This is also confirmed via the OpenAPI schema and
    # `information_schema`: film_cast's primary key is
    # the pair (film_id, person_id); both columns are individually
    # flagged "Primary Key" in their column descriptions, so
    # Internal/PageForm.elm correctly excludes both from the create/edit
    # form. But `Internal.Schema.tablePrimaryKeyName` - used to build
    # every "VIEW" link and to fetch a single record for PageDetail -
    # picks only ONE column: `columns |> Dict.filter (.primaryKey) |>
    # Dict.toList |> List.head`. Elm's Dict iterates in key-sorted order,
    # so "film_id" (alphabetically before "person_id") always wins. Every
    # row sharing a film_id therefore gets the identical "VIEW" href (e.g.
    # `/film_cast/11`), and PageDetail's fetch (`film_id=eq.11&limit=1`)
    # returns whichever one row Postgres/PostgREST happens to return
    # first - not the specific castmate the user clicked. This affects
    # every table with a genuine composite key: film_cast, film_crew,
    # film_production_company.

  @bug
  Scenario: Deleting a record only ever deletes that one record
    Given a table has no detected primary key (Staff, Language, Holiday)
    When a "DELETE" action is somehow triggered for one record of that table
    Then only that record should be removed

    # NOT executed against the running demo database - this is a
    # read-only finding from PostgRestAdmin/Client.elm plus the
    # PostgREST config, deliberately not reproduced live because doing so
    # would destroy real demo data with no confirmation step in between.
    #
    # `deleteRecord` builds its query as:
    #     Schema.tablePrimaryKeyName table
    #         |> Maybe.map (\key -> PG.eq (PG.string id) |> PG.param key)
    #     |> List.filterMap identity  -- i.e. [] when the key is Nothing
    #     |> PG.toQueryString
    # When no primary key is detected, this produces an EMPTY query
    # string, so the request becomes `DELETE /staff?` - no WHERE clause
    # at all. This demo's postgrest.conf sets no `db-*` safety options,
    # and PostgREST does not require a filter on DELETE by default, so
    # this would delete every row in the table. `saveRecord` has the same
    # gap for edits (falls back to POST instead of PATCH, silently
    # creating a duplicate instead of updating), and `fetchRecord` drops
    # its id filter the same way (returns an arbitrary row instead of the
    # one requested).
    #
    # This is currently unreachable through the UI: tables with no
    # detected primary key also get no "VIEW" link (see the first
    # scenario in this file) and creating a record never navigates
    # anywhere on success (see the second), so there is no path from the
    # listing page to a detail page where "DELETE" or "EDIT" could be
    # clicked. But it means fixing *only* the "no VIEW link" symptom -
    # without also fixing `deleteRecord`/`saveRecord`/`fetchRecord` to
    # handle a missing key - would turn a merely-annoying bug into a
    # data-loss one. Worth knowing about before anyone reaches for the
    # obvious-looking fix.
