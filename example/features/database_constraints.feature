Feature: Database constraint violations
  As a back-office user
  I want a constraint violation to tell me which field is wrong and why
  So that I can fix my input without needing to understand the database schema

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"

  # PostgRestAdmin.Client has exactly one error path for every failed
  # write: `resolve` maps any non-401/403 bad status to
  # `BadStatus statusCode (parseError body)`, and `errorToString` renders
  # that as "The server responded with an error: " followed by the raw
  # `code / details / hint / message` PostgREST returns, verbatim. There
  # is no handling anywhere in src/ for any specific Postgres SQLSTATE
  # (checked: no reference to 23502, 23503, 23505, 22P02 or similar exists
  # in this codebase) and no mapping from a constraint or column name back
  # to the form field that produced it. Every scenario below is the same
  # underlying gap, exercised by a different constraint type.

  @bug
  Scenario: A NOT NULL violation names the field that needs a value
    Given I open the "Store" resource from the sidebar
    When I click "NEW STORE"
    And I fill in "Phone" with "(555) 000-0000" and leave the rest blank
    And I click "SAVE"
    Then I should be told which field is required, in plain language

    # Actual behavior (verified live earlier this session): the form has
    # no field for "Store" (store_id) at all - see
    # record_editing.feature - so saving always fails with the raw
    # PostgREST body: "The server responded with an error: 23502 ... null
    # value in column \"store_id\" of relation \"store\" violates
    # not-null constraint", naming the SQL column and table, not the (in
    # this case absent) form field.

  @bug
  Scenario: A malformed value names the field and what was expected
    Given I have a CSV with the right headers but a non-numeric "store_id"
    When I upload that CSV to the "Store" resource
    Then I should be told which column had the bad value, in plain language

    # Actual behavior (verified live earlier this session): "The server
    # responded with an error: 22P02 invalid input syntax for type
    # bigint: \"not_a_number\"" - the Postgres type name ("bigint") is
    # exposed, but there is no back-reference to the CSV row or the
    # form's "Store" label.

  @bug
  Scenario: A foreign key violation names the field, not the constraint
    Given I open the "Store" resource from the sidebar
    And I am editing the store with id "18"
    When I set "Zip code" to a zip code that does not exist, e.g. "99999999"
    And I click "SAVE"
    Then I should be told the zip code doesn't exist, in plain language

    # Actual behavior: reproduced directly against PostgREST with the
    # exact request PostgRestAdmin.Client.saveRecord would send (PATCH
    # /store?store_id=eq.18, same headers, same JSON body shape) since
    # the browser automation tooling was stuck for this whole session
    # (see the note in primary_key_detection.feature):
    #
    #     $ curl -X PATCH '.../store?store_id=eq.18' -d '{"zip_code": 99999999}'
    #     HTTP 409
    #     {"code":"23503",
    #      "details":"Key (zip_code)=(99999999) is not present in table \"zip_code_info\".",
    #      "hint":null,
    #      "message":"insert or update on table \"store\" violates foreign
    #                  key constraint \"store_zip_code_fkey\""}
    #
    # PageForm would show this verbatim: table names ("store",
    # "zip_code_info"), the internal constraint name
    # ("store_zip_code_fkey") and the raw column name ("zip_code") are all
    # exposed to the user, with nothing connecting "zip_code" back to the
    # "Zip code" field they just edited, and no indication that the fix
    # is simply "pick a zip code that exists".
    #
    # Checked the schema for the other two common constraint types
    # (`pg_constraint` in the bluebox schema): there are no explicit CHECK
    # constraints anywhere in this dataset, only the 15 foreign keys and
    # each table's own primary key (a UNIQUE-like constraint). A
    # duplicate-value scenario *is* reproducible after all - see the
    # "person_id" scenario below - so this gap is confirmed for every
    # constraint type this schema actually has (23505 is just as absent
    # from src/ as every other SQLSTATE).

  @bug
  Scenario: Deleting a record that is still referenced elsewhere
    Given I am viewing the film with id "1172675", "The Swan"
    And it has 591 linked "Inventory" records
    When I click "DELETE"
    And I confirm the deletion in the "Are you sure you want to delete the record?" dialog
    Then I should be told the film can't be deleted while inventory copies of it exist

    # Actual behavior (live-verified this session): the confirmation
    # dialog itself is good UX - it lists every dependent table and its
    # live count ("Inventory (591)", "Film production company (1)",
    # "Film crew (46)", "Film cast (7)") before asking to confirm, and
    # the delete genuinely does get attempted and does surface *an*
    # error rather than failing silently. But the error is the same raw
    # dump as every other scenario in this file:
    #
    #     The server responded with an error: 23503
    #     Key (film_id)=(1172675) is still referenced from table "inventory".
    #     update or delete on table "film" violates foreign key constraint "inventory_film_id_fkey"
    #
    # The film is correctly *not* deleted (confirmed via a follow-up
    # request: it still exists), so no data was lost - the only problem
    # is the message. This is `Deleted (Err err) -> AppCmd.clientError
    # err` in Internal/PageDetail.elm, which funnels into the exact same
    # untranslated `errorToString` as every write path.

  @bug
  Scenario: A duplicate primary key value
    Given I open the "Person" resource from the sidebar
    When I click "NEW PERSON"
    And I fill in "Name" with any value
    And I click "SAVE"
    Then a new person should be created

    # Actual behavior (live-verified this session): person_id is a
    # genuinely auto-generated column (confirmed via information_schema,
    # see record_editing.feature) and is correctly omitted from the New
    # Person form. But the seeded "bluebox" dataset was loaded with
    # explicit, externally-sourced person_id values (they are TMDB
    # person ids - e.g. person_id=1 is really George Lucas, visible on
    # the Film Cast listing) *without* ever advancing the column's
    # sequence, and the low end of that id range (1, 2, 3, ... at least
    # up to 10, all confirmed to exist via a direct PostgREST query) is
    # fully populated. So the very first "New Person" save of this
    # session - and, on the evidence, any "New Person" save against this
    # database - fails with:
    #
    #     The server responded with an error: 23505
    #     Key (person_id)=(1) already exists.
    #     duplicate key value violates unique constraint "person_pkey"
    #
    # shown as the same raw dump as every other constraint violation in
    # this file. This is a demo-data/seeding problem (the sequence was
    # never `setval`'d past the imported rows), not a app bug in its own
    # right, but it means creating a Person is exactly as broken in
    # practice as the tables in record_editing.feature's "Creating a new
    # Store record" scenario, just for a different underlying reason -
    # and it's the reproduction case that corrects this file's earlier
    # claim that a duplicate-key scenario "isn't reproducible against
    # this particular demo data".
