Feature: Creating and editing records
  As a back-office user
  I want to create and edit records through the generated forms
  So that I can maintain the underlying data

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"

  Scenario: A Postgres enum column renders as a select with its real values
    Given I open the "Film" resource from the sidebar
    When I click "NEW FILM"
    Then the "Rating" field should be a dropdown
    And it should offer exactly "G", "PG", "PG-13", "R", "NC-17" and "NR"

  @bug
  Scenario: Editing a record persists the change and confirms it to the user
    Given I am viewing the store with id "18"
    When I click "EDIT"
    And I fill in "Phone" with "(555) 123-4567"
    And I click "SAVE"
    Then I should be returned to the store's detail page
    And I should see "Phone" as "(555) 123-4567"
    And I should not see an error message

    # Actual behavior: clicking SAVE keeps the user on the edit form and
    # shows a raw, unhandled error overlay: "Problem with the value at
    # json['store_id']: 18 - Expecting a STRING". The PATCH request itself
    # succeeds and the field IS persisted (confirmed by reloading the
    # detail page afterwards), but the response decoder expects store_id
    # to be a String while PostgREST returns it as a number - so the app
    # cannot tell the user the save worked, and it looks like it failed.
    #
    # Root cause (Internal/PageForm.elm, `Submitted`): the decoder used to
    # read the id back out of the save response is hardcoded as
    # `Decode.field pkName Decode.string`, regardless of the primary key's
    # actual column type. Every table in this schema (checked via the
    # OpenAPI document) has an integer primary key and no UUID/string keys
    # exist at all - so this is not a Store quirk, it breaks the "confirm
    # the save" step for editing literally any record in the demo.

  @bug
  Scenario: Creating a new Store record
    Given I am on the "Store" listing
    When I click "NEW STORE"
    And I fill in "Phone" with "(555) 000-0000"
    And I click "SAVE"
    Then a new store should be created
    And I should be shown its detail page

    # Actual behavior: the "New Store" form has no field for "Store"
    # (store_id), the table's primary key, and the database column has no
    # default/sequence. Saving always fails with the raw PostgREST error
    # "The server responded with an error: 23502 ... null value in column
    # \"store_id\" of relation \"store\" violates not-null constraint",
    # shown verbatim to the user instead of a friendly message. Creating a
    # Store through the UI is not currently possible.
    #
    # Root cause (Internal/PageForm.elm, `fieldFromColumn`): primary-key
    # columns are unconditionally dropped from every create/edit form
    # (`if column.primaryKey then Nothing`). This assumes the database
    # always generates the key (serial/identity), which does not hold for
    # tables with a manually-assigned primary key such as Store.
    #
    # Checked which of the 17 tables actually have a database-generated
    # key (via `information_schema.columns`: `column_default` /
    # `is_identity`): only inventory_id, payment_id, film_genre.genre_id,
    # person_id and rental_id are auto-generated. customer_id, film_id,
    # production_company_id, release_type_id, store_id and
    # zip_code_info.zip_code are plain manually-assigned integers with no
    # default and no identity - so "New Customer", "New Film", "New
    # Production Company", "New Release Type" and "New Zip Code Info" are
    # all broken exactly like "New Store", not just this one table.

  @bug
  Scenario: The New/Edit form heading names the record being edited
    Given I am on the "Film" listing
    When I click "NEW FILM"
    Then the page heading should read "New Film"

    # Actual behavior: the heading literally reads "title" - the raw name
    # of the column Internal.Schema.label picked as Film's identifying
    # column, rendered as-is instead of being used to look up a value.
    # Internal/PageForm.elm's view does
    # `Schema.label model.table |> Maybe.map Html.text`, which is correct
    # for Internal/PageDetail.elm (where a record is loaded and `label` is
    # used to fetch that record's *value*), but on a form there is no
    # record value to fetch, so the raw column name leaks into the UI.
    # Staff's New/Edit forms show "email" for the same reason.

  # The two form-toolkit dependency scenarios (required date/time field,
  # numeric min/max range) were moved to form_toolkit.feature to keep
  # repo-code bugs and dependency bugs separate.

  Scenario: A foreign key field rejects a value that isn't a real record
    Given I am on the "Inventory" listing
    When I click "NEW INVENTORY"
    And I fill in "Store" with "ZZZ nonexistent store"
    And I fill in "Film" with "ZZZ nonexistent film"
    Then both fields should be marked invalid with "This is not an allowed option"
    And the "SAVE" button should stay disabled
    When I fill in "Store" with a real store id and "Film" with a real film id
    Then both fields should become valid

    # Live-verified this session: "Store" and "Film" render as a plain
    # text `<input>` backed by a `<datalist>` (not a native `<select>`),
    # and typing a value that doesn't match a real record adds
    # `aria-invalid="true"` plus a `<p class="errors">This is not an
    # allowed option</p>` to the field, and keeps "SAVE" disabled -
    # exactly as a "strict autocomplete" FK field should behave. This is
    # a passing scenario, not a bug: worth recording since every other
    # FK-adjacent scenario in this suite (composite keys, the zip-code
    # foreign key violation in database_constraints.feature) is about a
    # gap, and this is the one place client-side FK validation actually
    # works as intended before a request is ever sent.
