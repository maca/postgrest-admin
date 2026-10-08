Feature: Resource listing and detail views
  As a back-office user
  I want to browse and inspect records from any PostgREST resource
  So that I can find the data I need to manage

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"

  Scenario: Browsing a resource listing
    When I open the "Store" resource from the sidebar
    Then I should see "196 records"
    And I should see a paginated table of stores with columns "Store", "Zip code" and "Geog"
    And each row should have a "VIEW" link

  Scenario: Viewing a record's detail page
    Given I am on the "Store" listing
    When I click "VIEW" on the store with id "18"
    Then I should see the store's field values, including "Zip code", "Geog", "Phone", "Road ref" and "Street name"
    And I should see "EDIT" and "DELETE" actions
    And I should see related-record links with live counts, such as "Inventory (1941)" and "Customer (1032)"

  @bug
  Scenario: A record's detail heading uses its most meaningful identifying column
    Given I am on the "Customer" listing
    When I click "VIEW" on the customer with id "1717"
    Then the page heading and breadcrumb should show the customer's full name, "Dr. Deion Schiller"

    # Actual behavior: the heading and breadcrumb instead show the
    # customer's email, "juwanpowlowski@cremin.info". Internal.Schema.label
    # picks the first matching column from a hardcoded `identifiers` list
    # (["title", "name", "full name", "email", "first name", "last name",
    # "city", "country"]) that exists on the table. That list uses
    # space-separated words ("full name", "first name", "last name"), but
    # real Postgres/PostgREST columns are snake_case ("full_name",
    # "first_name", "last_name") - so those three entries can never match
    # any real column, and a later, worse match ("email") wins instead.
    # This affects every table whose only good identifying column is a
    # multi-word one, not just Customer.

  @bug
  Scenario: Sorting a listing by clicking a column header
    Given I am on the "Customer" listing
    When I activate the sort control on the "Customer" column header
    Then the listing should re-render sorted by that column
    And the sort control should be reachable and operable via the keyboard

    # Actual behavior: the sort control is a plain `<span class="sort">`
    # with no button/link role and no tabindex. It does not appear at all
    # in the accessibility tree, so it can't be reached with Tab and has no
    # exposed name or role for assistive technology - it only works for
    # sighted mouse users. (Clicking it does work correctly for a mouse
    # user: the URL gains `?order=<column>.asc` and rows re-sort.)

  Scenario: Deleting a record asks for confirmation and shows what depends on it
    Given I am viewing a film that has linked inventory, cast and crew records
    When I click "DELETE"
    Then I should see a confirmation dialog asking "Are you sure you want to delete the record?"
    And it should warn "This action cannot be undone"
    And it should list each dependent table with its live record count, such as
      "Inventory (591)", "Film crew (46)" and "Film cast (7)"
    And clicking "CANCEL" should close the dialog without deleting anything

    # Live-verified this session (film id 1172675, "The Swan"): clicking
    # the page's "DELETE" button does not delete immediately - it opens a
    # modal built from `confirmDelete`/`DeleteModalOpened` in
    # Internal/PageDetail.elm, which lists the same related-record links
    # and counts shown on the detail page itself, then requires a second
    # click on the modal's own "Delete" button (`DeleteConfirmed`) before
    # anything is actually sent to the server. This is good UX and works
    # exactly as it should; see database_constraints.feature for what
    # happens next when the delete itself is rejected by the database.
