Feature: CSV upload
  As a back-office user
  I want to preview a CSV before it is applied
  So that I can catch bad data before it reaches the database

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"
    And I open the "Store" resource from the sidebar

  Scenario: Uploading a CSV with the wrong columns is rejected before preview
    When I click "Upload CSV" and choose a file whose header row doesn't match Store's columns
    Then I should see "Wrong columns" listing the missing and/or extra columns
    And no data should be sent to the server

    # Verified working. Minor copy bug noticed along the way: the message
    # reads "...columns: **geog**, and itshould not contain..." - missing
    # space between "it" and "should" (Internal/PageListing.elm's
    # `badSchemaPreview` joins the two clauses with the literal string
    # ", and it" and the second clause's own text starts with "should",
    # with nothing to separate them).

  @bug
  Scenario: A row with invalid data is flagged before it reaches the server
    Given I have a CSV with the right headers but one row with an invalid value (a non-numeric "store_id")
    When I click "Upload CSV" and choose that file
    Then that row should be flagged in a "Validation failed" preview
    And I should be able to fix it before anything is uploaded

    # Actual behavior: no preview is shown at all. The row goes straight
    # to a network request and fails with a raw Postgres error dialog:
    # "The server responded with an error: 22P02 invalid input syntax for
    # type bigint: 'not_a_number'". This isn't even the upload itself -
    # it's the id-existence pre-check (PostgRestAdmin.Client.chunk) that
    # every non-empty-id CSV triggers before the preview, querying
    # `store_id=eq.not_a_number` to decide create-vs-update. Root cause:
    # Internal/PageListing.elm defines `hasErrors _ = False` - a stub that
    # always returns False no matter what row it's given. `CsvProcessed`
    # filters rows with `List.filter hasErrors records` to decide whether
    # to show the "Validation failed" state, so that branch is
    # unreachable dead code - nothing about a row is ever actually
    # checked before it reaches the server, at any step of the pipeline.
