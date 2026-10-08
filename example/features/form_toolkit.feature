Feature: Form-toolkit dependency bugs
  As a back-office user
  I want the generated forms to validate fields the way the database does
  So that required fields cannot be silently skipped and number inputs
    have a sane range

  # This file collects every bug whose root cause lives in the
  # `maca/form-toolkit` Elm dependency, not in this repo's own code.
  # The scenarios were moved here (from record_editing.feature) so the
  # repo-code bugs and the dependency bugs stay separated: see
  # FORM_TOOLKIT_BUG_DOSSIER.md for the full bug write-up.
  #
  # The dependency is used in Internal/PageForm.elm to render schema
  # fields:
  #   Schema.IntegerCol              -> Field.int attrs
  #   Schema.Timestamp/Time/DateCol  -> Field.datetime attrs
  # Both bugs below are defaults baked into that dependency.

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"

  @bug
  Scenario: A required date/time field blocks saving until it is filled in
    Given I am on the "Staff" listing
    And I click "NEW STAFF"
    And I fill in every other required field
    But I leave "Last update" (a required date/time field) empty
    Then the "SAVE" button should stay disabled
    And submitting should be rejected with a validation error

    # Actual behavior: FormToolkit marks the empty "Last update" field
    # `invalid="true"` and shows all-zero day/month/year/hour/minute
    # values, but the SAVE button still becomes enabled once the other
    # required fields are filled, and clicking it submits successfully.
    # The blank date is silently replaced with the current server
    # timestamp rather than the form being rejected or the field erroring
    # - so "required" is not actually enforced for this field type, and
    # what gets saved is not what the (empty) form showed.
    #
    # Root cause (maca/form-toolkit): Field.datetime receives
    # `Field.required column.required` in its attrs, but the datetime
    # widget does not participate in the form-validity gate - it marks
    # the field invalid without blocking submit, and an empty datetime
    # serializes to the current timestamp. See FORM_TOOLKIT_BUG_DOSSIER.md
    # Bug 1 for the full write-up.

  @bug
  Scenario: Numeric fields render with a valid input range
    Given I am on the "Staff" listing
    And I click "NEW STAFF"
    Then the "Staff" number field should accept any positive integer

    # Actual behavior: every plain number field (Address, Staff, Store,
    # Budget, Runtime, etc.) renders with `min="0" max="0"` whenever the
    # OpenAPI schema gives no explicit bounds. Typing a value like "5"
    # still works because nothing enforces the attribute live, but the
    # field is nonsensical per the HTML spec (a range that only allows
    # "0") and would misbehave with the native spinner buttons or any
    # stricter browser validation. This comes from the maca/form-toolkit
    # dependency's Field.int default, not this repo's own code.
    #
    # Root cause (maca/form-toolkit): Field.int initializes a number
    # field with min="0" max="0" and never clears them when no bounds are
    # passed, so an unconstrained integer column renders an impossible
    # range. See FORM_TOOLKIT_BUG_DOSSIER.md Bug 2 for the full write-up.
