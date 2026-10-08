Feature: Schema-driven filtering
  As a back-office user
  I want the filter UI to match each column's actual type
  So that I get the right operators and the right styling for what I'm filtering

  Background:
    Given I am on "http://localhost:9000/"
    And I log in with API URL "http://localhost:9080", login "bluebox@example.com" and password "password"

  @bug
  Scenario: A number filter is styled as a number filter
    Given I open the "Film" resource from the sidebar
    And I add a filter on "Popularity" (a float column)
    Then the filter row should carry a "number" styling class, like the
      "Budget" and "Runtime" integer filters do

    # Actual behavior: confirmed by reading Internal/Search.elm's
    # `viewFilter`. Attempted live this session but blocked by a specific
    # automation limitation, distinct from the earlier session's stuck
    # tooling: changing the filter's column `<select>` from its default
    # ("Budget") to "Popularity" via every method tried - agent-browser's
    # `select` command, focus+arrow-keys+Enter, and genuine keyboard
    # type-ahead - visibly changes the `<select>` element's DOM value
    # (confirmed via a direct value read) but never reaches Elm: the
    # wrapping `.filter` div's class stayed "filter number" (Budget's)
    # instead of switching to "filter date" (Popularity's), and "Apply
    # Filters" kept sending `budget=eq.<value>` regardless. A sanity
    # check on an unrelated native `<select>` (the listing's pagination
    # dropdown, "Page 1" -> "Page 2") showed the same failure - the URL
    # never gained the expected page parameter - so this is a general
    # native-`<select>`-dispatch problem with headless Chrome in this
    # environment, not something specific to this filter or a sign the
    # bug itself is wrong. The source-level confirmation below stands.
    # Every other column type passes a
    # matching class to its filter wrapper (`inputs "number"` for
    # IntegerCol, `inputs "bool"` for BoolCol, `inputs "time"` for
    # date/time columns, `inputs "enum"` / `inputs "text"` for StringCol),
    # but FloatCol is wired to `inputs "date"`:
    #
    #     FloatCol ->
    #         floatFilterInputs column.required name idx op
    #             |> inputs "date"
    #
    # This looks like a copy-paste slip from the DateCol case right above
    # it. The filter itself still works (correct operators: equals, lte,
    # gte, lt, gt, between), but every float column's filter row is
    # styled with whatever CSS applies to ".filter.date" instead of
    # ".filter.number" - affecting Film's Popularity, Vote average, and
    # any other float column in the schema.

  @bug
  Scenario: An enum filter with nothing checked does not filter to NULLs
    Given I open the "Film" resource from the sidebar
    When I click "Add filter"
    And I leave the default "Rating" / "is one of" filter with no checkboxes ticked
    And I click "Apply Filters"
    Then the listing should be unaffected (or ask me to pick a value),
      not silently narrow to films with no rating at all

    # Actual behavior: confirmed by reading Internal/Filter/Operation.elm.
    # Not reproduced live this session either: the Film listing's "Add
    # filter" defaults to the alphabetically-first column ("Budget", a
    # float, not "Rating"), so reaching an enum filter at all requires
    # changing the same column `<select>` blocked in the previous
    # scenario - see that scenario's note for the concrete evidence that
    # this is a general automation limitation, not a sign the bug itself
    # is wrong. Adding a filter on a Postgres-enum
    # column (Internal/Filter.elm's `fromColumn`) always starts it as
    # `OneOf (Operand.enum choices Set.empty)` - "is one of" with nothing
    # checked. `Operation.toPGQuery` handles that empty-selection case as:
    #
    #     OneOf enum ->
    #         if Set.isEmpty chosen then
    #             param PG.null
    #         else
    #             param <| PG.inList PG.string <| Set.toList chosen
    #
    # So clicking "Add filter" then immediately "Apply Filters" - with no
    # other interaction at all - silently sends `rating=is.null`, showing
    # only the (likely few or zero) rows where the enum column has no
    # value. Nothing in the UI signals that this is what "is one of" with
    # no boxes checked means; a user would reasonably expect either "no
    # filter applied" or a validation nudge to pick at least one value.
