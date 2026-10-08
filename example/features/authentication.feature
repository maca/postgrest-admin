Feature: Authentication
  As a back-office user
  I want to log in to the PostgREST Admin demo
  So that I can manage the underlying database through the UI

  Scenario: Logging in with valid credentials from the home page
    Given I am on "http://localhost:9000/"
    When I fill in "PostgREST API URL" with "http://localhost:9080"
    And I fill in "Authentication endpoint URL" with "http://localhost:9080/rpc/login"
    And I fill in "Login" with "bluebox@example.com"
    And I fill in "Password" with "password"
    And I click "LOGIN"
    Then I should be redirected to "http://localhost:9000/customer"
    And I should see the resource sidebar, including "Customer", "Film" and "Store"
    And I should see a "LOGOUT" button

  @bug
  Scenario: Wrong credentials are reported to the user
    Given I am on "http://localhost:9000/"
    When I fill in "PostgREST API URL" with "http://localhost:9080"
    And I fill in "Authentication endpoint URL" with "http://localhost:9080/rpc/login"
    And I fill in "Login" with "bluebox@example.com"
    And I fill in "Password" with "wrong-password"
    And I click "LOGIN"
    Then I should see an error explaining that the login failed

    # Actual behavior (live-verified this session, screenshot on file):
    # nothing happens. The POST to /rpc/login correctly
    # returns 401, but Main.elm's `GotToken (Err _) -> ( model, Cmd.none )`
    # discards the error outright - no message, no shake, no red text,
    # nothing changes on screen. A user who mistypes their password has no
    # way to tell the click even registered. The "Your own JWT" login mode
    # has the same problem one level up: submitting a garbage/expired
    # token calls `PostgRestAdmin.applyConfiguration`, which dispatches a
    # schema-fetch request that also comes back 401 - and that failure
    # (`SchemaFetched (Err err) -> ( { model | client = Client.logout
    # model.client, authFormStatus = Failure err }, Cmd.none )`) is
    # tracked in the *library's* model, but Main.elm's custom view (shown
    # on "/") never reads `authFormStatus` at all, so it's just as
    # invisible. Both login paths on the root form fail completely
    # silently.
    #
    # This is squarely a demo bug, not a library one: PostgRestAdmin.elm's
    # *own* default login view already does this correctly. It carries an
    # `errorMessage : AuthFormStatus -> Html Msg` that renders a friendly
    # message per error type ("You may have entered the wrong password,
    # please try again." for Forbidden, "Please sign in to continue." for
    # Unauthorized, etc.) right above the form. Main.elm's custom form on
    # "/" just never wires up the equivalent - it has its own separate,
    # error-blind `AuthFormSubmitted`/`GotToken` handling instead of
    # reusing the library's.

  @bug
  Scenario: Session survives a full page reload
    Given I have logged in as described above
    When I reload the page
    Then I should still be logged in
    And I should see the resource listing, not a login form

    # Actual behavior: static/index.html correctly subscribes to
    # `app.ports.loggedIn` and calls `sessionStorage.setItem("jwt", jwt)`,
    # but sessionStorage stays empty after a successful login - the port
    # never fires with a token. Any reload therefore drops the user back to
    # a login form and loses all app state.
    #
    # Root cause, fully traced: `config.onLogin` (the callback Main.elm
    # wires to the `loggedIn` port) is only ever invoked from ONE place in
    # the whole codebase - PostgRestAdmin.elm's own `update`, in
    # `GotToken (Ok tokenStr) -> ( ..., Cmd.batch [ config.onLogin
    # tokenStr |> Cmd.map (always NoOp), Client.fetchSchema ... ] )`. That
    # `GotToken` is the result of PostgRestAdmin's *own* built-in login
    # form submitting via `requestToken` - the form shown on any
    # non-"/" path (see the previous scenario).
    #
    # The demo's custom root form never goes through that code path at
    # all. Its `AuthFormSubmitted` handles both login modes itself and,
    # on success, calls `PostgRestAdmin.applyConfiguration (PostgRestAdmin.configure
    # |> PostgRestAdmin.withJWT token |> PostgRestAdmin.withHost apiHost) model.adminAppModel`.
    # `withJWT` only appends a `clientAttributes` transformer
    # (`\attrs -> { attrs | authScheme = Client.jwt tokenStr }`), and
    # `applyConfiguration` folds that into `model.client` directly - it
    # never touches `config.onLogin` or dispatches `GotToken` in any way.
    # So logging in through the form actually used on "/" can never make
    # it to sessionStorage, no matter how many times you do it. The
    # mechanism itself works (it's exactly what the library's own form
    # exercises) - the demo's custom form just never triggers it.

  @bug
  Scenario: Logging back in after being signed out of a deep link
    Given I am not logged in
    When I navigate directly to "http://localhost:9000/customer"
    Then I should see a login form
    When I fill in "Login" with "bluebox@example.com"
    And I fill in "Password" with "password"
    And I click "LOGIN"
    Then I should be logged in and see the Customer listing

    # Actual behavior: navigating to any path other than "/" while logged
    # out renders PostgRestAdmin's own default login form (Login/Password
    # only - no host fields), instead of the demo's custom form used on
    # "/". That default form POSTs to the hardcoded fallback host
    # "http://localhost:3000/rpc/login" instead of the configured
    # "http://localhost:9080/rpc/login", so the request fails with
    # net::ERR_CONNECTION_REFUSED and login is impossible from this view.
    # The only way back in is to navigate to "/" first.
    #
    # Also live-verified this session via the "LOGOUT" button: clicking
    # it while on any resource page (e.g. "/film") reproduces the exact
    # same broken state without ever navigating anywhere - `LoggedOut`
    # only resets `model.client`, it never touches the URL path, so the
    # same non-"/" branch of Main.elm's view (and the same hardcoded
    # `localhost:3000` login form) is shown immediately, with no link
    # back to "/" anywhere on the page. The only way out is to manually
    # type "http://localhost:9000/" in the address bar.
    #
    # Root cause (src/PostgRestAdmin.elm): `model.authFormUrl` - the URL
    # this built-in login form posts to - is set once at init to
    # `{ defaultHost | path = "/rpc/login" }` (defaultHost is hardcoded to
    # localhost:3000) and is only ever changed by the separate
    # `withLoginUrl` config function. `withHost`, which the demo does
    # call, only updates `clientAttributes.host` (used for ordinary API
    # requests) - it does not touch `authFormUrl` at all. Since the demo
    # never calls `withLoginUrl`, the library's own login form silently
    # keeps posting to the library's example host forever. This is as
    # much a library footgun as an app bug: a consumer who sets `withHost`
    # would reasonably expect the framework's own login form to follow
    # it, especially given PostgREST's `/rpc/login`-on-the-same-host
    # convention.
