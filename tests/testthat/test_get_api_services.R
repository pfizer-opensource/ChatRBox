# Tests with mock API endpoint
test_that("get_api_client_fns works with a local mock API", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/client_fns", function(req, res) {
    res$
      set_type("text/plain")$
      send("foo <- function(x) x + 1")
  })
  server <- webfakes::new_app_process(app)
  env <- new.env()
  get_api_client_fns(
    base_url = server$url(),
    client_envir = env
  )
  expect_true(exists("foo", envir = env))
  expect_equal(env$foo(2), 3)
  server$stop()
})

testthat::test_that("get_api_client_env works with a local mock API endpoint", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/client_fns", function(req, res) {
    res$
      set_type("text/plain")$
      send("foo <- function(x) x + 1")
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop())
  
  url <- server$url()
  services <- list(mock = url)
  
  out <- get_api_client_env(
    services_list = services,
    token_service = NULL,
    header_service = NULL
  )
  
  testthat::expect_true(is.list(out))
  testthat::expect_equal(names(out), "mock")
  testthat::expect_true(is.environment(out$mock))
  testthat::expect_true(exists("foo", envir = out$mock))
  testthat::expect_equal(out$mock$foo(41), 42)
})

# Errors due to missing URL arg
test_that("fails if no base_url is provided", {
  testthat::expect_error(
    get_api_client_fns(base_url = NULL),
    "base_url must be provided."
  )
})

# JSON content type httr 2 request with no errors
test_that("handles JSON content type", {
  skip_if_not_installed("mockery")
  mockery::stub(
    get_api_client_fns,
    "httr2::req_perform",
    structure(list(), class = "httr2_response")
  )
  mockery::stub(
    get_api_client_fns,
    "httr2::resp_is_error",
    FALSE
  )
  mockery::stub(
    get_api_client_fns,
    "httr2::resp_content_type",
    "application/json"
  )
  mockery::stub(
    get_api_client_fns,
    "httr2::resp_body_json",
    list(code = "bar <- function() 24")
  )
  
  client_env <- get_api_client_fns(base_url = "http://example.com")
  testthat::expect_true("bar" %in% ls(client_env))
  testthat::expect_equal(client_env$bar(), 24)
})

# Mock API failure types
test_that("errors if API request fails", {
  skip_if_not_installed("mockery")
  mock_response <- structure(list(), class = "httr2_response")
  
  mockery::stub(
    get_api_client_fns,
    "httr2::req_perform",
    mock_response
  )
  mockery::stub(
    get_api_client_fns,
    "httr2::resp_is_error",
    TRUE
  )
  mockery::stub(
    get_api_client_fns,
    "httr2::resp_status_desc",
    "404 Not Found"
  )
  
  testthat::expect_error(
    get_api_client_fns(base_url = "http://example.com"),
    "API request failed"
  )
})

# Errors due to invalid API token type (should be character)
test_that("errors for invalid API token", {
  testthat::expect_error(
    get_api_client_fns(base_url = "http://example.com", token = 1234),
    "character vector or NULL" 
  )
})

testthat::test_that("Empty list returned for empty services", {
  res <- get_api_client_env(list())
  testthat::expect_equal(res, list())
})

testthat::test_that("Error for unnamed serves_list", {
  expect_error(
    get_api_client_env(list("http://example.com/api")),
    "services_list must be a named list/vector"
  )
})

testthat::test_that("Error for mismatched token names", {
  testthat::expect_error(
    get_api_client_env(
      services_list = list(service = "http://api.test"),
      token_service = list(mismatch = "token")
    ),
    "The following names in 'token_service' do not match any entry in 'services_list'"
  )
})

testthat::test_that("Error for duplicated token names", {
  expect_error(
    get_api_client_env(
      services_list = list(service = "http://api.test"),
      token_service = list(service = "token1", service = "token2")
    ),
    "Each item must have a unique name"
  )
})

testthat::test_that("Error for duplicated header names", {
  dup_headers <- list(service = "Header1", service = "Header2")
  expect_error(
    get_api_client_env(
      services_list = list(service = "http://api.test"),
      header_service = dup_headers
    ),
    "Each item must have a unique name"
  )
})
