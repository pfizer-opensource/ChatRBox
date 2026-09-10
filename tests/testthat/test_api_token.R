testthat::test_that("make_req returns httr2 request with basic input", {
  req <- make_req(base_url = "http://example.com", path = "foo")
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req errors if token is not character", {
  testthat::expect_error(
    make_req(base_url = "http://example.com", path = "foo", token = 123),
    "token must be a character vector or NULL"
  )
})

testthat::test_that("make_req basic structure and return", {
  req <- make_req(base_url = "http://example.com", path = "foo")
  testthat::expect_s3_class(req, "httr2_request")
  testthat::expect_equal(req$url, "http://example.com/foo")
  testthat::expect_equal(req$method, "GET")
})

testthat::test_that("make_req sets timeout for file download paths", {
  req <- make_req(base_url = "http://foo.com", path = "data.zip", timeout = NULL)
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req custom method and timeout for non-GET", {
  req <- make_req(base_url = "http://foo.com", path = "bar", method = "POST", timeout = NULL)
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req errors on bad token type", {
  expect_error(
    make_req(base_url = "http://foo.com", path = "bar", token = 555),
    "token must be a character vector or NULL"
  )
})

testthat::test_that("make_req supports Bearer Authorization", {
  req <- make_req(
    base_url = "http://foo.com",
    path = "abc",
    token = "abc123",
    auth_scheme = "bearer",
    token_header = "authorization"
  )
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req supports Key Authorization", {
  req <- make_req(
    base_url = "http://foo.com",
    path = "abc",
    token = "abc123",
    auth_scheme = "key",
    token_header = "authorization"
  )
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req supports custom Authorization", {
  req <- make_req(
    base_url = "http://foo.com",
    path = "abc",
    token = "abc123",
    auth_scheme = "custom",
    token_header = "authorization"
  )
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req supports non-Authorization custom header", {
  req <- make_req(
    base_url = "http://foo.com",
    path = "api",
    token = "abc123",
    token_header = "X-My-Token"
  )
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req supports NA/null/no token", {
  req <- make_req(
    base_url = "http://foo.com",
    path = "api",
    token = "",
    token_header = "X-My-Token"
  )
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("make_req sets verbose arg", {
  req <- make_req(
    base_url = "http://foo.com",
    path = "abc",
    verbose = TRUE
  )
  testthat::expect_s3_class(req, "httr2_request")
})

testthat::test_that("validate_name_match accepts matching names", {
  testthat::expect_silent(
    validate_name_match(
      name_list = list(foo = 1, bar = 2),
      main_list = list(foo = 10, bar = 20),
      arg_label = "token_service",
      target_label = "services_list"
    )
  )
})

testthat::test_that("validate_name_match errors on mismatched names", {
  testthat::expect_error(
    validate_name_match(
      name_list = list(foo = 1, badname = 2),
      main_list = list(foo = 10, bar = 20),
      arg_label = "token_service",
      target_label = "services_list"
    ),
    "The following names in 'token_service' do not match any entry in 'services_list'"
  )
})

testthat::test_that("check_names throws error when argument is not named", {
  testthat::expect_error(
    check_names(list(1, 2), "token_service", "services_list"),
    "All named entries in 'token_service' must match a name in 'services_list'"
  )
})

testthat::test_that("check_names accepts named arguments", {
  testthat::expect_silent(
    check_names(list(foo = 1, bar = 2), "token_service", "services_list")
  )
})

testthat::test_that("check_duplicate_names errors on duplicate names", {
  l <- list(a = 1, a = 2)
  names(l) <- c("foo", "foo") # Explicit duplicated names
  testthat::expect_error(
    check_duplicate_names(l, "token_service"),
    "Duplicate names detected in 'token_service'"
  )
})

testthat::test_that("check_duplicate_names passes for unique names", {
  l <- list(foo = 1, bar = 2)
  testthat::expect_silent(
    check_duplicate_names(l, "token_service")
  )
})
