# Tests starting API service
testthat::test_that("start_api_service works with NULL port", {
  proc <- start_api_service(port = NULL, host = "127.0.0.1")
  testthat::expect_s3_class(proc, "r_process")
  proc$kill()
})

# Tests starting default API service
testthat::test_that("start_api_service works with defaults", {
  proc <- start_api_service()
  testthat::expect_s3_class(proc, "r_process")
  proc$kill()
})

# Tests with custom port - should return S3 class
testthat::test_that("start_api_service works with custom port", {
  proc <- start_api_service(port = 8080, host = "127.0.0.1")
  testthat::expect_s3_class(proc, "r_process")
  proc$kill()
})

# Errors due to missing API file
testthat::test_that("API file not found", {
  skip_if_not_installed("mockery")
  mockery::stub(start_api_service, 'system.file', "")
  
  testthat::expect_error(
    start_api_service(),
    regexp = "Local API not found"
  )
})

# Errors due to invalid config file path
testthat::expect_error(store_keyring(config_path = NULL, 
                                     service = "ChatRBox"),
                       regexp = "invalid 'path'")

# Errors due to non-existent config file path
testthat::expect_error(store_keyring(config_path = "example_config_path.yml", 
                                     service = "ChatRBox"),
                       regexp = "file")

# Errors due to missing required argument
testthat::expect_error(
  get_keyring(var = NULL, 
              service = "ChatRBox"),
  regexp = "you must provide")

# Tests storing and retrieving secrets with no errors
# Suppresses expected warning for keyring 'env' backend
testthat::test_that("retrieves stored variable", {
  temp_config <- tempfile(fileext = ".yml")
  writeLines("RETRIEVE_TEST: my_secret", temp_config)
  suppressWarnings(store_keyring(config_path = temp_config, service = "ChatRBox"))
  
  value <- suppressWarnings(get_keyring(var = "RETRIEVE_TEST", service = "ChatRBox"))
  testthat::expect_equal(value, "my_secret")
  
  unlink(temp_config)
})
