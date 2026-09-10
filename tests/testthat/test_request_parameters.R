test_that("normalize_httr2_config handles various input scenarios", {
  # Test default behavior
  default_config <- normalize_httr2_config()
  expect_equal(default_config$timeout, 30)
  expect_equal(default_config$retry$max_tries, 2)
  expect_false(default_config$verbose)
  
  # Test with partial user configuration
  custom_config <- normalize_httr2_config(list(timeout = 60))
  expect_equal(custom_config$timeout, 60)
  expect_equal(custom_config$retry$max_tries, 2)
  
  # Test with full custom configuration
  full_custom_config <- normalize_httr2_config(list(
    timeout = 45,
    retry = list(max_tries = 3, backoff = 2),
    verbose = TRUE
  ))
  expect_equal(full_custom_config$timeout, 45)
  expect_equal(full_custom_config$retry$max_tries, 3)
  expect_equal(full_custom_config$retry$backoff, 2)
  expect_true(full_custom_config$verbose)
  
  # Test error handling
  expect_error(normalize_httr2_config(NULL), NA)
  expect_error(normalize_httr2_config("not a list"), "httr2_config must be a list")
})

test_that("apply_httr2_config modifies request object correctly", {
  mock_req <- httr2::request("https://example.com")
  
  # Test with empty config
  result_empty <- apply_httr2_config(mock_req)
  expect_identical(result_empty, mock_req)
  
  # Test with timeout
  result_timeout <- apply_httr2_config(mock_req, list(timeout = 60))
  expect_s3_class(result_timeout, "httr2_request")
  
  # Test with retry
  result_retry <- apply_httr2_config(mock_req, list(retry = list(max_tries = 3)))
  expect_s3_class(result_retry, "httr2_request")
  
  # Test with verbose
  result_verbose <- apply_httr2_config(mock_req, list(verbose = TRUE))
  expect_s3_class(result_verbose, "httr2_request")
})

test_that("format_httr2_config generates correct output", {
  empty_format <- format_httr2_config()
  expect_equal(empty_format, "No httr2 request configuration was applied.")
  
  # Test with timeout
  timeout_config <- format_httr2_config(list(timeout = 45))
  expect_true(grepl("timeout: 45 seconds", timeout_config))
  
  # Test with retry
  retry_config <- format_httr2_config(list(retry = list(max_tries = 3, backoff = 2)))
  expect_true(grepl("retry: max_tries = 3, backoff = 2", retry_config))
  
  # Test with verbose
  verbose_config <- format_httr2_config(list(verbose = TRUE))
  expect_true(grepl("verbose: TRUE", verbose_config))
  
  # Test full configuration
  full_config <- format_httr2_config(list(
    timeout = 60,
    retry = list(max_tries = 3),
    verbose = TRUE
  ))
  expect_true(grepl("timeout: 60 seconds", full_config))
  expect_true(grepl("retry: max_tries = 3", full_config))
  expect_true(grepl("verbose: TRUE", full_config))
})
