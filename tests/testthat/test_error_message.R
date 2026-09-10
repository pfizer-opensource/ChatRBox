test_that("http_status_from_condition extracts status from various error structures", {
  
  # Test with direct status field
  mock_error2 <- list(status = 401)
  expect_equal(http_status_from_condition(mock_error2), 401)
  
  # Test with direct status_code field
  mock_error3 <- list(status_code = 500)
  expect_equal(http_status_from_condition(mock_error3), 500)
  
  # Test with nested status_code field
  mock_resp <- structure(list(status_code = 404), class = "response")
  mock_error1 <- list(resp = mock_resp)
  expect_equal(http_status_from_condition(mock_error1), 404)
  
  mock_error4 <- list(message = "Something went wrong with 403 status")
  expect_equal(http_status_from_condition(mock_error4), 403)
  
  # Test with invalid status
  mock_error5 <- list(status = 99)
  expect_true(is.na(http_status_from_condition(mock_error5)))
  
  # Test with invalid error object
  mock_error6 <- list()
  expect_true(is.na(http_status_from_condition(mock_error6)))
})

# All HTTP error codes represented (partly for script coverage)
test_that("http_status_guidance returns correct advice for different status codes", {
  expect_match(http_status_guidance(400), "HTTP 400 Bad Request")
  expect_match(http_status_guidance(401), "HTTP 401 Unauthorized")
  expect_match(http_status_guidance(403), "HTTP 403 Forbidden")
  expect_match(http_status_guidance(404), "HTTP 404 Not Found")
  expect_match(http_status_guidance(405), "HTTP 405 Method Not Allowed")
  expect_match(http_status_guidance(408), "HTTP 408 Request Timeout")
  expect_match(http_status_guidance(409), "HTTP 409 Conflict")
  expect_match(http_status_guidance(413), "HTTP 413 Payload Too Large")
  expect_match(http_status_guidance(414), "HTTP 414 URI Too Long")
  expect_match(http_status_guidance(415), "HTTP 415 Unsupported Media Type")
  expect_match(http_status_guidance(422), "HTTP 422 Unprocessable Content")
  expect_match(http_status_guidance(429), "HTTP 429 Too Many Requests")
  expect_match(http_status_guidance(500), "HTTP 500 Internal Server Error")
  expect_match(http_status_guidance(502), "HTTP 502 Bad Gateway")
  expect_match(http_status_guidance(503), "HTTP 503 Service Unavailable")
  expect_match(http_status_guidance(504), "HTTP 504 Gateway Timeout")
  
  # Test edge cases
  expect_equal(http_status_guidance(NA), "")
  expect_equal(http_status_guidance(999), "")
})

test_that("add_http_status_guidance combines error messages correctly", {
  mock_error <- list(status = 404)
  original_error <- "API endpoint not found"
  
  # Test message combination
  result <- add_http_status_guidance(mock_error, original_error)
  expect_true(grepl("API endpoint not found", result))
  expect_true(grepl("HTTP 404 Not Found", result))
  
  # Test with no guidance
  no_guidance_error <- list(status = 999)
  result_no_guidance <- add_http_status_guidance(no_guidance_error, original_error)
  expect_equal(result_no_guidance, original_error)
  
  # Test with NA status
  na_error <- list(status = NA)
  result_na <- add_http_status_guidance(na_error, original_error)
  expect_equal(result_na, original_error)
})

test_that("Error handling works for edge cases", {
  expect_no_error(http_status_from_condition(list()))
  expect_true(is.na(http_status_from_condition(list())))
  expect_equal(http_status_guidance(NA), "")
  expect_no_error(add_http_status_guidance(list(), "Test error"))
})
