# Tests for the bundled plumber2 example API service (inst/api_service/plumber.R).
# The annotated handlers are plain R functions, so they can be sourced into a
# dedicated environment and exercised directly without launching a server.

# Sources the example plumber2 file into a fresh environment
load_example_service <- function() {
  api_file <- system.file("api_service", "plumber.R", package = "ChatRBox")
  testthat::skip_if(api_file == "", "Example API service file not found")
  env <- new.env()
  sys.source(api_file, envir = env)
  env
}

# The client_fns endpoint must return R source code that defines client_plot
testthat::test_that("client_fns endpoint returns installable client source", {
  env <- load_example_service()
  testthat::expect_true(is.function(env$client_fns))
  
  code <- env$client_fns()
  testthat::expect_type(code, "character")
  
  client_env <- new.env()
  eval(parse(text = code), envir = client_env)
  testthat::expect_true(exists("client_plot", envir = client_env))
  testthat::expect_true(is.function(client_env$client_plot))
})

# The plot handler consumes plumber2 `body` parameters and returns a ggplot
testthat::test_that("plot handler returns a ggplot for valid body input", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("ggplot2")
  env <- load_example_service()
  
  df <- data.frame(X = 1:10, Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30))
  table_text <- jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE)
  
  # Columns inferred from the first two numeric columns
  p_auto <- env$plot(list(table_text = table_text, x_col = "", y_col = "", title = ""))
  testthat::expect_s3_class(p_auto, "ggplot")
  
  # Explicit column selection
  p_cols <- env$plot(list(table_text = table_text, x_col = "X", y_col = "Y", title = "t"))
  testthat::expect_s3_class(p_cols, "ggplot")
})

# Invalid input must be rejected via plumber2's abort_bad_request helper
testthat::test_that("plot handler rejects invalid body input", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("plumber2")
  env <- load_example_service()
  
  testthat::expect_error(
    env$plot(list(table_text = "", x_col = "", y_col = "", title = ""))
  )
  
  one_numeric <- jsonlite::toJSON(
    data.frame(a = letters[1:3], b = 1:3),
    dataframe = "rows", auto_unbox = TRUE
  )
  testthat::expect_error(
    env$plot(list(table_text = one_numeric, x_col = "", y_col = "", title = ""))
  )
  
  two_numeric <- jsonlite::toJSON(
    data.frame(X = 1:3, Y = 4:6),
    dataframe = "rows", auto_unbox = TRUE
  )
  testthat::expect_error(
    env$plot(list(table_text = two_numeric, x_col = "Z", y_col = "Y", title = ""))
  )
})
