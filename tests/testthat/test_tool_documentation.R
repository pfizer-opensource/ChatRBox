testthat::test_that("build_tool_docs returns empty for invalid tools_env", {
  testthat::expect_identical(build_tool_docs(NULL), "")
  testthat::expect_identical(build_tool_docs("not_an_env"), "")
  testthat::expect_identical(build_tool_docs(new.env()), "")
})

testthat::test_that("build_tool_docs falls back to arg signature for bare fns", {
  tools_env <- new.env()
  assign("add_two_numbers", function(x, y) x + y, envir = tools_env)
  docs <- build_tool_docs(tools_env)
  testthat::expect_true(nzchar(docs))
  testthat::expect_match(docs, "add_two_numbers", fixed = TRUE)
  testthat::expect_match(docs, "Arguments: x, y", fixed = TRUE)
})

testthat::test_that("build_tool_docs escapes braces for glue safety", {
  tools_env <- new.env()
  assign("standard_deviation", stats::sd, envir = tools_env)
  docs <- build_tool_docs(tools_env)
  testthat::expect_no_error(glue::glue("Tools:\n{docs}"))
})

testthat::test_that("include_examples retains the examples section", {
  tools_env <- new.env()
  assign("standard_deviation", stats::sd, envir = tools_env)
  without <- build_tool_docs(tools_env, include_examples = FALSE)
  with    <- build_tool_docs(tools_env, include_examples = TRUE)
  testthat::expect_true(nchar(with) >= nchar(without))
  testthat::expect_no_error(glue::glue("Tools:\n{with}"))
})

testthat::test_that("build_tool_docs skips non-function bindings", {
  tools_env <- new.env()
  assign("a_tool", function(x) x, envir = tools_env)
  assign("not_a_tool", 42L, envir = tools_env)
  docs <- build_tool_docs(tools_env)
  testthat::expect_match(docs, "a_tool", fixed = TRUE)
  testthat::expect_false(grepl("not_a_tool", docs, fixed = TRUE))
})
