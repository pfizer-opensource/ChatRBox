# Tests internal prompt
# Verifies output is a character class
testthat::expect_true(methods::is(load_prompt_template(), "character"))

# Tests object properties
new <- object_generate()

get_property(new, property = "prompt")

testthat::test_that("Environments returned by default", {
  properties <- property_generate(new)
  testthat::expect_true(is.environment(properties$tools_env))
  testthat::expect_true(is.environment(properties$data_env))
  testthat::expect_true(is.environment(properties$object_env))
})

# Defines tool functions
add <- function(x, y) { x + y }
subtract <- function(x, y) { x - y }

# Constructs named tools list
tools_list <- list(
  add_two_numbers = add,
  subtract_two_numbers = subtract
)

updated <- object_update(new, tools_list = tools_list)

# Original S7 object has been updated
# Expect for updated to differ from new
testthat::expect_false(identical(new, updated))

# Creates new environment
tools_env <- new.env()

# Cannot convert empty tools_env to string
testthat::test_that("Empty Environment", {
  string <- env_to_str(tools_env)
  testthat::expect_equal(string, "")
})

# Adds tools list to new environment
list2env(tools_list, envir = tools_env)

# String of tools_env contents
testthat::test_that("Environment to string", {
  string <- env_to_str(tools_env)
  testthat::expect_type(string, "character")
  testthat::expect_match(string, "add_two_numbers", fixed = TRUE)
  testthat::expect_match(string, "subtract_two_numbers", fixed = TRUE)
})

# Extracts tool function paths from tools environment
paths <- get_function_path(obj = tools_env)
testthat::expect_no_error(paths)
testthat::expect_equal(
  paths,
  c("services$add_two_numbers", "services$subtract_two_numbers")
)

# Extracts arguments from tool function
function_args <- get_args_str(add)
testthat::expect_no_error(function_args)
testthat::expect_equal(function_args, "x, y")

# Extracts tool function paths with arguments from tools environment
args <- get_function_args(obj = tools_env)
testthat::expect_no_error(args)
testthat::expect_equal(
  args, 
  c(
    "services$add_two_numbers(x, y)",
    "services$subtract_two_numbers(x, y)"
  )
)

# S7/R6 tool documentation interpolation
testthat::test_that("tool_docs defaults empty and off leaves prompt unchanged", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  testthat::expect_identical(get_property(object, "tool_docs"), "")
})

testthat::test_that("tool_docs = TRUE populates the S7 property and prompt", {
  add <- function(x, y) x + y
  object <- object_generate(
    tools_list = list(standard_deviation = stats::sd),
    tool_docs = TRUE)
  docs <- get_property(object, "tool_docs")
  testthat::expect_true(nzchar(docs))
  testthat::expect_match(get_property(object, "prompt"),
                         "standard_deviation", fixed = TRUE)
})

testthat::test_that("tool_docs S7 property is validated as length 1", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  testthat::expect_error(
    { object@tool_docs <- c("a", "b") },
    regexp = "@tool_docs must be length 1"
  )
})

testthat::test_that("object_update regenerates tool_docs", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  updated <- object_update(object, tool_docs = TRUE)
  testthat::expect_true(nzchar(get_property(updated, "tool_docs")))
})
