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
