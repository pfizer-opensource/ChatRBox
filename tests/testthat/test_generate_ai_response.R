# Good for functions that should only be passed args to be used
testthat::test_that("Empty/default inputs", {
  testthat::expect_error({
    code_extract()
  }, regexp = "string is empty")
  
  testthat::expect_error({
    code_remove()
  }, regexp = "string is empty")
  
  testthat::expect_error({
    extract_path()
  }, regexp = "path is empty")
  
  testthat::expect_error({
    llm_api_result()
  }, regexp = "object is NULL")
})

testthat::test_that("Extracting code blocks", {
  extract <- code_extract(string = 'Testing parsing functions```json
  {
    "NO": "",
    "ERROR": ""
  }
  ```', language = "json")
  
  testthat::expect_equal(extract, 
                         "  {\n    \"NO\": \"\",\n    \"ERROR\": \"\"\n  }\n  ")
})

testthat::test_that("Removing code blocks", {
  remove <- code_remove(string = 'Testing parsing functions```json
  {
    "NO": "",
    "ERROR": ""
  }
  ```', language = "json")
  
  testthat::expect_equal(remove, "Testing parsing functions")
})

testthat::test_that("Extract path returns last components", {
  info <- extract_path("this$is$an$example$path")
  testthat::expect_no_error(info)
})

testthat::test_that("Insufficient path length", {
  info <- extract_path("path")
  testthat::expect_type(info, "list")
  testthat::expect_true(is.na(info[[1]]))
})

testthat::test_that("Extract name", {
  response <- 'This service adds two numbers together!```json
  {
   "path": "services$tools$add_two_numbers",
   "name": "add",
   "x": 70,
   "y": 76
  }
  ```'
  code <- code_extract(response)
  name <- get_name(json_str = code)
  testthat::expect_equal(name, "add")
})

testthat::test_that("Empty JSON string", {
  name <- get_name(json_str = "")
  testthat::expect_equal(name, NA)
})

testthat::test_that("Correct do.call()", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'This service adds two numbers together!```json
  {
   "path": "services$tools$add_two_numbers",
   "name": "add",
   "x": 70,
   "y": 76
  }
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_type(result, "list")
})

testthat::test_that("Chained API calls resolve object_env arguments", {
  get_data <- function() data.frame(x = 1:3, y = 4:6)
  summarize_data <- function(data) sum(data$y)
  object <- object_generate(tools_list = list(
    get_data = get_data,
    summarize_data = summarize_data
  ))
  response <- 'Chaining two services together```json
  [
    {
      "path": "services$tools$get_data",
      "name": "MyData"
    },
    {
      "path": "services$tools$summarize_data",
      "name": "MySummary",
      "data": "MyData"
    }
  ]
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_type(result, "list")
  testthat::expect_equal(length(result), 2)
  testthat::expect_true(is.data.frame(result[["MyData"]]))
  testthat::expect_equal(result[["MySummary"]], 15)
})

testthat::test_that("resolve_arg_refs returns input unchanged when empty", {
  testthat::expect_equal(resolve_arg_refs(list()), list())
  testthat::expect_equal(
    resolve_arg_refs(list(x = 1, y = "literal")),
    list(x = 1, y = "literal")
  )
})

testthat::test_that("resolve_arg_refs resolves any object type by name", {
  data_env <- new.env()
  object_env <- new.env()
  assign("my_df", data.frame(a = 1:2), envir = data_env)
  assign("my_json", list(species = "setosa", values = list(1, 2, 3)),
         envir = object_env)
  assign("my_text", "a narrative summary of results", envir = object_env)
  assign("my_vec", c(1.5, 2.5, 3.5), envir = object_env)
  
  resolved <- resolve_arg_refs(
    arg_list = list(df = "my_df", body = "my_json",
                    text = "my_text", nums = "my_vec",
                    literal = "not_stored"),
    data_env = data_env,
    object_env = object_env
  )
  
  testthat::expect_true(is.data.frame(resolved$df))
  testthat::expect_type(resolved$body, "list")
  testthat::expect_equal(resolved$body$species, "setosa")
  testthat::expect_equal(resolved$text, "a narrative summary of results")
  testthat::expect_equal(resolved$nums, c(1.5, 2.5, 3.5))
  testthat::expect_equal(resolved$literal, "not_stored")
})

testthat::test_that("resolve_arg_refs only resolves registered objects (inherits = FALSE)", {
  data_env <- new.env()
  object_env <- new.env()
  global_only <- "should not be used"
  resolved <- resolve_arg_refs(
    arg_list = list(x = "global_only"),
    data_env = data_env,
    object_env = object_env
  )
  testthat::expect_equal(resolved$x, "global_only")
})

testthat::test_that("resolve_arg_refs prefers object_env over data_env", {
  data_env <- new.env()
  object_env <- new.env()
  assign("shared", "from_data", envir = data_env)
  assign("shared", "from_object", envir = object_env)
  resolved <- resolve_arg_refs(
    arg_list = list(x = "shared"),
    data_env = data_env,
    object_env = object_env
  )
  testthat::expect_equal(resolved$x, "from_object")
})

testthat::test_that("resolve_arg_refs resolves object_env objects of any type", {
  object_env <- new.env()
  assign("my_df", data.frame(a = 1:2), envir = object_env)
  assign("my_json", list(species = "setosa", values = list(1, 2, 3)),
         envir = object_env)
  assign("my_text", "a narrative summary", envir = object_env)
  resolved <- resolve_arg_refs(
    arg_list = list(df = "my_df", body = "my_json", text = "my_text"),
    object_env = object_env
  )
  testthat::expect_true(is.data.frame(resolved$df))
  testthat::expect_type(resolved$body, "list")
  testthat::expect_equal(resolved$body$species, "setosa")
  testthat::expect_equal(resolved$text, "a narrative summary")
})

testthat::test_that("llm_api_result stores outputs in object_env as named R objects", {
  get_json <- function() list(species = "setosa", measurements = list(1, 2, 3))
  count_fields <- function(json) length(json)
  object <- object_generate(tools_list = list(
    get_json = get_json,
    count_fields = count_fields
  ))
  object_env <- get_property(object, "object_env")
  response <- 'First service returns a JSON object```json
  {
    "path": "services$tools$get_json",
    "name": "iris_json_like"
  }
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_type(result, "list")
  testthat::expect_true(exists("iris_json_like", envir = object_env, inherits = FALSE))
  stored <- get("iris_json_like", envir = object_env, inherits = FALSE)
  testthat::expect_type(stored, "list")
  testthat::expect_equal(stored$species, "setosa")
  response2 <- 'Second service consumes the stored object```json
  {
    "path": "services$tools$count_fields",
    "name": "FieldCount",
    "json": "iris_json_like"
  }
  ```'
  result2 <- llm_api_result(llm_response = response2, object = object)
  testthat::expect_equal(result2[["FieldCount"]], 2)
})

testthat::test_that("object_env outputs feed past_outputs prompt memory", {
  get_json <- function() list(species = "setosa", measurements = list(1, 2, 3))
  object <- object_generate(tools_list = list(get_json = get_json))
  empty_props <- property_generate(object)
  testthat::expect_equal(empty_props$past_outputs, NULL)
})

testthat::test_that("Chained API calls resolve non-data-frame object_env arguments", {
  get_json <- function() list(species = "setosa", measurements = list(1, 2, 3))
  count_fields <- function(json) length(json)
  object <- object_generate(tools_list = list(
    get_json = get_json,
    count_fields = count_fields
  ))
  response <- 'Chaining JSON object across two services```json
  [
    {
      "path": "services$tools$get_json",
      "name": "iris_json_like"
    },
    {
      "path": "services$tools$count_fields",
      "name": "FieldCount",
      "json": "iris_json_like"
    }
  ]
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_type(result, "list")
  testthat::expect_equal(length(result), 2)
  testthat::expect_type(result[["iris_json_like"]], "list")
  testthat::expect_equal(result[["FieldCount"]], 2)
})

testthat::test_that("Non-existent do.call()", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'This service subtracts two numbers from each other!```json
  {
   "name": "subtract",
   "path": "services$tools$subtract_two_numbers",
   "x": 70,
   "y": 76
  }
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_no_error(result)
  captured <- testthat::capture_output(
    llm_api_result(llm_response = response, object = object)
  )
  testthat::expect_match(captured, "ChatRBox attempted to run the function 'subtract_two_numbers' from the service 'tools'", fixed = TRUE)
  testthat::expect_match(captured, "List of 2", fixed = TRUE)
})

testthat::test_that("llm_followup_summary errors on missing inputs", {
  testthat::expect_error(llm_followup_summary(), regexp = "chat is NULL")
  stub_chat <- list(chat = function(input) "ignored")
  testthat::expect_error(llm_followup_summary(chat = stub_chat),
                         regexp = "input is empty")
})

testthat::test_that("get_service_key derives service keys from paths", {
  testthat::expect_equal(
    get_service_key("services$tools$get_table"), "get_table")
  testthat::expect_equal(
    get_service_key("services$api_services$arithmetic$add_two_numbers"),
    "arithmetic")
  testthat::expect_true(is.na(get_service_key("")))
  testthat::expect_true(is.na(get_service_key(NULL)))
})

testthat::test_that("llm_api_result tags results with their service keys", {
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  object <- object_generate(tools_list = list(get_table = get_table))
  response <- 'Run it```json
  {
    "path": "services$tools$get_table",
    "name": "MyTable"
  }
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  keys <- attr(result, "service_keys")
  testthat::expect_false(is.null(keys))
  testthat::expect_equal(keys[["MyTable"]], "get_table")
})

testthat::test_that("Malformed JSON returns NULL instead of erroring", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'This response contains a broken JSON object```json
  {
    "path": "services$tools$add_two_numbers",
    "name": "MaxNumber"     "list": ["fave_color", "simple_data"]
  }
  ```'
  result <- testthat::expect_no_error(
    suppressMessages(llm_api_result(llm_response = response, object = object))
  )
  testthat::expect_null(result)
})

testthat::test_that("Missing 'path' triggers the dynamic error and %||% fallback", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  
  # No "path" key in the JSON object
  response <- 'The AI forgot to choose a service```json
  {
    "name": "MyResult",
    "x": 70,
    "y": 76
  }
  ```'
  result <- testthat::expect_no_error(
    suppressMessages(llm_api_result(llm_response = response, object = object))
  )
  testthat::expect_type(result, "list")
  testthat::expect_equal(length(result), 1)
  
  captured <- testthat::capture_output(
    suppressMessages(llm_api_result(llm_response = response, object = object))
  )
  # The dynamic message is still produced for a failed do.call()
  testthat::expect_match(
    captured,
    "but failed to generate a response",
    fixed = TRUE
  )
})

testthat::test_that("Non-list array element is skipped, not fatal to the batch", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'Mixed array with one bad element```json
  [
    "this is not an object",
    {
      "path": "services$tools$add_two_numbers",
      "name": "GoodResult",
      "x": 1,
      "y": 2
    }
  ]
  ```'
  testthat::expect_message(
    result <- llm_api_result(llm_response = response, object = object),
    regexp = "skipped item 1"
  )
  testthat::expect_type(result, "list")
  testthat::expect_equal(length(result), 2)
  
  # Slot 1: skipped -> "NULL" placeholder under a fallback name
  testthat::expect_equal(result[[1]], "NULL")
  testthat::expect_equal(names(result)[1], "api_result_1")
  
  # Slot 2: the valid object still ran and produced its result
  testthat::expect_equal(result[["GoodResult"]], 3)
})

testthat::test_that("Skipped non-list element gets NA service key", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'Bad first element```json
  [
    "not an object",
    {
      "path": "services$tools$add_two_numbers",
      "name": "GoodResult",
      "x": 4,
      "y": 5
    }
  ]
  ```'
  result <- suppressMessages(
    llm_api_result(llm_response = response, object = object)
  )
  keys <- attr(result, "service_keys")
  testthat::expect_false(is.null(keys))
  testthat::expect_true(is.na(keys[["api_result_1"]]))
  testthat::expect_equal(keys[["GoodResult"]], "add_two_numbers")
})

testthat::test_that("Duplicate AI-generated names are de-duplicated, not collided", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'Two calls, identical names```json
  [
    {
      "path": "services$tools$add_two_numbers",
      "name": "Sum",
      "x": 1,
      "y": 1
    },
    {
      "path": "services$tools$add_two_numbers",
      "name": "Sum",
      "x": 10,
      "y": 10
    }
  ]
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_equal(length(result), 2)
  testthat::expect_true(all(c("Sum", "Sum_1") %in% names(result)))
  testthat::expect_equal(result[["Sum"]], 2)
  testthat::expect_equal(result[["Sum_1"]], 20)
  keys <- attr(result, "service_keys")
  testthat::expect_equal(keys[["Sum"]], "add_two_numbers")
  testthat::expect_equal(keys[["Sum_1"]], "add_two_numbers")
})

testthat::test_that("A failed item does not discard successful earlier items", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  response <- 'One good call, one hallucinated path```json
  [
    {
      "path": "services$tools$add_two_numbers",
      "name": "GoodResult",
      "x": 3,
      "y": 4
    },
    {
      "path": "services$tools$nonexistent_tool",
      "name": "BadResult",
      "x": 1,
      "y": 2
    }
  ]
  ```'
  result <- suppressMessages(
    llm_api_result(llm_response = response, object = object)
  )
  testthat::expect_equal(length(result), 2)
  testthat::expect_equal(result[["GoodResult"]], 7)
  testthat::expect_equal(result[["BadResult"]], "NULL")
})

testthat::test_that("Duplicate names still store independently in object_env", {
  add <- function(x, y) x + y
  object <- object_generate(tools_list = list(add_two_numbers = add))
  object_env <- get_property(object, "object_env")
  response <- 'Two identically named results```json
  [
    {
      "path": "services$tools$add_two_numbers",
      "name": "Total",
      "x": 5,
      "y": 5
    },
    {
      "path": "services$tools$add_two_numbers",
      "name": "Total",
      "x": 100,
      "y": 100
    }
  ]
  ```'
  result <- llm_api_result(llm_response = response, object = object)
  testthat::expect_equal(result[["Total"]], 10)
  testthat::expect_equal(result[["Total_1"]], 200)
})

testthat::test_that("redact_sensitive masks default credential keys", {
  args <- list(
    token        = "MY_TOKEN",
    token_header = "Authorization"
  )
  out <- redact_sensitive(args)
  testthat::expect_equal(out$token, "KEY_REDACTED")
  testthat::expect_equal(out$token_header, "KEY_REDACTED")
})

testthat::test_that("redact_sensitive is case-insensitive on names", {
  args <- list(Token = "MY_TOKEN", AUTH_SCHEME = "Bearer", ApiKey = "MY_TOKEN")
  out <- redact_sensitive(args)
  testthat::expect_equal(out$Token, "KEY_REDACTED")
  testthat::expect_equal(out$AUTH_SCHEME, "KEY_REDACTED")
  testthat::expect_equal(out$ApiKey, "KEY_REDACTED")
})

testthat::test_that("redact_sensitive returns non-list input unchanged", {
  testthat::expect_equal(redact_sensitive("MY_TOKEN"), "MY_TOKEN")
  testthat::expect_equal(redact_sensitive(42L), 42L)
  testthat::expect_null(redact_sensitive(NULL))
})

testthat::test_that("redact_sensitive honors a custom sensitive vector", {
  args <- list(session_key = "MY_TOKEN", token = "MY_TOKEN", limit = 5)
  out <- redact_sensitive(args, sensitive = c("session_key"))
  testthat::expect_equal(out$session_key, "KEY_REDACTED")
  testthat::expect_equal(out$token, "MY_TOKEN")   
  testthat::expect_equal(out$limit, 5)
})

testthat::test_that("redact_sensitive never leaks the real value in str() output", {
  args <- list(
    token      = "MY_TOKEN",
    token_list = list(api_key = "MY_TOKEN")
  )
  dump <- paste(
    utils::capture.output(utils::str(redact_sensitive(args))),
    collapse = "\n"
  )
  testthat::expect_false(grepl("MY_TOKEN", dump, fixed = TRUE))
  testthat::expect_true(grepl("KEY_REDACTED", dump, fixed = TRUE))
})

testthat::test_that("redact_sensitive preserves unnamed list elements", {
  args <- list("a", "b", token = "MY_TOKEN")
  out <- redact_sensitive(args)
  testthat::expect_equal(out[[1]], "a")
  testthat::expect_equal(out[[2]], "b")
  testthat::expect_equal(out$token, "KEY_REDACTED")
})

testthat::test_that("redact_sensitive recurses into nested lists", {
  nested_args <- list(
    token_list = list(api_key = "MY_TOKEN")
  )
  out <- redact_sensitive(nested_args)
  testthat::expect_type(out$token_list, "list")                 
  testthat::expect_equal(out$token_list$api_key, "KEY_REDACTED")
})

testthat::test_that("redact_sensitive preserves non-sensitive values", {
  args <- list(
    token      = "MY_TOKEN",
    token_list = list(api_key = "MY_TOKEN", limit = 10)
  )
  out <- redact_sensitive(args)
  testthat::expect_equal(out$token, "KEY_REDACTED")             
  testthat::expect_equal(out$token_list$api_key, "KEY_REDACTED")
  testthat::expect_equal(out$token_list$limit, 10)            
})
