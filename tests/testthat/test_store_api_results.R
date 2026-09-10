test_that("resolve_arg_refs handles various input scenarios", {
  # Create test environments
  object_env <- new.env()
  data_env <- new.env()
  
  # Populate test environments
  assign("test_object", list(a = 1, b = 2), envir = object_env)
  assign("test_data", data.frame(x = 1:3), envir = data_env)
  
  # Test resolving from object_env
  arg_list1 <- list(x = "test_object")
  resolved1 <- resolve_arg_refs(arg_list1, object_env = object_env)
  expect_equal(resolved1$x, list(a = 1, b = 2))
  
  # Test resolving from data_env
  arg_list2 <- list(y = "test_data")
  resolved2 <- resolve_arg_refs(arg_list2, data_env = data_env)
  expect_equal(resolved2$y, data.frame(x = 1:3))
  
  # Test nested list resolution
  arg_list3 <- list(z = list(nested = "test_object"))
  resolved3 <- resolve_arg_refs(arg_list3, object_env = object_env)
  expect_equal(resolved3$z$nested, list(a = 1, b = 2))
  
  # Test non-reference arguments remain unchanged
  arg_list4 <- list(a = "literal", b = 42)
  resolved4 <- resolve_arg_refs(arg_list4)
  expect_equal(resolved4, arg_list4)
  
  # Test empty list
  expect_equal(resolve_arg_refs(), list())
})

test_that("normalize_result_name handles various naming scenarios", {
  # Valid names
  expect_equal(normalize_result_name("valid_name"), "valid_name")
  expect_equal(normalize_result_name(" trimmed_name "), "trimmed_name")
  
  # Invalid names
  expect_equal(normalize_result_name(""), "api_result_1")
  expect_equal(normalize_result_name(NA_character_), "api_result_1")
  expect_equal(normalize_result_name(NULL), "api_result_1")
  expect_equal(normalize_result_name(c("multiple", "names")), "api_result_1")
  
  # Custom index
  expect_equal(normalize_result_name("", index = 5), "api_result_5")
})

test_that("store_api_result handles different storage scenarios", {
  # Create test environment
  object_env <- new.env()
  
  # Successful storage
  expect_true(store_api_result(list(a = 1), "test_result", object_env))
  expect_true(exists("test_result", envir = object_env))
  expect_equal(get("test_result", envir = object_env), list(a = 1))
  
  # Failed storage scenarios
  expect_false(store_api_result(list(a = 1), "", object_env))
  
  # Invalid environment
  expect_error(store_api_result(list(a = 1), "test", list()), 
               "object_env must be an environment.")
})
