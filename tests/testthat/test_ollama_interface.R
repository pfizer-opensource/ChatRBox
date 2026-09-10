if (Sys.getenv("TESTTHAT_RUN_OLLAMA") != "true") {
  testthat::skip("Skipping Ollama tests: set TESTTHAT_RUN_OLLAMA=true to enable")
}

testthat::skip_if_not_installed("ellmer")

ollama_model    <- Sys.getenv("OLLAMA_MODEL", unset = "mistral")
ollama_base_url <- Sys.getenv("OLLAMA_HOST",  unset = "http://localhost:11434")

# --- Helper: turns live-call timeouts into skips --------------------------
# Runs `expr`; if it raises a curl operation-timeout, the test is skipped instead of
# failing. All other errors are re-raised so genuine bugs still fail
skip_on_timeout <- function(expr) {
  withCallingHandlers(
    expr,
    error = function(e) {
      msg <- conditionMessage(e)
      if (inherits(e, "curl_error_operation_timedout") ||
          grepl("Timeout was reached", msg, fixed = TRUE)) {
        testthat::skip(paste("Live Ollama call timed out:", msg))
      }
    }
  )
}

# --- Infrastructure checks -----------------------------------------------

testthat::test_that("Ollama endpoint is reachable", {
  response <- skip_on_timeout(
    testthat::expect_no_error(
      httr2::request(ollama_base_url) |>
        httr2::req_perform()
    )
  )
  testthat::expect_false(is.null(response))
  testthat::expect_equal(httr2::resp_status(response), 200)
})

testthat::test_that("Expected model is available", {
  models <- skip_on_timeout(
    ellmer::models_ollama(base_url = ollama_base_url)
  )
  testthat::expect_false(is.null(models))
  model_names <- if ("name" %in% names(models)) models$name else as.character(models)
  testthat::expect_true(any(startsWith(model_names, ollama_model)))
})

# --- Test 1: ChatRBox$new() initializes with an empty data_env -----------

testthat::test_that("ChatRBox initializes with an empty data_env", {
  session <- ChatRBox::ChatRBox$new(
    ai_provider = ellmer::chat_ollama,
    model       = ollama_model,
    base_url    = ollama_base_url
  )
  initial_data <- ls(envir = ChatRBox::get_property(session$chat_object, "data_env"))
  testthat::expect_equal(length(initial_data), 0L)
})

# Build a shared session for remaining tests
session <- ChatRBox::ChatRBox$new(
  ai_provider = ellmer::chat_ollama,
  model       = ollama_model,
  base_url    = ollama_base_url
)

testthat::test_that("ChatRBox initializes without error", {
  testthat::expect_true(inherits(session, "ChatRBox"))
})

# --- Test 2: ChatRBox_update() correctly populates data_env --------------

testthat::test_that("ChatRBox_update() populates data_env", {
  simple_data <- data.frame(X = 1:5, Y = c(10, 13, 15, 18, 21))
  fave_color  <- "blue"
  new_data_list <- list(simple_data = simple_data, fave_color = fave_color)
  
  ChatRBox::ChatRBox_update(object = session, data_list = new_data_list)
  
  data_env_objects <- ls(envir = ChatRBox::get_property(session$chat_object, "data_env"))
  testthat::expect_true(all(names(new_data_list) %in% data_env_objects))
})

# --- Test 3: ChatRBox_update() errors on an unnamed list -----------------

testthat::test_that("ChatRBox_update() errors on an unnamed list", {
  unnamed_list <- list(data.frame(X = 1:3), "blue")  
  testthat::expect_null(names(unnamed_list))
  
  testthat::expect_error(
    ChatRBox::ChatRBox_update(object = session, data_list = unnamed_list),
    regexp = "must be a character vector of the same length as x"
  )
})

# --- Test 4: $talk() errors when input is missing ------------------------

testthat::test_that("$talk() errors when input is missing", {
  testthat::expect_error(session$talk(), regexp = '"input" is missing')
})

# --- Test 5: $talk("") succeeds with an empty string --------------------

testthat::test_that("$talk('') succeeds with an empty string", {
  skip_on_timeout(
    testthat::expect_no_error(session$talk(""))
  )
})

# --- Test 6: ChatRBox_update() syncs the prompt onto the ellmer object ----

testthat::test_that("ChatRBox_update() pushes a changed prompt to the chat object", {
  add <- function(x, y) x + y
  
  # Adding a tool changes paths_string/args_string, so the regenerated
  # system prompt must differ from the current one
  ChatRBox::ChatRBox_update(object = session, tools_list = list(add_two_numbers = add))
  
  s7_prompt <- ChatRBox::get_property(session$chat_object, "prompt")
  r6_prompt <- session$chat$get_system_prompt()
  testthat::expect_identical(r6_prompt, s7_prompt)
  testthat::expect_match(r6_prompt, "add_two_numbers", fixed = TRUE)
})

# --- Test 7: a no-op ChatRBox_update() keeps R6 and S7 prompts in sync ----

testthat::test_that("No-op ChatRBox_update() leaves the prompt consistent and errors-free", {
  prompt_before <- session$chat$get_system_prompt()
  
  # No new services/tools/data: the regenerated prompt is identical
  testthat::expect_no_error(ChatRBox::ChatRBox_update(object = session))
  
  prompt_after  <- session$chat$get_system_prompt()
  s7_prompt     <- ChatRBox::get_property(session$chat_object, "prompt")
  testthat::expect_identical(prompt_after, prompt_before)
  testthat::expect_identical(prompt_after, s7_prompt)
})

# --- Existing live-call test ---------------------------------------------

testthat::test_that("ChatRBox talk returns a non-empty response", {
  talk_output <- skip_on_timeout(
    capture.output(session$talk("hello"))
  )
  testthat::expect_true(is.character(talk_output))
  testthat::expect_true(any(nzchar(trimws(talk_output))))
})
