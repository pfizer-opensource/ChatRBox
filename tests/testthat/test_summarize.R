testthat::test_that("Summary prompt template loads correctly", {
  summary_template <- load_prompt_template(prompt_name = "summary_prompt",
                                           package = "ChatRBox")
  testthat::expect_type(summary_template, "character")
  testthat::expect_length(summary_template, 1)
  testthat::expect_gt(nchar(summary_template), 0)
  testthat::expect_match(summary_template, "{object_name}", fixed = TRUE)
})

testthat::test_that("talk() uses a matching summary_list instruction", {
  record <- new.env()
  record$inputs <- character(0)
  record$n <- 0L
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider  = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$inputs <- c(record$inputs, input)
            record$n <- record$n + 1L
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              "A short plain-text summary."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list   = list(get_table = get_table),
    summary_list = list(get_table = "BESPOKE SERVICE SUMMARY INSTRUCTION:")
  )
  
  invisible(utils::capture.output(session$talk("Get me the table",
                                               summarize = TRUE)))
  
  testthat::expect_equal(record$n, 2L)
  followup_input <- record$inputs[[2]]
  testthat::expect_match(followup_input,
                         "BESPOKE SERVICE SUMMARY INSTRUCTION:",
                         fixed = TRUE)
  testthat::expect_match(followup_input, "YOU ARE NOW IN SUMMARY MODE", fixed = TRUE)
  testthat::expect_match(followup_input, "Search the past service outputs", fixed = TRUE)
})

testthat::test_that("talk() falls back to the default summary instruction", {
  record <- new.env()
  record$inputs <- character(0)
  record$n <- 0L
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$inputs <- c(record$inputs, input)
            record$n <- record$n + 1L
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              "A short plain-text summary."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list  = list(get_table = get_table)
  )
  
  invisible(utils::capture.output(session$talk("Get me the table",
                                               summarize = TRUE)))
  
  testthat::expect_equal(record$n, 2L)
  followup_input <- record$inputs[[2]]
  testthat::expect_match(followup_input, "YOU ARE NOW IN SUMMARY MODE", fixed = TRUE)
})

testthat::test_that("ChatRBox_update can add a summary_list entry", {
  record <- new.env()
  record$inputs <- character(0)
  record$n <- 0L
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$inputs <- c(record$inputs, input)
            record$n <- record$n + 1L
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              "A short plain-text summary."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list  = list(get_table = get_table)
  )
  
  ChatRBox::ChatRBox_update(
    session,
    summary_list = list(get_table = "UPDATED SUMMARY INSTRUCTION:")
  )
  
  sl <- ChatRBox::get_property(session$chat_object, "summary_list")
  testthat::expect_equal(sl[["get_table"]], "UPDATED SUMMARY INSTRUCTION:")
  
  invisible(utils::capture.output(session$talk("Get me the table",
                                               summarize = TRUE)))
  testthat::expect_match(record$inputs[[2]], "UPDATED SUMMARY INSTRUCTION:",
                         fixed = TRUE)
})
