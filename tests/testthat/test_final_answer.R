testthat::test_that("Final answer prompt template loads and interpolates", {
  final_template <- load_prompt_template(prompt_name = "final_answer_prompt",
                                         package = "ChatRBox")
  testthat::expect_type(final_template, "character")
  testthat::expect_length(final_template, 1)
  testthat::expect_gt(nchar(final_template), 0)
  testthat::expect_match(final_template, "{past_outputs}", fixed = TRUE)
  testthat::expect_match(final_template, "{input}", fixed = TRUE)
  testthat::expect_match(final_template, "FINAL ANSWER MODE", fixed = TRUE)
  
  testthat::expect_match(final_template, "INTERNAL identifiers", fixed = TRUE)
  testthat::expect_match(final_template, "must NEVER appear in your answer", fixed = TRUE)
  testthat::expect_match(final_template, "holistic", fixed = TRUE)
  testthat::expect_match(final_template, "the first service", fixed = TRUE)
  
  testthat::expect_match(final_template, "machine-readable instruction", fixed = TRUE)
  testthat::expect_match(final_template, "emit the fenced block ONLY", fixed = TRUE)
  testthat::expect_match(final_template, "Here are the requested outputs", fixed = TRUE)
  
  input <- "Q"
  past_outputs <- "MyTable:\n1"
  instruction <- ""
  rendered <- as.character(glue::glue(final_template))
  testthat::expect_match(rendered, '{ "show":', fixed = TRUE)
  testthat::expect_no_match(rendered, "{past_outputs}", fixed = TRUE)
})

testthat::test_that("parse_display_directive extracts a valid directive", {
  directive <- ChatRBox::parse_display_directive(
    'Here is the answer.```display
{ "show": ["MyTable"], "how": "table" }
```'
  )
  testthat::expect_type(directive, "list")
  testthat::expect_equal(directive$show, "MyTable")
  testthat::expect_equal(directive$how, "table")
})

testthat::test_that("parse_display_directive supports multiple names", {
  directive <- ChatRBox::parse_display_directive(
    '```display
{ "show": ["A", "B"], "how": "inline" }
```'
  )
  testthat::expect_equal(directive$show, c("A", "B"))
  testthat::expect_equal(directive$how, "inline")
})

testthat::test_that("parse_display_directive defaults 'how' to table", {
  d1 <- ChatRBox::parse_display_directive('```display
{ "show": ["X"] }
```')
  testthat::expect_equal(d1$how, "table")
  
  d2 <- ChatRBox::parse_display_directive('```display
{ "show": ["X"], "how": "banana" }
```')
  testthat::expect_equal(d2$how, "table")
})

testthat::test_that("parse_display_directive returns NULL when absent or empty", {
  testthat::expect_null(ChatRBox::parse_display_directive("Just prose, nothing to show."))
  testthat::expect_null(ChatRBox::parse_display_directive(NULL))
  testthat::expect_null(ChatRBox::parse_display_directive('```display
{ "show": [] }
```'))
})

testthat::test_that("resolve_display strips fences and attaches intent (context-independent)", {
  resolved <- ChatRBox::resolve_display(
    'Here is the interpretation.```display
{ "show": ["MyTable"], "how": "table" }
```'
  )
  testthat::expect_equal(as.character(resolved), "Here is the interpretation.")
  testthat::expect_no_match(as.character(resolved), "```", fixed = TRUE)
  testthat::expect_no_match(as.character(resolved), "display", fixed = TRUE)
  testthat::expect_equal(attr(resolved, "display")$show, "MyTable")
  testthat::expect_equal(attr(resolved, "display")$how, "table")
})

testthat::test_that("resolve_display strips json tool-call fences too", {
  resolved <- ChatRBox::resolve_display(
    'Running the service.```json
{ "path": "services$tools$get_table", "name": "MyTable" }
```'
  )
  testthat::expect_equal(as.character(resolved), "Running the service.")
  testthat::expect_no_match(as.character(resolved), "```", fixed = TRUE)
  testthat::expect_null(attr(resolved, "display"))
})

testthat::test_that("resolve_display yields clean prose with NULL intent for malformed/absent directives", {
  r1 <- ChatRBox::resolve_display("Just a plain conversational reply.")
  testthat::expect_equal(as.character(r1), "Just a plain conversational reply.")
  testthat::expect_null(attr(r1, "display"))
  
  r2 <- ChatRBox::resolve_display('Some prose.```display
{ "show": [] }
```')
  testthat::expect_equal(as.character(r2), "Some prose.")
  testthat::expect_no_match(as.character(r2), "```", fixed = TRUE)
  testthat::expect_null(attr(r2, "display"))
  
  r3 <- ChatRBox::resolve_display(NULL)
  testthat::expect_equal(as.character(r3), "")
  testthat::expect_null(attr(r3, "display"))
})

testthat::test_that("llm_final_answer errors on missing chat or input", {
  testthat::expect_error(ChatRBox::llm_final_answer(chat = NULL, input = "x"),
                         regexp = "chat is NULL")
  fake_chat <- list(chat = function(input) "hi")
  testthat::expect_error(ChatRBox::llm_final_answer(chat = fake_chat, input = NULL),
                         regexp = "input is empty")
})

testthat::test_that("llm_final_answer synthesizes across all outputs and attaches display", {
  object_env <- new.env()
  object_env$MyTable <- data.frame(a = 1:3, b = 4:6)
  object_env$Note <- "some text output"
  
  record <- new.env()
  fake_chat <- list(chat = function(input) {
    record$input <- input
    'A single synthesized answer across all outputs.```display
{ "show": ["MyTable"], "how": "link" }
```'
  })
  
  answer <- ChatRBox::llm_final_answer(
    chat       = fake_chat,
    input      = "Show me the table and interpret it",
    result     = list(MyTable = 1, Note = 1),
    object_env = object_env
  )
  
  testthat::expect_type(answer, "character")
  testthat::expect_equal(as.character(answer),
                         "A single synthesized answer across all outputs.")
  testthat::expect_no_match(as.character(answer), "```", fixed = TRUE)
  
  directive <- attr(answer, "display")
  testthat::expect_equal(directive$show, "MyTable")
  testthat::expect_equal(directive$how, "link")
  
  testthat::expect_match(record$input, "Show me the table and interpret it", fixed = TRUE)
  testthat::expect_match(record$input, "MyTable", fixed = TRUE)
  testthat::expect_match(record$input, "Note", fixed = TRUE)
  testthat::expect_match(record$input, "FINAL ANSWER MODE", fixed = TRUE)
})

testthat::test_that("llm_final_answer returns NULL display attribute when no directive", {
  object_env <- new.env()
  object_env$MyTable <- data.frame(a = 1:3)
  
  fake_chat <- list(chat = function(input) "Just the interpretation, no raw data.")
  
  answer <- ChatRBox::llm_final_answer(
    chat       = fake_chat,
    input      = "What is the trend?",
    result     = list(MyTable = 1),
    object_env = object_env
  )
  
  testthat::expect_equal(as.character(answer), "Just the interpretation, no raw data.")
  testthat::expect_null(attr(answer, "display"))
})

testthat::test_that("talk(summarize = 'final') returns one synthesized answer", {
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
              "The table shows values increasing steadily across both columns."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = get_table)
  )
  
  answer <- invisible(utils::capture.output(
    result <- session$talk("What does my table show?", summarize = "final")
  ))
  
  testthat::expect_equal(record$n, 2L)
  testthat::expect_type(result, "character")
  testthat::expect_equal(as.character(result),
                         "The table shows values increasing steadily across both columns.")
  testthat::expect_match(record$inputs[[2]], "FINAL ANSWER MODE", fixed = TRUE)
  testthat::expect_match(record$inputs[[2]], "MyTable", fixed = TRUE)
})

testthat::test_that("talk(summarize = 'final') emits only the synthesized answer, not master narration", {
  record <- new.env()
  record$n <- 0L
  
  add_then_subtract <- function() -4
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$n <- record$n + 1L
            if (record$n == 1L) {
              'The first service adds 3 and 3, storing the sum as AddResult. The second service subtracts 10 from that sum.```json
{
  "path": "services$tools$add_then_subtract",
  "name": "AddResult"
}
```'
            } else {
              "The calculation yields a final result of -4."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(add_then_subtract = add_then_subtract)
  )
  
  printed <- utils::capture.output(
    res <- session$talk("What is the result?", summarize = "final")
  )
  
  testthat::expect_equal(as.character(res),
                         "The calculation yields a final result of -4.")
  testthat::expect_no_match(as.character(res), "AddResult", fixed = TRUE)
  testthat::expect_no_match(as.character(res), "The first service", fixed = TRUE)
  testthat::expect_false(any(grepl("AddResult", printed, fixed = TRUE)))
  testthat::expect_false(any(grepl("The first service", printed, fixed = TRUE)))
})

testthat::test_that("talk(summarize = 'final') falls back to resolved base dialogue when no answer", {
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) "Just a plain conversational reply.",
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = function() data.frame(a = 1:3))
  )
  
  printed <- utils::capture.output(
    res <- session$talk("hello there", summarize = "final")
  )
  
  testthat::expect_equal(as.character(res), "Just a plain conversational reply.")
  testthat::expect_true(is.null(attr(res, "display")))
  testthat::expect_true(any(grepl("Just a plain conversational reply.", printed,
                                  fixed = TRUE)))
})

testthat::test_that("talk(summarize = 'final') attaches the display intent on the returned answer", {
  record <- new.env()
  record$n <- 0L
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$n <- record$n + 1L
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              'The table shows values increasing steadily.```display
{ "show": ["MyTable"], "how": "table" }
```'
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = get_table)
  )
  
  invisible(utils::capture.output(
    res <- session$talk("show me my table", summarize = "final")
  ))
  
  testthat::expect_equal(as.character(res), "The table shows values increasing steadily.")
  testthat::expect_no_match(as.character(res), "```", fixed = TRUE)
  testthat::expect_equal(attr(res, "display")$show, "MyTable")
  testthat::expect_equal(attr(res, "display")$how, "table")
})

testthat::test_that("talk() resolves the display channel on a no-tool conversational turn", {
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            'Here are those outputs.```display
{ "show": ["MyTable"], "how": "table" }
```'
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = function() data.frame(a = 1:3))
  )
  
  printed <- utils::capture.output(
    res <- session$talk("show me those outputs")
  )
  
  testthat::expect_equal(as.character(res), "Here are those outputs.")
  testthat::expect_no_match(as.character(res), "```", fixed = TRUE)
  testthat::expect_equal(attr(res, "display")$show, "MyTable")
  testthat::expect_equal(attr(res, "display")$how, "table")
  
  testthat::expect_false(any(grepl("```display", printed, fixed = TRUE)))
})

testthat::test_that("talk() rejects unknown summarize values", {
  get_table <- function() data.frame(a = 1:3)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) "noop",
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = get_table)
  )
  
  testthat::expect_error(session$talk("hi", summarize = "bogus"),
                         regexp = "summarize must be")
  testthat::expect_error(session$talk("hi", summarize = NA),
                         regexp = "summarize must be")
})

testthat::test_that("talk(summarize = TRUE) is unchanged by the new mode", {
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
    tools_list = list(get_table = get_table)
  )
  
  result <- invisible(utils::capture.output(
    res <- session$talk("Get me the table", summarize = TRUE)
  ))
  
  testthat::expect_equal(record$n, 2L)
  testthat::expect_match(record$inputs[[2]], "YOU ARE NOW IN SUMMARY MODE", fixed = TRUE)
  testthat::expect_true(is.list(res))
  testthat::expect_true("MyTable" %in% names(res))
})


testthat::test_that("final_summary_prompt defaults to empty string with no package default", {
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) "noop",
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = function() data.frame(a = 1:3))
  )
  default_prompt <- ChatRBox::get_property(session$chat_object,
                                           "final_summary_prompt")
  testthat::expect_equal(default_prompt, "")
})

testthat::test_that("talk(summarize = 'final') default output is unchanged by the new argument", {
  record <- new.env()
  record$n <- 0L
  record$inputs <- character(0)
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$n <- record$n + 1L
            record$inputs <- c(record$inputs, input)
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              "The table shows values increasing steadily across both columns."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = get_table)
  )
  
  invisible(utils::capture.output(
    res <- session$talk("What does my table show?", summarize = "final")
  ))
  
  testthat::expect_equal(as.character(res),
                         "The table shows values increasing steadily across both columns.")
  testthat::expect_match(record$inputs[[2]], "FINAL ANSWER MODE", fixed = TRUE)
})

testthat::test_that("a literal final_summary_prompt reaches the synthesis call input", {
  record <- new.env()
  record$n <- 0L
  record$inputs <- character(0)
  
  steer <- "STEER_TOKEN_XYZ answer concisely."
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$n <- record$n + 1L
            record$inputs <- c(record$inputs, input)
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              "A synthesized answer."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = get_table),
    final_summary_prompt = steer
  )
  
  invisible(utils::capture.output(
    res <- session$talk("What does my table show?", summarize = "final")
  ))
  
  testthat::expect_match(record$inputs[[2]], "STEER_TOKEN_XYZ", fixed = TRUE)
  testthat::expect_match(record$inputs[[2]], "FINAL ANSWER MODE", fixed = TRUE)
})

testthat::test_that("a final_summary_prompt .md path is read and interpolated", {
  record <- new.env()
  record$n <- 0L
  record$inputs <- character(0)
  
  tmp_md <- tempfile(fileext = ".md")
  on.exit(unlink(tmp_md), add = TRUE)
  writeLines("STEER_FROM_FILE_ABC follow this steer.", tmp_md)
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) {
            record$n <- record$n + 1L
            record$inputs <- c(record$inputs, input)
            if (record$n == 1L) {
              'Running the service.```json
{
  "path": "services$tools$get_table",
  "name": "MyTable"
}
```'
            } else {
              "A synthesized answer."
            }
          },
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = get_table),
    final_summary_prompt = tmp_md
  )
  
  # Property stores the path; llm_final_answer resolves it at call time
  testthat::expect_equal(
    ChatRBox::get_property(session$chat_object, "final_summary_prompt"),
    tmp_md
  )
  
  invisible(utils::capture.output(
    res <- session$talk("What does my table show?", summarize = "final")
  ))
  
  testthat::expect_match(record$inputs[[2]], "STEER_FROM_FILE_ABC", fixed = TRUE)
})

testthat::test_that("ChatRBox_update updates final_summary_prompt in place", {
  session <- ChatRBox::ChatRBox$new(
    ai_provider = local({
      function(system_prompt = NULL, ...) {
        list(
          chat = function(input) "noop",
          set_system_prompt = function(prompt) invisible(NULL)
        )
      }
    }),
    tools_list = list(get_table = function() data.frame(a = 1:3))
  )
  
  original <- ChatRBox::get_property(session$chat_object, "final_summary_prompt")
  ChatRBox::ChatRBox_update(session, final_summary_prompt = "NEW_STEER_123")
  updated <- ChatRBox::get_property(session$chat_object, "final_summary_prompt")
  
  testthat::expect_false(identical(original, updated))
  testthat::expect_equal(updated, "NEW_STEER_123")
  
  ChatRBox::ChatRBox_update(session, prompt_template = "unrelated {paths_string}")
  testthat::expect_equal(
    ChatRBox::get_property(session$chat_object, "final_summary_prompt"),
    "NEW_STEER_123"
  )
})

testthat::test_that("final_summary_prompt does not leak into summarize = TRUE or FALSE", {
  record <- new.env()
  record$n <- 0L
  record$inputs <- character(0)
  
  get_table <- function() data.frame(a = 1:3, b = 4:6)
  
  make_session <- function() {
    ChatRBox::ChatRBox$new(
      ai_provider = local({
        function(system_prompt = NULL, ...) {
          list(
            chat = function(input) {
              record$n <- record$n + 1L
              record$inputs <- c(record$inputs, input)
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
      tools_list = list(get_table = get_table),
      final_summary_prompt = "LEAK_TOKEN_SHOULD_NOT_APPEAR"
    )
  }
  
  # summarize = TRUE
  record$n <- 0L; record$inputs <- character(0)
  session_true <- make_session()
  res_true <- invisible(utils::capture.output(
    out_true <- session_true$talk("Get me the table", summarize = TRUE)
  ))
  testthat::expect_true(is.list(out_true))
  testthat::expect_true("MyTable" %in% names(out_true))
  testthat::expect_match(record$inputs[[2]], "YOU ARE NOW IN SUMMARY MODE", fixed = TRUE)
  testthat::expect_false(any(grepl("LEAK_TOKEN_SHOULD_NOT_APPEAR",
                                   record$inputs, fixed = TRUE)))
  
  # summarize = FALSE
  record$n <- 0L; record$inputs <- character(0)
  session_false <- make_session()
  res_false <- invisible(utils::capture.output(
    out_false <- session_false$talk("Get me the table", summarize = FALSE)
  ))
  testthat::expect_true(is.list(out_false))
  testthat::expect_true("MyTable" %in% names(out_false))
  testthat::expect_false(any(grepl("LEAK_TOKEN_SHOULD_NOT_APPEAR",
                                   record$inputs, fixed = TRUE)))
})
