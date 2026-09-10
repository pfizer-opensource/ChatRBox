skip_if_not_installed("ellmer")

config_path <- path.expand("~/ChatRBox_config.yml")
skip_if(
  !file.exists(config_path),
  "ChatRBox_config.yml not found"
)

session <- yaml::read_yaml(config_path, eval.expr = TRUE) |>
  purrr::pluck("session_load", "session")

initial_session_data <- ls(envir = ChatRBox::get_property(session$chat_object,
                                                          "data_env"))

testthat::expect_true(
  # should have nothing
  length(initial_session_data) == 0
)

# Preparing new object to add
simple_data <- data.frame(
  X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30)
)

fave_color <- "blue"

new_data_list <- list(simple_data = simple_data,
                      fave_color = fave_color)

ChatRBox_update(
  object = session,
  data_list = new_data_list)

data_env_objects <-ls(envir = ChatRBox::get_property(session$chat_object,
                                                     "data_env"))
testthat::expect_true(
  all(names(new_data_list) %in%  data_env_objects) )


# Testing what happens when list has no names -----------------------------

# Same list as above, but with names removed
new_data_list_no_names <- new_data_list %>%
  purrr::set_names(NULL)

testthat::expect_null(names(new_data_list_no_names))

# Should produce an error
testthat::expect_error({
  ChatRBox_update(
    object = session,
    data_list = new_data_list_no_names)
},
regexp = "must be a character vector of the same length as x")


# Testing chat input ------------------------------------------------------

# Should have error about missing input
testthat::expect_error(session$talk(),
                       regexp = '"input" is missing')

# No error
testthat::expect_no_error(session$talk(""))


# Testing system prompt updates -------------------------------------------

prompt_before <- session$chat$get_system_prompt()

testthat::expect_no_error(
  ChatRBox_update(object = session)
)

prompt_after <- session$chat$get_system_prompt()
s7_prompt    <- ChatRBox::get_property(session$chat_object, "prompt")


testthat::expect_identical(prompt_after, prompt_before)
testthat::expect_identical(prompt_after, s7_prompt)

# This update alters the system prompt
add <- function(x, y) x + y
ChatRBox_update(object = session, tools_list = list(add_two_numbers = add))

testthat::expect_identical(
  session$chat$get_system_prompt(),
  ChatRBox::get_property(session$chat_object, "prompt")
)
