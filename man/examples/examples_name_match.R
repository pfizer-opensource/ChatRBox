# Lists to compare represent service and token/summary prompt names
arg <- c(alpha = 1, beta = 2)
main <- c(alpha = 10, beta = 20, gamma = 30)

# arg is a named list, leading to no error and NULL result
result <- check_names(
  arg = arg,
  arg_label = "arg",
  input_label = "main"
)
result

# alpha and beta are present in main, leading to no error and NULL result
result <- validate_name_match(
  name_list = arg,
  main_list = main,
  arg_label = "name_list",
  target_label = "main_list"
)
result

# arg has unique names, leading to no error and NULL result 
check_duplicate_names(
  arg = arg,
  arg_label = "arg"
)

# These helpers were defined for their side effects, meaning this example code
# will only lead to NULL assignments. Failing tests will error with a
# descriptive message
