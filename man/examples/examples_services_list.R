# Defines tool functions
add <- function(x,y) {
  x + y
}

subtract <- function(x,y) {
  x - y
}

# Constructs named tools list
tools_list = list(add_two_numbers = add,
                  subtract_two_numbers = subtract)

# Defines new tools environment
tools_env <- new.env()

# Adds tools list to tools environments
list2env(tools_list, envir = tools_env)

# Extracts tool function paths from tools environment
# services$ prefix is hard-coded
paths <- get_function_path(obj = tools_env)
paths

# Extracts arguments from tool functions 
function_args <- get_args_str(add)
function_args

# get_args_str() is used as a helper function to combine 
# function paths with arguments
# Extracts tool function paths with arguments from tools environment
args <- get_function_args(obj = tools_env)
args

# This process is used to construct function paths with arguments 
# for every API service/tool function. These are used in the default AI prompt 
