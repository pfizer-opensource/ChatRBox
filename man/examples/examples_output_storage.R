# Any API or tool output is stored inside object_env until chatbot 
# initialization under an AI-generated, human-readable name
# Chained API calls are facilitated by passing these names as subsequent 
# API arguments

# Create new data and object environments
data_env <- new.env()
object_env <- new.env()

# Populate data_env with a data frame
data_env$my_data <- data.frame(x = 1:5, y = 6:10)

# Populate object_env with a fictitious past output 
object_env$previous_result <- list(status = "completed", value = 42)

# Chatbots create argument lists based on names within object_env and data_env
arg_list <- list(
  input_data = "my_data",
  previous = "previous_result",
  literal = "hello world"
)

# Human-readable names are replaced with the associated objects during 
# chained API calls 
resolved_args <- resolve_arg_refs(
  arg_list, 
  data_env = data_env, 
  object_env = object_env
)

# The list conatins a data frame, list and character string
str(resolved_args)

# Hence, this process is dependent on valid AI-generated names
# These functions prevent errors following AI naming hallucinations 

# Example AI-generated names
names <- list(
  "valid_name",
  "  spaces_around  ",
  "",
  NA,
  42
)

# Normalized names remove whitespace and produce fallback values for 
# invalid entries
normalized <- lapply(seq_along(names), function(i) {
  normalize_result_name(names[[i]], index = i)
})
print(normalized)

# Example API results
results <- list(
  valid_result = list(data = c(1, 2, 3)),
  null_result = NULL
)

# Store all results incl NULL
lapply(names(results), function(name) {
  result <- results[[name]]
  store_api_result(result, name, object_env)
})

# Verify all results are present
ls(object_env)
