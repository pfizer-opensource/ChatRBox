# Defines a tools environment containing a documented package function
tools_env <- new.env()
assign("standard_deviation", stats::sd, envir = tools_env)

# Renders package documentation for each tool, excluding examples
docs <- build_tool_docs(tools_env)
cat(docs)

# Retain the examples section for richer prompt grounding
docs_full <- build_tool_docs(tools_env, 
                             include_examples = TRUE)

# The result is brace-escaped and therefore safe for glue interpolation
prompt <- glue::glue("You have these tools:\n\n{docs}")

# Enable documentation interpolation via ChatRBox underlying S7 object
add <- function(x, y) x + y

object <- object_generate(tools_list = list(add_two_numbers = add),
                          tool_docs = TRUE)

get_property(object, "tool_docs")
