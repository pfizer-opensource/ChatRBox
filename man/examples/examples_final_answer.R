# The summarize = "final" $talk() mode produces a single natural-language
# response to a user question, synthesized across all service outputs

# Initializing the ellmer Ollama provider
# See examples.R in man/examples or the ChatRBox vignette for further
# information regarding environmental variables
\donttest{
  ollama_reachable <- function() {
    host <- Sys.getenv("OLLAMA_HOST", "http://localhost:11434")
    tryCatch(
      httr2::resp_status(
        httr2::req_perform(
          httr2::req_error(httr2::request(paste0(host, "/api/tags")),
                           is_error = function(resp) FALSE)
        )
      ) < 400,
      error = function(e) FALSE
    )
  }
  
  if (ollama_reachable()) {
    session <- ChatRBox$new(ai_provider = ellmer::chat_ollama,
                            base_url = Sys.getenv("OLLAMA_HOST", "http://localhost:11434"),
                            model = Sys.getenv("OLLAMA_MODEL", "mistral"))
    
    # Defining a tool function to return example_data in R
    return_example_data <- function() {
      data.frame(
        X = c(1, 2, 3, 4, 5),
        Y = c(10, 13, 15, 18, 21)
      )
    }
    
    ChatRBox_update(object = session,
                    tools_list = list(return_example_data = return_example_data))
    
    # Although the LLM calls return_example_data(), the output is not shown
    answer <- session$talk("What is the overall trend in my example dataset?",
                           summarize = "final")
    answer
    
    # Output renders upon explicit request
    attr(answer, "display")
  }
}

# parse_display_directive() reads a fenced ```display block into a render intent
response <- 'Here is the interpretation.```display
{ "show": ["MyTable"], "how": "table" }
```'
parse_display_directive(response)

# No display block returns NULL
parse_display_directive("Just prose, nothing to show.")

# render_display() prints each named stored output from object_env
env <- new.env()
env$AddResult <- 6
env$FinalResult <- -4
answer_with_intent <- structure(
  "The result is -4.",
  display = list(show = c("AddResult", "FinalResult"), how = "table")
)
render_display(answer_with_intent, env)

# No-op when the display intent is NULL
answer_no_intent <- structure("Just prose.", display = NULL)
render_display(answer_no_intent, env)

# Missing names produce a clear message
answer_missing <- structure(
  "Looking for a ghost.",
  display = list(show = "GhostOutput", how = "table")
)
render_display(answer_missing, env)
