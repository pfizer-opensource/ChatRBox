# Chatbots produce AI-generated summaries of any API or tool output when the 
# $talk() argument summarize is set to TRUE

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
      example_data <- data.frame(
        X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30)
      )
      return(example_data)
    }
    
    # Add tool function to chatbot
    ChatRBox_update(object = session, 
                    tools_list = list(return_example_data = return_example_data))
    
    # summarize = TRUE appends summary using default prompt
    # Default prompt includes dataset dimensions, shape and context
    session$talk("Return my example dataset", summarize = TRUE)
    
    # Summary prompts are supplied via summary_list per service
    ChatRBox_update(object = session, 
                    summary_list = list(return_example_data = "Output the summary in French"))
    session$talk("Return my example dataset", summarize = TRUE)
  }
}

# Matching summary prompts to the intended service occurs via name 
# matching logic
# The service name is firstly extracted from the client paths

# API paths have an extra level to avoid namespace conflicts, as these are 
# not defined by users in R
api_path <- "services$api_services$plot$client_plot"
tool_path <- "services$plot$client_plot"

# The same service name is extracted from API and tool paths via indexing
get_service_key(api_path)
get_service_key(tool_path)

# Example set of extracted service names
service_keys <- c("weather_service", "data_processing", "analytics")

# Example set of distinct summary prompts
summary_list <- list(
  weather_service = "Focus on temperature trends",
  data_processing = "Highlight key statistical insights")

# Matched services and summary prompts
processed_summaries <- process_summary_list(
  summary_list, 
  service_keys = service_keys)
processed_summaries
