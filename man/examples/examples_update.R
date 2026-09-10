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
    
    # Simulates deployment of remote API service using the linear plotting API, 
    # stored locally as plumber.R in inst/api_service 
    proc <- start_api_service()
    Sys.sleep(2) # This gives the API time to start
    
    # Defines data set
    example_data <- data.frame(
      X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30)
    )
    
    # Defines tool function
    add <- function(x,y) {
      x + y
    }
    
    # Updates chatbot session given new parameters
    ChatRBox_update(object = session,
                    services_list = list(line_plot = "http://127.0.0.1:8000"),
                    data_list = list(example_data = example_data),
                    tools_list = list(add_two_numbers = add))
    
    # Interacts with chatbot given new parameters
    session$talk("Produce a linear plot of example_data")
    session$talk("Use a service to compute 70 + 76")
  }
}
