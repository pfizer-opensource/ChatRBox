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
    
    # Defining example data set in R
    example_data <- data.frame(
      X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30)
    )
    
    # Simulate the deployment of a remote Plumber API service using the linear 
    # plotting API, stored locally as plumber.R in inst/api_service 
    # This takes the default port and host, later referenced in the root URL
    proc <- ChatRBox::start_api_service(port = 8000, 
                                        host = "127.0.0.1")
    
    proc$wait() # This gives the API time to start 
    
    # Note that R modules may have to be loaded for local API access
    # Hence, the plotting service can be added to the existing chatbot object 
    # by passing the root URL to the update function
    ChatRBox::ChatRBox_update(object = session, 
                              data_list = list(example_data = example_data),
                              services_list = list(line_plot = "http://127.0.0.1:8000"))
    
    # Ask a question involving passing example_data to the plotting API
    session$talk("Produce a linear plot of my data set called example_data")
  }
}
