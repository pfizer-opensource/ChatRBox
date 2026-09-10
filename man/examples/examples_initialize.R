# The R6 object, session, utilizes the Ollama AI provider from the ellmer package 
# and defines AI model within additional arguments
# The chatbot object is initialized using $new
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
    
    # Interacts with the initialized session using $talk
    session$talk("How many states are there in the USA?")
    
    # Asks follow-up questions using chatbot memory
    session$talk("How many of these begin with the letter A?")
  }
}
