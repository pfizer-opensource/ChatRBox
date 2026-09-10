# ChatRBox Examples ----------------------------------------------------------

# ChatRBox is a mutable R6 object that facilitates AI responses using API services/tools 

# This creates an interactive chatbot whereby services, tools, data, prompts, httr2 request options and environments can be defined and updated by the user

# Firstly, the R6 object is initialized. The following arguments may be defined or take default values: 

# ai_provider: Function for AI provider 
# services_list: Named list of Plumber API URLs 
# tools_list: Named list of R session functions
# openapi_list: Named list of Plumber API OpenAPI schema URLs
# data_list: Named list of wide-formatted data frames 
# tools_env: Environment for tool functions
# data_env: Environment for data frames
# object_env: Environment for past API outputs to facilitate chained API calls
# prompt_template: Prompt to control AI behavior
# summary_list: Prompt for AI-generated summaries
# token_service: API token for services_list URLs
# header_service: API header for services_list URLs
# token_openapi: API token for openapi_list URLs
# header_openapi: API header for openapi_list URLs
# httr2_config: Named list of httr2 request options for generated client functions
# config_env: Environment for httr2 request parameters
# ... : Additional AI provider arguments

# Following initialization, the R6 object can be passed as an argument to ChatRBox_update() to modify parameters in-place without losing chatbot memory. 


# Chatbot Initialization ----------------------------------------------------------

# The R6 object, session, utilizes the Ollama AI provider from the ellmer package and defines AI model within additional arguments
# The chatbot object is initialized using $new
# Note that this file is not incorporated in Rmd checks but checking the Ollama
# server is reachable has been presented for consistency
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
    
    
    # Chatbot Interaction -------------------------------------------------------------
    
    # Interact with the initialized session using $talk
    session$talk("Who is credited for the Theory of Relativity?")
    
    # Chatbots retain conversation memory until re-initialized
    session$talk("And what about superposition?")
    
    
    # Data --------------------------------------------------------------------
    
    # This defines example_data in the R session
    example_data <- data.frame(
      X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30)
    )
    
    # This data frame can be added to the existing chatbot object by passing the variable name to the update function
    ChatRBox_update(object = session, data_list = list(example_data = example_data))
    
    # This update stores example_data in the data environment, which is automatically interpolated into the AI prompt
    # Therefore, the chatbot can immediately view, summarize and manipulate example_data
    
    session$talk("What is the mean of the X column of example_data")
    
    # API Services ------------------------------------------------------------
    
    # NOTE: The local API service is launched using the r_bg() function from callr. This copies user .Renviron files into the R session's temporary directory. Any sensitive information within .Renviron files can be moved to a YAML config file and accessed via keyring instead. 
    
    # Simulate the deployment of a remote Plumber API service using the linear plotting API, stored locally as plumber.R in inst/api_service 
    # This takes the default port and host, later referenced in the root URL
    proc <- start_api_service()
    Sys.sleep(2) # This gives the API time to start but can be adjusted as needed
    
    # Note that R modules may have to be loaded for local API access
    
    # Hence, the plotting service can be added to the existing chatbot object by passing the root URL to the update function
    ChatRBox_update(object = session, services_list = list(line_plot = "http://127.0.0.1:8000"))
    
    # Ask session to use this service with example_data using $talk
    session$talk("Produce a linear plot of my data set called example_data")
    
    # Close the local API service
    proc$kill()
    
    
    # Tool Functions ------------------------------------------------------
    
    # As well as Plumber APIs, services may be provided as tool functions
    # Here, we define an addition function in the R session
    add <- function(x,y) {
      x + y
    }
    
    # The addition tool can be added to the existing chatbot object by passing the tool function name to the update function
    ChatRBox_update(object = session, tools_list = list(add_two_numbers = add))
    
    # Ask an arithmetic function using $talk and the result is displayed
    session$talk("Use a service to compute 70 + 76")
    
    # Note that these updates occur not only in-place but cumulatively, meaning only the parameter to add need be referenced in the update function 
    
    # Similarly, a tool function could be defined to output the example_data set
    return_example_data <- function() {
      example_data <- data.frame(
        X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        Y = c(10, 13, 15, 18, 21, 20, 23, 27, 28, 30)
      )
      return(example_data)
    }
    
    ChatRBox_update(object = session, tools_list = list(return_example_data = return_example_data))
    
    
    # AI-Generated Summaries --------------------------------------------------
    
    # Since, example_data is now a service output, the $talk() argument summarize can be utilized. This is only for tool/API outputs
    # Setting summarize = TRUE employs the default summary prompt
    session$talk("Return my example data", summarize = TRUE)
    
    # Users may provide specific summary prompts per service using the summary_list argument
    ChatRBox_update(object = session, summary_list = list(return_example_data = "Output your summary in French"))
    session$talk("Return my example data", summarize = TRUE)
    
    # Note that the name matching logic involved in summary_list assignments enables distinct summary prompts per individual service, whilst they all adhere to the same overarching AI prompt
    
    
    # Environments ------------------------------------------------------------
    
    # Since updates are additive, the current R6 object environments contain all API services, data frames and tools added so far
    ls(session$chat_object@services$api_services) # This is the API services
    ls(session$chat_object@tools_env) # This is the tools environment
    ls(session$chat_object@data_env) # This is the data environment
    ls(session$chat_object@object_env) # This is the storage environment 
    
    # The storage environment object_env contains any past API service or tool output under a human-readable and AI-generated name. These names are passed to subsequent APIs during chained API calls and act as a universal identifier for past outputs, for both human and machine
    # Note that API services rather reside in nested environments to avoid naming conflicts 
    
    # Assigning either NULL or new.env() during update creates new environments
    ChatRBox_update(object = session, tools_env = NULL, data_env = NULL)
    
    # Unlike previously, environments are overwritten during update 
    # The original tools and data are no longer available
    ls(session$chat_object@tools_env)
    ls(session$chat_object@data_env)
    
    # To populate an updated environment, define the parameters to add as well
    ChatRBox_update(object = session, 
                    tools_env = NULL, 
                    data_env = NULL, 
                    tools_list = list(add_two_numbers = add),
                    data_list = list(example_data = example_data))
    
    # The addition tool and example data set are available again
    ls(session$chat_object@tools_env)
    ls(session$chat_object@data_env)
    
    
    # Prompting ---------------------------------------------------------------
    
    # AI instruction may be updated using prompt_template
    # This may reference service paths as {paths_string} and service arguments as {args_string}, which also update in-place given new API services and tool functions
    # The default prompt is written in ChatRBox_prompt.md in the inst/prompt folder and loaded into the R6 object upon initialization. This directs the AI to choose the most suitable service to answer an input question and populate the relevant client function accordingly.
    
    # However, prompt_template may also make no reference to API services or tools
    ChatRBox_update(object = session, prompt_template = "Don't include the letter 'E' in your responses")
    
    session$talk("Who was the first president of the USA?")
  }
}  
