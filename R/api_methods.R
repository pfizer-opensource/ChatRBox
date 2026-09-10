#' Starts the Local Plotting API Service 
#' 
#' This function starts a local \pkg{plumber2} API service which may be provided to chatbots for linear plotting. This is mainly utilized for example and instruction scripts.
#' 
#' @param port Port. Port to launch API. Defaults to 8000
#' @param host Host. Host for API. Defaults to "127.0.0.1"
#' @return A \pkg{callr} process handle. Use \code{$kill()} to stop the API service.
#' @details
#' This function is used to locally start a linear plotting API service, to demonstrate the process by which users may supply AI chatbots with remote \pkg{plumber2} APIs. \pkg{plumber2} is the successor to \pkg{plumber} and adds asynchronous request handling. The service is firstly initiated using \code{proc <- ChatRBox::start_api_service()}, followed by \code{Sys.sleep(2)} to provide sufficient time and finally \code{proc$kill()} to disable the service. See \code{examples.R} in the examples folder and the ChatRBox vignette for further detail. 
#' @example man/examples/examples_api_service.R
#' @export
start_api_service <- function(port = 8000, 
                              host = "127.0.0.1") {
  
  api_file <- system.file("api_service", "plumber.R", package = "ChatRBox")
  if (api_file == "") stop("Local API not found")
  
  proc <- callr::r_bg(function(api_file, port, host) {
    pr <- plumber2::api(api_file)
    plumber2::api_run(pr, host = host, port = port)
  }, args = list(api_file = api_file, port = port, host = host))
  
  invisible(proc)
}

#' Stores Config File Values in Keyring
#'
#' Reads a YAML config file and adds each entry to the keyring under the specified service. 
#' @param config_path File path. Path to YAML config file. Defaults to \code{NULL}. Users are advised to name their YAML config file "ChatRBox_config.yml", stored in their home directory.
#' @param service Character. Name of keyring service. Defaults to "ChatRBox".
#' @return This function includes an invisible return as it stores all environment variables listed in the inputted config file into the keyring.
#' @details
#' \code{\link{start_api_service}} uses the \pkg{callr} function \code{r_bg}. This contains a background R process that temporarily copies user .Renviron files. Hence, users are advised to create a config file containing necessary environment variables (such as \pkg{ellmer} base URLs) and store these with \pkg{keyring}. This function facilitates this, whereby variables can be loaded using \code{\link{get_keyring}}. Note that user .Renviron files are only copied into the R session's temporary directory, such that \code{callr::r_bg()} is safe and adheres to file operation guidance.   
#' @importFrom yaml read_yaml
#' @importFrom keyring default_backend key_set_with_value
#' @examples
#' # Store config values with keyring, retrieve during initialization
#' \dontrun{
#' store_keyring(config_path = "~/ChatRBox_config.yml")
#'   chat <- ellmer::chat_ollama(base_url = get_keyring("OLLAMA_HOST"),
#'                               model    = get_keyring("OLLAMA_MODEL"))
#' }
#' @export 
store_keyring <- function(config_path = NULL, 
                          service = "ChatRBox") {
  
  config_path <- path.expand(config_path)
  if (!file.exists(config_path)) stop("Config file not found: ", config_path)
  config <- yaml::read_yaml(config_path, 
                            eval.expr = FALSE)
  
  # Recursive function to handle both nested and non-nested configurations
  store_keys <- function(config, parent_key = NULL, service) {
    for (var in names(config)) {
      full_key <- if(is.null(parent_key)) var else paste(parent_key, var, sep = ".")
      if(is.list(config[[var]])) {
        store_keys(config[[var]], full_key, service)
      } else {
        tryCatch(
          {
            keyring::key_set_with_value(
              service = service, 
              username = full_key, 
              password = as.character(config[[var]])
            )
            cat("Successfully stored key:", full_key, "\n")
          },
          error = function(e) {
            warning(sprintf("Could not set key for %s: %s", full_key, e$message))
          }
        )
      }
    }
  }
  
  # Call the recursive key storage function
  store_keys(config, service = service)
  
  invisible(TRUE)
}

#' Retrieves Values From Keyring
#'
#' This function retrieves values from keyring using the environment variable name and service name. This is used to load environment variables (such as \pkg{ellmer} base URLs) without the use of a .Renviron file.
#' @param var Variable name. Required.
#' @param service Keyring service. Defaults to "ChatRBox", in accordance with \code{\link{store_keyring}} default values. 
#' @return The retrieved value as a string.
#' @details
#' This function works with \code{\link{store_keyring}} to load stored user credentials within \code{\link{start_api_service}}, consistent with \pkg{callr} recommendations. This function uses the \pkg{callr} function \code{r_bg} to locally start the example API service, but temporarily makes a copy of user .Renviron files during this background process. Users are advised to store environmental variables in a YAML file named 'ChatRBox_config.yml', located in the home directory. The \code{examples.R} script in man/examples and the ChatRBox vignette include potential structures for this configuration file, with equivalent functionality for nested and non-nested variable retrieval. 
#' 
#' If users do not contain sensitive information in their .Renviron files or do not locally run the example API service using \code{\link{start_api_service}}, the \pkg{callr} background process poses no threat. Note that user .Renviron files are only copied into the R session's temporary directory, such that \code{callr::r_bg()} is safe and adheres to file operation guidance.
#' @importFrom keyring key_get
#' @examples
#' # Store config values with keyring, retrieve during initialization
#' \dontrun{
#' store_keyring(config_path = "~/ChatRBox_config.yml")
#'   chat <- ellmer::chat_ollama(base_url = get_keyring("OLLAMA_HOST"),
#'                               model    = get_keyring("OLLAMA_MODEL"))
#' }
#' @export         
get_keyring <- function(var = NULL, 
                        service = "ChatRBox") {
  
  if(is.null(var)){
    stop("you must provide a keyring var")
  }
  
  # Splits variables for nested config files
  var_parts <- strsplit(var, "\\.")[[1]]
  
  tryCatch(
    {
      keyring::key_get(service = service, username = var)
    },
    error = function(e) {
      if(length(var_parts) > 1) {
        warning(paste("Attempting to retrieve nested key:", var))
        keyring::key_get(service = service, username = var)
      } else {
        stop(e)
      }
    }
  )
}

#' Loads API Headers from a Serialized Environment Variable
#' 
#' De-serializes JSON strings for use as named character vector arguments during chatbot initialization.
#' @param var Character. Name of the environment variable holding JSON headers. Defaults to "API_HEADERS".
#' @return A named character vector suitable for the \code{api_headers} argument of the \pkg{ellmer} function \code{chat_openai_compatible()}.
#' @details
#' ChatRBox is compatible with any \pkg{ellmer} AI provider but initializing the \code{chat_ollama()} and \code{chat_openai_compatible()} functions have been documented. The latter contains the \code{api_headers} argument for extra headers appended to each API call. This must be a named character vector. Hence, users are advised to store this vector in an environment file as a single serialized (JSON) string. Loading this file ensures this environment variable is present within user R sessions, and calling this function de-serializes the string into the named vector during chatbot initialization.  
#' @importFrom jsonlite fromJSON
#' @export
get_env_headers <- function(var = "API_HEADERS") {
  
  unlist(jsonlite::fromJSON(Sys.getenv(var)))
  
}