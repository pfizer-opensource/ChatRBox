#' Normalizes httr2 Configuration List for Generated Client Functions 
#'
#' This function merges the user-supplied \code{httr2_config} chat object argument with the package defaults using the \pkg{utils} package. This enables single parameters within \code{httr2_config} to be updated independently of others during chatbot updates. 
#' @param httr2_config List. Named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings.
#' @return This function returns the modified list of \pkg{httr2} arguments for client function generation. The returned list acts as the input to \code{\link{apply_httr2_config}} which sets user preferences within client functions. Request behavior is built once per API service, meaning all endpoint-specific client functions share these settings. Hence, updating \pkg{httr2} arguments using \code{\link{ChatRBox_update}} re-generates client functions for APIs provided using \code{openapi_list}.    
#' @details
#' There are two methods to supply API services to ChatRBox chatbots: using API URLs via \code{services_list} or using OpenAPI schema URLs via \code{openapi_list}. The former method requires the inclusion of client function source code beneath the \code{client_fns} endpoint, which is extracted and downloaded. Therefore, the ability to assign and update \pkg{httr2} arguments using the ChatRBox package is exclusive to APIs supplied using \code{openapi_list}. These APIs do not contain client function source code, meaning client functions are automatically generated using the \pkg{httr2} request arguments provided by \code{httr2_config} upon chatbot initialization. It is assumed that similar user-control of HTTP requests may be achieved using \code{services_list} APIs by directly editing the \code{client_fns} endpoint. Note that the \code{httr2_config} list may contain any \pkg{httr2} settings but only \code{timeout}, \code{retry} and \code{verbose} are currently facilitated by \code{apply_httr2_config} and \code{format_httr2_config}. 
#' @importFrom utils modifyList
#' @seealso \code{\link{apply_httr2_config}}, \code{\link{format_httr2_config}}, \code{\link{ChatRBox_update}}
#' @example man/examples/examples_request_parameters.R
#' @export 
normalize_httr2_config <- function(httr2_config = list()) {
  
  if (is.null(httr2_config)) {
    httr2_config <- list()
  }
  
  if (!is.list(httr2_config)) {
    stop("httr2_config must be a list")
  }
  
  # Default httr2 settings
  defaults <- list(
    timeout = 30,
    retry = list(max_tries = 2),
    verbose = FALSE
  )
  
  # Modify default list with user preferences
  utils::modifyList(defaults, httr2_config)
}

#' Applies httr2 Configuration List to API Requests for Generated Client Functions 
#'
#' This function is used within \code{\link{build_api_client}} to apply the normalized \pkg{httr2} parameters to generated client functions.  
#' @param req A \pkg{httr2} request object to configure with normalized parameters. Required.
#' @param httr2_config A named list of user-specified \pkg{httr2} request options, outputted by \code{\link{normalize_httr2_config}}. Therefore, this list results from the merge of user-assigned values and default options for \code{timeout}, \code{retry} and \code{verbose}. Currently, only these \pkg{httr2} parameters may be customized. Defaults to an empty list. 
#' @return This function returns the inputted \pkg{httr2} request object with the \pkg{httr2} configuration settings applied. Hence, if \code{httr2_config} is empty or \code{NULL}, this function will return an unchanged request object.
#' @details
#' This function is used within \code{\link{build_api_client}} to assign the \pkg{httr2} request preferences within generated client functions. Firstly, the user-supplied \pkg{httr2} options are normalized using \code{\link{normalize_httr2_config}}, which combines them with default settings such that users may only define the parameters they wish to differ from default options. Next a \pkg{httr2} request body is established before this function applies the normalized \code{httr2_config} list to this object. Users are advised to iteratively adjust these \pkg{httr2} parameters if experiencing timeout, retry or access issues with supplied APIs, which may be facilitated without chatbot re-initialization. Note that these functions are only used within the generated client function workflow, associated with APIs provided via \code{openapi_list}. Similar request parameter control may be achieved for APIs provided via \code{services_list} by manually editing the \code{client_fns} endpoint.            
#' @importFrom httr2 req_timeout req_retry req_verbose
#' @seealso \code{\link{normalize_httr2_config}}, \code{\link{format_httr2_config}}, \code{\link{build_api_client}}, \code{\link{ChatRBox_update}} 
#' @example man/examples/examples_request_parameters.R 
#' @export 
apply_httr2_config <- function(req, 
                               httr2_config = list()) {
  
  if (is.null(httr2_config) || length(httr2_config) == 0) {
    return(req)
  }
  
  # Applies timeout assignment to request object
  if (!is.null(httr2_config$timeout) && !is.na(httr2_config$timeout)) {
    req <- httr2::req_timeout(req, seconds = httr2_config$timeout)
  }
  
  # Assigns retry assignment to request object
  if (!is.null(httr2_config$retry) && length(httr2_config$retry) > 0) {
    retry_args <- c(list(req = req), httr2_config$retry)
    req <- do.call(httr2::req_retry, retry_args)
  }
  
  # Assigns verbose assignment to request object
  if (isTRUE(httr2_config$verbose)) {
    req <- httr2::req_verbose(req)
  }
  
  req
}

#' Formats httr2 Configuration Lists for Display in Dynamic Error Message
#'
#' This function renders the normalized \code{httr2_config} list into a concise, human-readable, multi-line string which describes each request option used within generated client functions.  
#' @param httr2_config List. A named list of user-specified \pkg{httr2} request options, outputted by \code{\link{normalize_httr2_config}}. These are the \pkg{httr2} parameters that were used during client function generation by \code{\link{apply_httr2_config}}. Defaults to empty list.  
#' @return This function returns a single character string summarizing the \pkg{httr2} configuration used within generated client functions. The character string 'No httr2 request configuration was applied' outputs for empty or \code{NULL} \code{httr2_config} assignments. However, this enables this function to be used in isolation, since during client function generation, an empty \code{httr2_config} assignment leads to default values as defined in \code{\link{normalize_httr2_config}}. Therefore, dynamic error messages for failed API requests will always include a formatted configuration list for generated client functions.              
#' @details
#' Whilst this function utilizes the output of \code{\link{normalize_httr2_config}}, it is not involved in the client generation workflow but rather used within \code{\link{llm_api_result}}. \code{\link{llm_api_result}} acts as a master function within ChatRBox that extracts the AI-outputted responses following user questions and uses these to perform relevant API requests. API requests are performed using \code{do.call()} calls on extracted client paths, whereby an error message is revealed for any failed \code{do.call()}. This error message is dynamic and reveals the attempted client path and parameters, API HTTP error, \pkg{httr2} configuration used by the client function and suggested next steps. Therefore, this function formats the \pkg{httr2} configuration list for the ChatRBox dynamic error message. Hence, users may view these alongside the associated API HTTP error and adjust \pkg{httr2} parameters accordingly using \code{\link{ChatRBox_update}}.
#' @seealso \code{\link{llm_api_result}}, \code{\link{normalize_httr2_config}}, \code{\link{apply_httr2_config}}
#' @example man/examples/examples_request_parameters.R
#' @export
format_httr2_config <- function(httr2_config = list()) {
  
  if (is.null(httr2_config) || length(httr2_config) == 0) {
    return("No httr2 request configuration was applied.")
  }
  
  parts <- character()
  
  if (!is.null(httr2_config$timeout) && !is.na(httr2_config$timeout)) {
    parts <- c(parts, sprintf("- timeout: %s seconds", httr2_config$timeout))
  }
  
  if (!is.null(httr2_config$retry) && length(httr2_config$retry) > 0) {
    retry_txt <- paste(
      paste(names(httr2_config$retry), unlist(httr2_config$retry), sep = " = "),
      collapse = ", "
    )
    parts <- c(parts, sprintf("- retry: %s", retry_txt))
  }
  
  if (!is.null(httr2_config$verbose)) {
    parts <- c(parts, sprintf("- verbose: %s", httr2_config$verbose))
  }
  
  if (length(parts) == 0) {
    return("No httr2 request configuration was applied.")
  }
  
  paste(parts, collapse = "\n")
}
