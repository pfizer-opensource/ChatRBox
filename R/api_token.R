#' Constructs a httr2 API Request with Authentication
#' 
#' This function takes user HTTP request preferences to construct a \pkg{httr2} request body to facilitate API token usage.
#' 
#' @param base_url Character. Base API URL (without trailing slash). Defaults to \code{NULL}.
#' @param path Character. The path to the relevant endpoint. Defaults to \code{NULL}.
#' @param token Character. Optional API authentication token. Defaults to \code{NULL}.
#' @param auth_scheme Character. API authorization scheme. Defaults to \code{NULL}, given \code{token_header} is populated. 
#' @param token_header Character. API token header name if \code{auth_scheme} is \code{NULL}. Defaults to \code{NULL}.
#' @param timeout Numeric. Timeout value. Defaults to 60 seconds.
#' @param method Character. API HTTP method. Defaults to "GET" for likely chatbot API services.
#' @param verbose Logical. Whether to print verbose information. Defaults to FALSE.
#' @return
#' A \code{httr2_request} object representing an HTTP request, with the method, URL path, headers, timeout, and authentication applied as specified by the function arguments.
#' This request object can be executed with \code{httr2::req_perform()} to make the API call and is used within \code{\link{get_api_client_fns}} to extract external API client functions for chatbot access.  
#' @details
#' This function acts as a helper within \code{\link{get_api_client_fns}} to extract client function R source code from the API endpoint \code{client_fns}. These have been separated to optimize API token handling, whereby tokens and authorization methods may be chosen to access protected APIs.  
#' @seealso \code{\link{get_api_client_fns}}
#' @importFrom httr2 request req_url_path_append req_method req_timeout req_auth_bearer_token req_headers req_verbose
#' @importFrom stats setNames
#' @importFrom cli cli_alert_info
#' @examples
#' # Constructs request body with fictitious inputs
#' req <- make_req(
#'  base_url = "https://api.example.com",
#'  path = "example/path",
#'  token = "FAKE_TOKEN",
#'  auth_scheme = "Bearer",
#'  token_header = "Authorization")
#'  
#' req
#' @export
make_req <- function(
    base_url = NULL,
    path = NULL,
    token = NULL,
    auth_scheme = NULL,
    token_header = NULL,  
    timeout = 60,
    method = "GET",
    verbose = FALSE) {
  
  if (is.null(timeout)) {
    if (method == "GET") {
      if (grepl("\\.(zip|tar|gz|rds|csv|xlsx?)$", path, ignore.case = TRUE) ||
          grepl("/download/|/file/", path)) {
        timeout <- 300 
      } else {
        timeout <- 60   
      }
    } else {
      timeout <- 30
    }
  }
  if (verbose) {
    cli::cli_alert_info("make_req: method = {method}, timeout = {timeout} sec for {basename(path)}")
  }
  req <- httr2::request(base_url = base_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_method(method = method) |>
    httr2::req_timeout(seconds = timeout)
  
  if (!is.null(token) && !is.character(token)) {
    stop("token must be a character vector or NULL")
  }
  
  if (!is.null(token) && nzchar(token)) {
    if (tolower(token_header) == "authorization") {
      scheme <- tolower(auth_scheme)
      if (scheme == "bearer") {
        req <- httr2::req_auth_bearer_token(req, token)
      } else if (scheme == "key") {
        req <- httr2::req_headers(req, Authorization = paste("Key", token))
      } else {
        req <- httr2::req_headers(req, Authorization = paste(auth_scheme, token))
      }
    } else {
      req <- httr2::req_headers(req, !!!stats::setNames(list(token), token_header))
    }
  }
  if (verbose) req <- httr2::req_verbose(req)
  return(req)
}

#' Validates that Names of an Argument Match Entries in a Target List
#'
#' Checks that all names in a provided named list or vector are present in the names of another reference list. Throws an error listing all unmatched names if any are not found in the reference.
#'
#' @param name_list Named list or vector. The list whose names are being validated. Required.
#' @param main_list Named list or vector. The reference list whose names must be matched. Required.
#' @param arg_label Character. The label (argument name) to display in error messages for \code{name_list}. Required.
#' @param target_label Character. The label (argument name) to display in error messages for \code{main_list}. Required.
#' @return 
#' This function is called for its side effect and will throw an error if non-matching names are found.
#' @details
#' This function is an internal checker on the validity of inputted API tokens. Tokens and token headers are assigned to the corresponding API service URL in \code{\link{get_api_client_env}} or OpenAPI JSON schema URL in \code{\link{build_api_client_env}}, based on name matching between named lists. Token and header names must be present, identical to an inputted URL and unique. This function checks that they are identical to an inputted URL. The remaining checks are conducted using \code{\link{check_names}} and \code{\link{check_duplicate_names}}. Similar name matching logic is used to assign summary prompts to given API services, within \code{\link{process_summary_list}}. Summary logic is documented in \code{chatrbox_ai_services.R}, as this workflow is triggered when \code{summarize} is set to \code{TRUE} within ChatRBox \code{talk} calls.    
#' @seealso \code{\link{get_api_client_env}}, \code{\link{build_api_client_env}}, \code{\link{check_names}}, \code{\link{check_duplicate_names}} 
#' @example man/examples/examples_name_match.R       
#' @export
validate_name_match <- function(name_list, 
                                main_list, 
                                arg_label, 
                                target_label) {
  
  if (!is.null(name_list) && length(name_list) > 0 && !is.null(names(name_list))) {
    unmatched <- setdiff(names(name_list), names(main_list))
    if (length(unmatched) > 0) {
      stop(sprintf(
        "The following names in '%s' do not match any entry in '%s': %s",
        arg_label,
        target_label,
        paste(shQuote(unmatched), collapse = ", ")
      ), call. = FALSE)
    }
  }
}

#' Ensures Argument is a Named List or Vector
#'
#' Checks that the provided argument is named. Throws an error if any elements are not named.
#' @param arg List or vector. The object to check for names. Required.
#' @param arg_label Character. The label (argument name) to display in error messages for \code{arg}. Required.
#' @param input_label Character. The label (argument name) to display in error messages for the expected input target. Required.
#' @return 
#' This function is called for its side effect and will throw an error if \code{arg} is not a named list or vector.
#' @details
#' This function is an internal checker on the validity of inputted API tokens. Tokens and token headers are assigned to the corresponding API service URL in \code{\link{get_api_client_env}} or OpenAPI JSON schema URL in \code{\link{build_api_client_env}}, based on name matching between named lists. Token and header names must be present, identical to an inputted URL and unique. This function checks that they are present. The remaining checks are conducted using \code{\link{validate_name_match}} and \code{\link{check_duplicate_names}}. Similar name matching logic is used to assign summary prompts to given API services, within \code{\link{process_summary_list}}. Summary logic is documented in \code{chatrbox_ai_services.R}, as this workflow is triggered when \code{summarize} is set to \code{TRUE} within ChatRBox \code{talk} calls.
#' @seealso \code{\link{get_api_client_env}}, \code{\link{build_api_client_env}}, \code{\link{validate_name_match}}, \code{\link{check_duplicate_names}}
#' @example man/examples/examples_name_match.R 
#' @export
check_names <- function(arg, 
                        arg_label, 
                        input_label) {
  
  if (!is.null(arg) && length(arg) > 0 && is.null(names(arg))) {
    stop(sprintf(
      "All named entries in '%s' must match a name in '%s'.",
      arg_label, input_label
    ), call. = FALSE)
  }
}

#' Ensures All Names in an Argument are Unique
#'
#' Checks that all names in the provided argument (list or vector) are unique. Throws an error if any duplicate names are found.
#' @param arg List or vector. The object to check for duplicate names. Required.
#' @param arg_label Character. The label (argument name) to display in error messages for \code{arg}. Required.
#' @return 
#' This function is called for its side effect and will throw an error if \code{arg} contains duplicate names.
#' @details
#' This function is an internal checker on the validity of inputted API tokens. Tokens and token headers are assigned to the corresponding API service URL in \code{\link{get_api_client_env}} or OpenAPI JSON schema URL in \code{\link{build_api_client_env}}, based on name matching between named lists. Token and header names must be present, identical to an inputted URL and unique. This function checks that they are unique. The remaining checks are conducted using \code{\link{validate_name_match}} and \code{\link{check_names}}. Similar name matching logic is used to assign summary prompts to given API services, within \code{\link{process_summary_list}}. Summary logic is documented in \code{chatrbox_ai_services.R}, as this workflow is triggered when \code{summarize} is set to \code{TRUE} within ChatRBox \code{talk} calls.
#' @seealso \code{\link{get_api_client_env}}, \code{\link{build_api_client_env}}, \code{\link{check_names}}, \code{\link{validate_name_match}}
#' @example man/examples/examples_name_match.R 
#' @export
check_duplicate_names <- function(arg, 
                                  arg_label) {
  
  nms <- names(arg)
  dups <- nms[duplicated(nms) & !is.na(nms)]
  if (length(dups) > 0) {
    stop(sprintf(
      "Duplicate names detected in '%s': %s.\nEach item must have a unique name.",
      arg_label,
      paste(shQuote(unique(dups)), collapse = ", ")
    ), call. = FALSE)
  }
}
