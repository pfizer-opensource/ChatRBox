#' Extracts HTTP Status Code from Error Conditions 
#' 
#' This function extracts the HTTP status code from failed API calls to present, alongside suggested next steps, in the ChatRBox dynamic error message.
#' 
#' @param e Error condition. An HTTP request error condition to extract the status code from. Required.
#' @return This function returns the extracted HTTP error status code as an integer. If unavailable, this function returns \code{NA_integer_}. 
#' @details
#' The dynamic error message within \code{\link{llm_api_result}} outputs for any failed \code{do.call()}, populated with the attempted client path and parameters, API HTTP error, \pkg{httr2} configuration used by the client function and suggested next steps. This acknowledges that multiple components under various controls may contribute towards a failed \code{do.call()}. For example, if using external API services with generated client functions, failures may arise from access issues or timeouts. Whilst the chatbot prompt is precise, incorrect client paths or parameters may arise from AI hallucinations. Therefore, the dynamic error message presents users with maximum information such that they may trouble-shoot failed \code{do.call()} calls by adjusting what they deem the most necessary. This function extracts the API service HTTP error which is not only presented in this error message, but used to print the relevant suggested improvements using \code{\link{http_status_guidance}}. Lastly, this function was designed to be as robust as possible, whereby it attempts to extract error codes from multiple different structures like nested lists and from within character warnings. In the absence of valid extraction using these methods, this function returns \code{NA}.   
#' @seealso \code{\link{llm_api_result}}, \code{\link{http_status_guidance}} 
#' @example man/examples/examples_error_message.R
#' @export
http_status_from_condition <- function(e) {
  
  # This function locates error codes in nested structures
  find_status_code <- function(obj) {
    if (!is.list(obj)) return(NULL)
    
    # Potential status code field names
    status_fields <- c(
      "status", "status_code", "statusCode", "code", 
      "httpStatus", "http_status", "error_code"
    )
    
    # Direct field check
    for (field in status_fields) {
      if (field %in% names(obj)) {
        status <- obj[[field]]
        if (is.numeric(status) && status >= 100 && status <= 599) {
          return(as.integer(status))
        }
      }
    }
    
    # Recursive search in nested lists
    for (item in obj) {
      if (is.list(item)) {
        status <- find_status_code(item)
        if (!is.null(status)) return(status)
      }
    }
    
    NULL
  }
  
  # Extracts status code
  status <- find_status_code(e)
  
  # Fallback 
  if (is.null(status)) {
    # Extract status from message
    msg <- tryCatch(
      if ("message" %in% names(e)) e$message else conditionMessage(e), 
      error = function(err) ""
    )
    match <- regmatches(
      msg,
      regexpr("\\b([1-5][0-9]{2})\\b", msg)
    )
    
    if (length(match) == 1L && nzchar(match)) {
      status <- as.integer(match)
    }
  }
  
  # Return status or NA
  if (!is.null(status)) {
    return(status)
  }
  
  NA_integer_
}

#' Returns User Advice per HTTP Status Code 
#' 
#' This function matches inputted HTTP integer status codes to string advice blocks. These strings suggest next steps for users to resolve failed \code{do.call()} calls, and are appended to the ChatRBox dynamic error message. 
#'
#' @param status Integer. Integer HTTP status code extracted using \code{\link{http_status_from_condition}}. Required.
#' @return This function returns a character string specific to each HTTP status code, with advice on how users may resolve failed \code{do.call()} calls. Each advice block is presented below the HTTP status code within the dynamic error message and specific to ChatRBox workflows. If no status code is inputted, this function returns an empty string. 
#' @seealso \code{\link{http_status_guidance}}
#' @example man/examples/examples_error_message.R 
#' @export
http_status_guidance <- function(status) {
  
  if (is.na(status)) {
    return("")
  }
  
  switch(
    as.character(status),
    
    "400" = paste0(
      "Suggested fix for HTTP 400 Bad Request:\n",
      "The API rejected the request as invalid. Check that the generated ",
      "arguments match the endpoint's expected names, types, and request ",
      "format. If the endpoint expects JSON, confirm that the OpenAPI schema ",
      "declares a requestBody and that the body is valid JSON."
    ),
    
    "401" = paste0(
      "Suggested fix for HTTP 401 Unauthorized:\n",
      "Check that the required API token, key, username/password, or other ",
      "credentials are present and valid. Also confirm that ChatRBox is passing ",
      "the credentials using the API's expected authentication method, such as ",
      "an Authorization header, bearer token, API-key header, or query token. ",
      "These parameters may be adjusted by updating the chatbot."
    ),
    
    "403" = paste0(
      "Suggested fix for HTTP 403 Forbidden:\n",
      "Authentication may have succeeded, but the authenticated user or token ",
      "does not have permission to access this endpoint. Check API permissions, ",
      "token scopes, organization/project access, IP restrictions, and whether ",
      "the endpoint is enabled for your account."
    ),
    
    "404" = paste0(
      "Suggested fix for HTTP 404 Not Found:\n",
      "Check that the API base URL or OpenAPI schema URL are correct. ",
      "If this is a local plumber2/API service, confirm ",
      "that it is running on the expected host and port."
    ),
    
    "405" = paste0(
      "Suggested fix for HTTP 405 Method Not Allowed:\n",
      "The endpoint exists, but the HTTP method is likely wrong. Check whether ",
      "the API expects GET, POST, PUT, PATCH, or DELETE, and confirm that the ",
      "OpenAPI schema declares the correct method."
    ),
    
    "408" = paste0(
      "Suggested fix for HTTP 408 Request Timeout:\n",
      "The server timed out waiting for the request. Try again, increase the ",
      "httr2 timeout setting by updating the chatbot, reduce the payload size ", 
      "or check whether the API service is overloaded."
    ),
    
    "409" = paste0(
      "Suggested fix for HTTP 409 Conflict:\n",
      "The request conflicts with the current server state. Check whether the ",
      "resource already exists, whether the operation is valid in the current ",
      "state, or whether the API requires different input values."
    ),
    
    "413" = paste0(
      "Suggested fix for HTTP 413 Payload Too Large:\n",
      "The request body is larger than the server allows. Reduce the payload ",
      "size, send a smaller data subset, compress or upload data separately, ",
      "or increase the server/API payload limit if you control the API."
    ),
    
    "414" = paste0(
      "Suggested fix for HTTP 414 URI Too Long:\n",
      "The request URL became too long, usually because a large value such as ",
      "a JSON string, data frame, list, document, or other payload was sent as ",
      "a URL query parameter. Large or structured inputs should be sent in the ",
      "request body instead. For generated OpenAPI clients, update the API or ",
      "OpenAPI schema so the large input is declared as requestBody. For ",
      "provided client functions, use httr2::req_body_json() or another body ",
      "method instead of httr2::req_url_query() in the client_fns endpoint."
    ),
    
    "415" = paste0(
      "Suggested fix for HTTP 415 Unsupported Media Type:\n",
      "The API rejected the request content type. Check whether the endpoint ",
      "expects application/json, multipart/form-data, text/plain, raw bytes, ",
      "or another media type, and ensure the client uses the matching httr2 ",
      "body helper."
    ),
    
    "422" = paste0(
      "Suggested fix for HTTP 422 Unprocessable Content:\n",
      "The request reached the API and was syntactically valid, but failed API ",
      "validation. Check required fields, allowed values, object structure, ",
      "column names, data types, and whether the request body matches the ",
      "endpoint schema."
    ),
    
    "429" = paste0(
      "Suggested fix for HTTP 429 Too Many Requests:\n",
      "The API rate limit was exceeded. Wait before retrying, reduce request ",
      "frequency, configure retry/backoff settings, or check the API's rate ",
      "limit policy."
    ),
    
    "500" = paste0(
      "Suggested fix for HTTP 500 Internal Server Error:\n",
      "The API server encountered an internal error. If you control the API, ",
      "check the server logs for the failing endpoint. Otherwise, retry later ",
      "or contact the API provider with the request details."
    ),
    
    "502" = paste0(
      "Suggested fix for HTTP 502 Bad Gateway:\n",
      "A gateway or proxy received an invalid response from an upstream service. ",
      "Check whether the API service and any dependent services are running, ",
      "then retry."
    ),
    
    "503" = paste0(
      "Suggested fix for HTTP 503 Service Unavailable:\n",
      "The API service may be down, overloaded, starting up, or temporarily ",
      "unavailable. Retry later, check service health, or increase retry/backoff ",
      "settings."
    ),
    
    "504" = paste0(
      "Suggested fix for HTTP 504 Gateway Timeout:\n",
      "An upstream service took too long to respond. Try again, reduce the size ",
      "or complexity of the request, increase timeout limits if possible, or ",
      "check the health of dependent services."
    ),
    
    ""
  )
}

#' Adds Extracted HTTP Status Guidance to Original API Error Message
#' 
#' This function appends the suggested guidance for a given HTTP status code, extracted by \code{\link{http_status_guidance}} to the original error message associated with the API service. Both are presented in the ChatRBox dynamic error message. 
#'
#' @param e Error condition. An HTTP request error condition to extract the status code from. Required.
#' @param api_error Character. The original API/client error message. Required.
#' @return This function returns a character string that contains the original API error message and the HTTP status code-specific guidance extracted by \code{\link{http_status_guidance}} as a character string, separated by a line. 
#' @details
#' This function combines the helper functions \code{\link{http_status_from_condition}} and \code{\link{http_status_guidance}} to determine the user-facing error message associated with HTTP status errors. The dynamic error message for any failed \code{do.call()} contains maximum information for user trouble-shooting, including chatbot-relevant parameters (chosen client paths and parameters), request-relevant parameters (\pkg{httr2} request assignments) and API-relevant parameters (HTTP status codes). This function is concerned with the API-relevant parameters and ensures that the original, simple, API error is presented alongside ChatRBox-specific advice for any HTTP status.  
#' @seealso \code{\link{http_status_from_condition}}, \code{\link{http_status_guidance}}, \code{\link{llm_api_result}}
#' @example man/examples/examples_error_message.R
#' @export
add_http_status_guidance <- function(e, 
                                     api_error) {
  
  # Extracts HTTP status code from error message
  status <- http_status_from_condition(e)
  
  # Assigns relevant guidance to status code
  guidance <- http_status_guidance(status)
  
  if (!nzchar(guidance)) {
    return(api_error)
  }
  
  paste0(
    api_error,
    "\n\n",
    guidance
  )
}
