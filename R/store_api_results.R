#' Resolves Named References to Stored Objects in Client Function Arguments
#'
#' This function generalizes chained API service/tool calls so that the output of one service may be referenced by name and passed as input to another, regardless of the object type. It scans a list of client function arguments and, for any scalar character argument whose value matches the name of an object stored in either \code{object_env} or \code{data_env}, replaces that name with the stored object itself. This facilitates chained API calls using a single input query, since chatbots search \code{object_env} to pass previously outputted API responses into subsequent APIs within a single \code{$talk()} call. Stored names are AI-generated and human-readable, as instructed within the default ChatRBox prompt.
#'
#' @param arg_list List. Named list of client function arguments, as extracted from the LLM JSON response. Defaults to empty list.
#' @param data_env Environment. Environment containing uploaded data objects. Defaults to \code{NULL}, which is treated as an empty environment.
#' @param object_env Environment. Environment containing past API service/tool outputs held as named R objects of any type. This environment is read first, taking precedence over \code{data_env}. Defaults to \code{NULL}, which is treated as an empty environment.
#' @return The \code{arg_list} with any name references resolved to their corresponding stored objects. Arguments that do not reference a stored object are returned unchanged.
#' @details
#' This helper interprets \code{object_env} and \code{data_env} as stores of arbitrary input objects, so that any object type produced by an API service or tool (such as a JSON object parsed into an R list, narrative text, a numeric vector or a single value) may be referenced by name and passed to a subsequent service within a single input query. This facilitates chained API calls which may enable more complex ChatRBox workflows.
#'
#' Only scalar, non-empty character arguments are considered candidate references, since these are the values the AI emits when referring to a previously named output. Resolution is performed with \code{inherits = FALSE} so that only objects explicitly registered in \code{object_env} or \code{data_env} are substituted. Therefore, arbitrary objects from parent or global environments are never silently resolved. The chatbot is instructed to create unique human-readable names per service response. Whilst unlikely, name duplication may occur since users rather than chatbots name objects within the data environment. Hence, the dedicated object environment is checked first, then the data environment, so that the most recently produced output takes precedence when a name is present in more than one store. Values that do not match a stored object are left untouched, preserving literal string arguments.
#' @seealso \code{\link{llm_api_result}}
#' @example man/examples/examples_output_storage.R
#' @export
resolve_arg_refs <- function(arg_list = list(),
                             data_env = NULL,
                             object_env = NULL) {
  
  resolve_one <- function(x) {
    is_ref <- isTRUE(
      is.character(x) &&
        length(x) == 1L &&
        nzchar(x)
    )
    
    # object_env is checked first
    if (is_ref) {
      if (!is.null(object_env) &&
          exists(x, envir = object_env, inherits = FALSE)) {
        return(get(x, envir = object_env, inherits = FALSE))
      }
      
      # data_env is checked second
      if (!is.null(data_env) &&
          exists(x, envir = data_env, inherits = FALSE)) {
        return(get(x, envir = data_env, inherits = FALSE))
      }
      
      return(x)
    }
    
    if (is.list(x)) {
      return(lapply(x, resolve_one))
    }
    
    x
  }
  
  if (length(arg_list) == 0L) {
    return(arg_list)
  }
  
  lapply(arg_list, resolve_one)
}

#' Normalizes AI-Generated API Result Names
#'
#' This function ensures robustness in AI-generated naming by constructing a default name which avoids name clashes, rejecting structurally invalid names, trimming whitespace and rejecting empty names.
#'
#' @param name Character. AI-generated and human-readable name for a given service output. Required.
#' @param index Integer. Position of the API call in the current batch, used for fallback naming. Defaults to 1L.
#'
#' @return This function returns a character scalar for API response naming that is suitable for subsequent workflows, as a safeguard against AI hallucinations. 
#' @details
#' ChatRBox AI chatbots are instructed to assign every service output (both API and tool responses) a human-readable name. This accompanies responses during output in a named list, acts as a common identifier, whereby both users and chatbots may refer to past outputs by name and enables chained API calls since chatbots may pass these names between API services to represent stored objects. Therefore, valid naming is AI-controlled yet crucial to successful subsequent workflows. Hence, this function is used within \code{\link{llm_api_result}} to ensure that AI-generated names are valid for later R processes. Names must be character scalars without trailing spaces, whereby fallback names are used for any inconsistencies following AI hallucination.  
#' @seealso \code{\link{llm_api_result}}
#' @example man/examples/examples_output_storage.R
#' @export
normalize_result_name <- function(name, 
                                  index = 1L) {
  
  # Fallback names differ in numbering for invalid AI naming
  fallback <- paste0("api_result_", index)
  
  # Must be a scalar character without missing values
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    return(fallback)
  }
  
  # Trim name of whitespace
  name <- trimws(name)
  
  # Return acceptable AI-generated or fallback name for future workflows
  if (!nzchar(name)) {
    return(fallback)
  }
  
  name
}

#' Stores API Results in the Object Environment
#'
#' This function stores original API results in \code{object_env} under their AI-assigned and human-readable names. These results are stored unchanged so that later chained API calls can reuse the exact R object returned by the previous call.
#' @param result API output. Object returned by an API client function. Required.
#' @param name Character. Normalized chatbot-assigned result name. Required.
#' @param object_env Environment. Environment used for chatbot memory and chained API calls. Required.
#' @return This function invisibly returns \code{TRUE} if a service result is suitable for \code{object_env} storage and stored. This function returns \code{FALSE} otherwise, which reflects that a service output has not been stored within \code{object_env}.
#' @details
#' This function facilitates API service/tool output storage in \code{object_env} which enables chained API calls since chatbots may refer to past responses as further API arguments using their human-readable names. This passes the stored R objects that these names represent, such that API chaining may occur using a single input query.
#' @seealso \code{\link{normalize_result_name}}, \code{\link{llm_api_result}}  
#' @example man/examples/examples_output_storage.R 
#' @export
store_api_result <- function(result, 
                             name, 
                             object_env) {
  
  if (!is.environment(object_env)) {
    stop("object_env must be an environment.")
  }
  
  # Validate name
  if (!is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)) {
    return(invisible(FALSE))
  }
  
  # Store everything (including NULL and raw bytes)
  # For rendered images raw bytes are stored in object_env and placeholders 
  # are stored in past_outputs 
  assign(name, result, envir = object_env)
  invisible(TRUE)
}
