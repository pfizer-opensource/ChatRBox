# Supported code block languages for extraction/removal.
# Extend this vector to support additional structured output formats.
# See CONTRIBUTING.md ('Adding a New Structured Output Language').
.SUPPORTED_CODE_LANGUAGES <- c("json", "display", "yaml", "yml", "tsv", "csv", "md")

#' @noRd
.code_block <- function(string = NULL,
                        language = NULL,
                        action = c("extract", "remove")) {
  
  if (any_is_empty(string)) stop("string is empty")
  if (any_is_empty(language)) stop("language is empty")
  
  action <- match.arg(action)
  
  if (identical(action, "extract")) {
    stringr::str_extract(
      string,
      glue::glue("(?s)(?<=```{language}\n)(.+?)(?=\n?```)"))
  } else {
    stringr::str_remove_all(
      string,
      glue::glue("(?s)```{language}(?=\\s)\\s?.+?```"))
  }
}

#' Redacts Sensitive Values from an Argument List for Safe Display
#'
#' This function replaces the values of any list elements whose names match known credential keys with the constant string \code{"KEY_REDACTED"}.
#'
#' @param x List. The list to sanitize of confidential credentials. This is typically \code{arg_list} for either supplied (via \code{services_list}) or generated (via \code{openapi_list}) client functions. Required.
#' @param sensitive Character. Vector of element names to redact. This is case-insensitive and defaults to a vector of common confidential names such as \code{token}, \code{api_key} and \code{auth_scheme}. This argument may be expanded to encompass further confidential arguments to replace.
#' @return This function returns a copy of \code{x} with sensitive values replaced by \code{"KEY_REDACTED"}. Notably, both non-nested and nested confidential arguments are replaced for safe error message display following various client function structures.
#' @details
#' Replaced values are listed within the function argument \code{sensitive} and include \code{token}, \code{api_key} and \code{auth_scheme}. This function is employed during dynamic error message building in \code{\link{llm_api_result}}, to inform users of the attempted service arguments that led to failed tool execution, without leaking secrets. The confidential argument names listed in \code{sensitive} may be updated or expanded by supplying a custom character vector to the \code{sensitive} argument, or by editing package source code. These defaults include the exact names of token arguments within the ChatRBox token framework.
#' @examples
#' # Default redaction replaces known credential keys
#' args <- list(
#'   token        = "MY_TOKEN",
#'   token_header = "Authorization")
#' redact_sensitive(args)
#'
#' # Nested credentials are also redacted
#' nested_args <- list(
#'   token_list = list(api_key = "MY_TOKEN"))
#' redact_sensitive(nested_args)
#'
#' @seealso \code{\link{llm_api_result}}
#' @export
redact_sensitive <- function(x,
                             sensitive = c("token", "token_header",
                                           "auth_scheme", "api_key", "apikey",
                                           "authorization", "client_secret",
                                           "client_id", "password", "secret",
                                           "bearer")) {
  if (!is.list(x)) {
    return(x)
  }
  nms <- names(x)
  for (i in seq_along(x)) {
    nm <- if (!is.null(nms)) nms[[i]] else ""
    if (!is.null(nm) && nzchar(nm) &&
        tolower(nm) %in% tolower(sensitive)) {
      x[[i]] <- "KEY_REDACTED"
    } else if (is.list(x[[i]])) {
      x[[i]] <- redact_sensitive(x[[i]], sensitive = sensitive)
    }
  }
  x
}

#' Extracts Code Block from Markdown String
#'
#' This function extracts the first code block of a specified language from a Markdown-formatted string. The extracted block excludes language tags and code fences. 
#'
#' @param string Character. The input string containing Markdown code block(s). Required.
#' @param language Character. The language tag of the code block. One of the supported tags in \code{.SUPPORTED_CODE_LANGUAGES} (\code{"json"}, \code{"display"}, \code{"yaml"}, \code{"yml"}, \code{"tsv"}, \code{"csv"}, \code{"md"}). This may be extended by adding to the vector assignment. See CONTRIBUTING.md for further detail. Defaults to "json".
#' @return The contents of the first extracted code block.  
#' @details
#' This function is used in \code{\link{llm_api_result}} to parse the LLM response. The ChatRBox AI response given the default prompt should contain both a JSON object and text output, if an API service/tool has been used. The JSON object is used to populate the chosen API service/tool function with parameters from the input question, whereas the explanation is for user interaction. Hence, these are employed differently in downstream workflows. 
#' 
#' This function separates the JSON object and outputs the contained client function arguments. The equivalent function for obtaining the text output is \code{\link{code_remove}}.   
#' @seealso \code{\link{llm_api_result}}, \code{\link{code_remove}}
#' @importFrom stringr str_extract
#' @importFrom glue glue
#' @example man/examples/examples_parse.R
#' @export
code_extract <- function(string = NULL,
                         language = "json") {
  
  if (any_is_empty(string)) stop("string is empty")
  
  if (any_is_empty(language)) stop("language is empty")
  
  language <- match.arg(arg = language, choices = .SUPPORTED_CODE_LANGUAGES)
  
  .code_block(string = string, language = language, action = "extract")
}

#' Removes Code Block from Markdown String
#'
#' This function removes all code blocks of a specified language from a Markdown-formatted string. The string is returned excluding code blocks and code fences. 
#'
#' @param string Character. The input string containing Markdown code block(s). Required.
#' @param language Character. The language tag of the code block. One of the supported tags in \code{.SUPPORTED_CODE_LANGUAGES} (\code{"json"}, \code{"display"}, \code{"yaml"}, \code{"yml"}, \code{"tsv"}, \code{"csv"}, \code{"md"}). This may be extended by adding to the vector assignment. See CONTRIBUTING.md for further detail. Defaults to "json".
#' @return A character string with specified code blocks and fences removed.  
#' @details
#' This function is used in \code{\link{llm_api_result}} to parse text and code components from the LLM response. The ChatRBox AI response given the default prompt should contain both a JSON object and plain text output, if an API service/tool has been used. The JSON object is used to populate the chosen API service/tool function with parameters from the input question, whereas the explanation is for user interaction. Hence, these are employed differently in downstream workflows. 
#' 
#' This function separates the text and outputs this as a character string. The equivalent function for obtaining the code output is \code{\link{code_extract}}.  
#' @seealso \code{\link{llm_api_result}}, \code{\link{code_extract}}
#' @importFrom stringr str_remove_all
#' @importFrom glue glue
#' @example man/examples/examples_parse.R
#' @export
code_remove <- function(string = NULL,
                        language = "json") {
  
  if (any_is_empty(string)) stop("string is empty")
  
  if (any_is_empty(language)) stop("language is empty")
  
  language <- match.arg(arg = language, choices = .SUPPORTED_CODE_LANGUAGES)
  
  .code_block(string = string, language = language, action = "remove")
}

#' Returns Second Value from JSON Object 
#' 
#' This function returns the value from the second key-value pair in a JSON object, thought to correspond to API/tool service names for ChatRBox name-matching workflows. These are involved in applying AI-generated summary prompts and token assignments to the correct services.
#' 
#' @param json_str Character. The input JSON object as a string. Defaults to empty string leading to \code{NA} output.
#' @return The value corresponding to the second key-value pair in the inputted
#' JSON object.
#' @details
#' The default prompt instructs the AI to assign a human-readable name to the value of the second key-value pair in its JSON output. The key for this pair is always \code{name}. Retrieving the value via positioning rather than referring to \code{name} was preferred, in case later arguments are also called \code{name}. This function is used when adding past API service/tool function outputs to the object environment to inform further AI prompting. Therefore, the chatbot can access API results to summarize or manipulate them further.
#' @importFrom jsonlite fromJSON
#' @example man/examples/examples_parse.R
#' @export
get_name <- function(json_str = "") {
  if(any_is_empty(json_str)){
    return(NA)
  }
  obj <- jsonlite::fromJSON(json_str)
  keys <- names(obj)
  if (length(keys) >= 2) {
    return(obj[[keys[2]]])
  } else {
    return(NA)
  }
}

#' Returns Contents of Environment as a Character String
#' 
#' This function is used to provide chatbots with access to \code{data_env} and \code{object_env} contents via AI prompt interpolation. The AI prompt not only provides chatbots with behavioral instructions but dynamically updates with service paths (encapsulated within \code{paths_string} and \code{args_string}) and API/tool outputs (encapsulated within \code{past_outputs}). This necessitates converting objects stored in \code{data_env} and \code{object_env} into character strings, as achieved via this function.     
#' 
#' @param env Environment. The environment containing contents to convert to character string. Required.
#' @return The contents of the environment, without truncation, as a character string.
#' @details
#' This function converts all named bindings in \code{env} into a single string suitable for interpolation into the AI prompt via \pkg{glue}. Raw image bytes, \code{nativeRaster} and \code{raster} objects are replaced with the placeholder \code{"Image output"}, \code{NULL} values are replaced with \code{"NULL"}, and every other object is serialized with \code{dput} (or kept as-is when it is already a length-one character string). This function is used to build \code{past_outputs} from \code{data_env} and \code{object_env} so that both uploaded data and past API/tool outputs are visible to the AI.
#' @examples
#' # Create new environment
#' example_env <- new.env()
#' 
#' # Populate environment
#' example_env$a <- "hello"
#' example_env$b <- 1L
#' 
#' # Convert environment into string
#' env_to_str(example_env)
#' @export
env_to_str <- function(env) {
  results_list <- as.list(env)
  if (length(results_list) == 0) {
    return("")
  }
  
  object_strings <- sapply(names(results_list), function(obj_name) {
    obj <- results_list[[obj_name]]
    
    # Inline image-checking logic
    normalized_obj <- if (is.raw(obj) || inherits(obj, c("nativeRaster", "raster"))) {
      "Image output"
    } else if (is.null(obj)) {
      "NULL"
    } else {
      obj
    }
    
    # Serialize if else
    if (is.character(normalized_obj) && length(normalized_obj) == 1) {
      obj_text <- normalized_obj
    } else {
      obj_text <- paste(capture.output(dput(normalized_obj)), collapse = "\n")
    }
    
    paste0(obj_name, ":\n", obj_text)
  }, USE.NAMES = FALSE)
  
  paste(object_strings, collapse = "\n\n")
}

#' Extracts Latter Two Components of Function Path
#'
#' This function extracts the last two names from a '$'-delimited string, representing the path to a client function. Therefore, the last component represents the function whereas the penultimate component represents the environment.
#' 
#' This function returns a list with these components named \code{client} and \code{service}, respectively. If fewer than two components are provided, both elements will be \code{NA}. 
#'
#' @param path Character. The '$'-delimited string representing a function path. Required.
#' @return A named list with elements \code{client} and \code{service}, containing the extracted components.  
#' @details
#' This function is used in \code{\link{llm_api_result}} when populating the chosen API service/tool with the extracted arguments fails. By locating the client function and environment chosen by the AI, a dynamic error message is outputted to the user. This informs them of an unsuccessful LLM response to an input question given a failed \code{do.call()} in approachable language. It is thought that users may more easily understand the attempted client and environment of the failed call than the full client function path. Hence, these have been extracted such that the user question, prompt, services or data may be refined.  
#' @seealso \code{\link{llm_api_result}}
#' @example man/examples/examples_parse.R
#' @export
extract_path <- function(path = NULL) {
  
  if(any_is_empty(path)){
    stop("path is empty")
  }
  
  segments <- strsplit(path, "\\$")[[1]]
  segments <- trimws(segments)
  
  n <- length(segments)
  if (n < 2) return(list(service = NA, client = NA))
  list(
    service = segments[n-1],
    client = segments[n]
  )
}

#' Performs AI-facilitated API Service/Tool Call
#'
#' This function uses \code{\link{code_extract}} to extract the function parameters from the LLM-generated JSON response in \code{\link{ChatRBox}}. This is used to populate the necessary client function and facilitate the AI response to an input question. This function also facilitates image rendering of API responses, AI-written summaries of service outputs and dynamic error messages such that it may be considered the ChatRBox master function.  
#'
#' @param llm_response Character. LLM output from chosen AI provider. This may contain text and code blocks as JSON objects. Defaults to empty string.
#' @param object Object. The \code{ChatRBox_obj} \code{S7} object containing the API services/tool functions and populated tool/data environments. Defaults to NULL.
#' @return The output of the most suitable API client/tool function, populated with extracted parameters from the input question.  
#' @details
#' \code{S7} generics are used to extract the properties from the \code{ChatRBox_obj} \code{S7} object necessary to populate the relevant API client/tool function. This function implements the AI-chosen service with the extracted function arguments. The user-facing \code{R6} object is updated after every API/tool call so that \code{do.call()} outputs are interpolated into the AI prompt without explicit updates. Therefore, chained API calls are possible, whereby the AI uses a previous service output as input for a sequential call, within a single AI response.
#' 
#' Chained API calls are generalized to any object type via \code{\link{resolve_arg_refs}}. Every non-empty service/tool output is stored as a named R object in the dedicated object environment (\code{object_env}) under its AI-generated, human-readable name, regardless of its type. Before each \code{do.call()}, scalar character arguments that match the name of an object stored in \code{object_env} or \code{data_env} are replaced with the stored object itself (the object environment taking precedence). Because these environments store past service/tool outputs of any type, this means JSON objects, data frames, lists, narrative text, vectors or single values may all be referenced by name and passed as input to a subsequent service.
#' 
#' Error handling includes a generic message for no suitable service/failed AI API call, alongside a dynamic message for failed function population using \code{do.call()}. The dynamic message depicts the function, associated environment, arguments and argument types attempted to be used in natural language (with no reference to function path), API HTTP status codes and suggested troubleshooting steps dependent on above. Finally, if the \code{summarize} argument is set to \code{TRUE} during a \code{$talk()} call, this function employs \code{\link{llm_followup_summary}} to append an AI-generated summary of service outputs to the named list of service results. Users may provide specific summary prompts, separate from that supplied using \code{prompt_template}, via \code{summary_list} during chatbot initialization. These are assigned to given services via name matching, enabling each service summary to utilize distinct prompting. 
#' @seealso \code{\link{ChatRBox}}, \code{\link{llm_followup_summary}} 
#' @importFrom jsonlite fromJSON
#' @importFrom utils capture.output str
#' @example man/examples/examples_object.R
#' @export
llm_api_result <- function(llm_response = "",
                           object = NULL) {
  
  if (is.null(object)) {
    stop("S7 object is NULL")
  }
  
  # Extract environments from underlying S7 object
  tools_env  <- ChatRBox::get_property(object, "tools_env")
  data_env   <- ChatRBox::get_property(object, "data_env")
  services   <- ChatRBox::get_property(object, "services")
  object_env <- ChatRBox::get_property(object, "object_env")
  config_env <- ChatRBox::get_property(object, "config_env")
  
  # Pre-compute allowed service paths
  valid_paths <- ChatRBox::get_property(object, "paths_string")
  valid_paths <- strsplit(valid_paths, "\n")[[1]]
  valid_paths <- trimws(valid_paths[nzchar(trimws(valid_paths))])
  
  # Extract httr2 parameters
  httr2_config <- if (!is.null(config_env$httr2_config)) {
    config_env$httr2_config
  } else {
    ChatRBox::get_property(object, "httr2_config")
  }
  
  # Extract JSON objects from LLM response
  code_block <- ChatRBox::code_extract(
    string = llm_response,
    language = "json"
  )
  
  if (any_is_empty(code_block)) {
    return(NULL)
  }
  
  # Parse one JSON object or a list of JSON objects
  objs <- tryCatch(
    jsonlite::fromJSON(code_block, simplifyVector = FALSE),
    error = function(e) {
      message("ChatRBox could not parse AI response: ", conditionMessage(e))
      NULL
    }
  )
  
  # No usable JSON
  if (is.null(objs)) {
    return(NULL)
  }
  
  # Single named JSON wrapped in lists
  if (!is.list(objs) || !is.null(names(objs))) {
    objs <- list(objs)
  }
  
  # Initialize results and naming objects
  results <- list()
  names_list <- character(length(objs))
  service_keys <- character(length(objs))
  
  # Extract service paths and names to construct argument list
  for (i in seq_along(objs)) {
    obj <- objs[[i]]
    
    # Skip invalid array elements 
    if (!is.list(obj)) {
      message(sprintf(
        "ChatRBox skipped item %d: expected a JSON object but received a %s.",
        i, class(obj)[1]
      ))
      name <- normalize_result_name(NULL, index = i)
      results[[i]]      <- format_api_result_for_display(NULL, FALSE)
      names_list[[i]]   <- name
      service_keys[[i]] <- NA_character_
      next
    }
    
    path <- obj$path
    name <- normalize_result_name(obj$name, index = i)
    
    service_keys[[i]] <- tryCatch(
      ChatRBox::get_service_key(path),
      error = function(e) NA_character_
    )
    
    arg_list <- obj[setdiff(names(obj), c("path", "name"))]
    
    # Resolves named references to stored objects within object_env
    arg_list <- ChatRBox::resolve_arg_refs(
      arg_list = arg_list,
      data_env = data_env,
      object_env = object_env
    )
    
    # Execute the LLM-chosen client function path using do.call()
    result <- tryCatch({
      # Only allows pre-computed service paths
      fn <- if (!validate_service_path(path, valid_paths)) {
        NULL
      } else {
        tryCatch(eval(parse(text = path)), error = function(e) NULL)
      }
      # Accepts only function names
      if (!is.function(fn)) {
        stop(sprintf(
          "No callable client function found at path '%s'.", path %||% "<missing>"
        ))
      }
      do.call(what = fn, args = arg_list)
    }, error = function(e) {
      info <- if (any_is_empty(path)) {
        list(service = NA, client = NA)
      } else {
        tryCatch(
          ChatRBox::extract_path(path),
          error = function(e) list(service = NA, client = NA)
        )
      }
      httr2_msg <- ChatRBox::format_httr2_config(httr2_config)
      
      api_error <- conditionMessage(e)
      
      if (is.null(api_error) || !nzchar(api_error)) {
        api_error <- "No additional error message was returned by the API client."
      }
      
      api_error <- ChatRBox::add_http_status_guidance(e, api_error)
      
      # Construct dynamic error message using attempted parameters and HTTP status code
      msg <- sprintf(
        paste0(
          "ChatRBox attempted to run the function '%s' from the service '%s' ",
          "but failed to generate a response.\n\n",
          "Arguments supplied:\n%s\n\n",
          "httr2 request configuration:\n%s\n\n",
          "Underlying API/client error:\n%s\n"
        ),
        info$client,
        info$service,
        paste(capture.output(str(ChatRBox::redact_sensitive(arg_list))), collapse = "\n"),
        httr2_msg,
        api_error
      )
      
      cat(msg)
      
      NULL
    })
    
    # Store original result in object_env for chained API calls before formatting for display
    store_api_result(
      result = result,
      name = name,
      object_env = object_env
    )
    
    # Render images/plots centrally within ChatRBox when not included in client_fns
    rendered_image <- isTRUE(
      tryCatch(
        render_api_image(result),
        error = function(e) FALSE
      )
    )
    
    display_result <- format_api_result_for_display(
      result = result,
      rendered_image = rendered_image
    )
    
    results[[i]] <- display_result
    names_list[[i]] <- name
  }
  
  # De-duplicate AI-generated names 
  names_list <- make.unique(names_list, sep = "_")
  
  names(results) <- names_list
  
  names(service_keys) <- names_list
  attr(results, "service_keys") <- service_keys
  
  results
}

# internal check -----------------------------------------------------

#' Checks for Non-whitespace Values in Specified Vector
#' @description
#' Use [internal_is_empty()] if you want logical values for each element in
#' a vector.
#' 
#' Use [any_is_empty()] if you want a single logical value returned for 
#' a vector. This is the equivalent to ```any(internal_is_empty(x))```.
#' 
#' @param x vector to check
#' @return [internal_is_empty()] returns vector of logical values
#' indicating the presence of empty values TRUE 
#' or non-empty values FALSE.
#' @noRd
internal_is_empty <- function(x = NULL){
  
  # Handle NULL input - return single TRUE
  if (is.null(x)) {
    return(TRUE)
  }
  
  # Handle empty vector - return single TRUE
  if (length(x) == 0) {
    return(TRUE)
  }
  
  # grepl returns FALSE for NA values (they don't match the pattern)
  # !grepl returns TRUE for empty strings, whitespace-only, and NA
  vector_out <- !grepl(x = x, pattern = "\\S")
  
  return(vector_out)
}

#' @noRd
any_is_empty <- function(x = NULL) {
  
  vector_out <- any(internal_is_empty(x = x))
  
  return(vector_out)
}

#' @noRd
validate_service_path <- function(path, valid_paths) {
  if (any_is_empty(path)) {
    return(invisible(FALSE))
  }
  isTRUE(path %in% valid_paths)
}

#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
