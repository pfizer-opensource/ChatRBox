#' ChatRBox S7 Class
#'
#' An \code{S7} class for storing AI prompts, API services/tools, tools/data environments and function path summaries.
#' 
#' This object class is generated in \code{\link{ChatRBox}} using \code{\link{object_generate}}.
#' 
#' @section Properties:
#' \describe{
#'   \item{prompt}{Character. The AI prompt to set model behavior. Defaults to \code{character(0)}.}
#'   \item{services}{List. Named list of API services/tools. Defaults to empty list.}
#'   \item{tools_env}{Environment. Environment containing tool functions. Defaults to empty environment.}
#'   \item{data_env}{Environment. Environment containing data frames. Defaults to empty environment.}
#'   \item{object_env}{Environment. Environment containing past API/tool outputs stored as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to empty environment.}
#'   \item{paths_string}{Character. Concatenated function paths. Defaults to \code{character(0)}.}
#'   \item{args_string}{Character. Concatenated function paths with arguments. Defaults to \code{character(0)}.}
#'   \item{prompt_template}{Character. Template used to generate AI prompt. This may refer to \code{paths_string} and \code{args_string} which append using \code{glue::glue()}. Defaults to \code{character(0)}.}
#'   \item{summary_list}{List. Named list of instructions for AI-enabled summaries. Defaults to empty list.}
#'   \item{final_summary_prompt}{Character. Single synthesis instruction steering \code{summarize = "final"}. Defaults to an empty string.}
#'   \item{httr2_config}{List. Named list of \pkg{httr2} request options for generated client functions. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings.}
#'   \item{config_env}{Environment. Environment caching the normalized \code{httr2_config} assignments. Defaults to empty environment.}
#' }
#' @section Validator:
#' The \code{validator} ensures all properties are the expected type and length. This is analogous to setValidity checks for S4 objects. 
#' 
#' @param prompt Character. The AI prompt to set model behavior. Defaults to \code{character(0)}.
#' @param services List. Named list of API services/tools. Defaults to empty list.
#' @param tools_env Environment. Environment containing tool functions. Defaults to empty environment.
#' @param data_env Environment. Environment containing data frames. Defaults to empty environment.
#' @param object_env Environment. Environment containing past API/tool outputs stored as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to empty environment.
#' @param paths_string Character. Concatenated function paths. Defaults to \code{character(0)}.
#' @param args_string Character. Concatenated function paths with arguments. Defaults to \code{character(0)}.
#' @param prompt_template Character. Template used to generate AI prompt. Defaults to \code{character(0)}.
#' @param summary_list List. Optional named list of instructions for AI-enabled summaries, assigned to specific services via name matching. Prompting may be provided via character strings or a Markdown file. Defaults to empty list.
#' @param final_summary_prompt Character. Single synthesis instruction steering \code{summarize = "final"}. Unlike the per-service \code{summary_list}, this is a single prompt applied to the one holistic answer. Supplied as a literal string or the path to a Markdown file, resolved identically to \code{prompt_template}. Defaults to an empty string.
#' @param httr2_config List. Optional named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings.
#' @param config_env Environment. Optional environment caching the normalized \code{httr2_config} so it is shared by all generated client functions. Defaults to NULL, in which case it is created internally. Defaults to empty environment.
#' @seealso \code{\link{ChatRBox}}, \code{\link{object_generate}}
#' @example man/examples/examples_object.R
#' @export
# S7 class: typed, validated internal state.
# See CONTRIBUTING.md for the OOP architecture rationale.
ChatRBox_obj <- S7::new_class(
  name = "ChatRBox_obj",
  properties = list(
    prompt = S7::class_character,
    services = S7::class_list,
    tools_env = S7::class_environment,
    data_env = S7::class_environment,
    object_env = S7::class_environment,
    paths_string = S7::class_character,
    args_string = S7::class_character,
    prompt_template = S7::class_character,
    summary_list = S7::class_list,
    final_summary_prompt = S7::class_character,
    httr2_config = S7::class_list,
    config_env = S7::class_environment
  ),
  validator = function(self) {
    if (length(self@prompt) != 1) {
      "@prompt must be length 1"
    } else if (!is.list(self@services)) {
      "@services must be a list"
    } else if (!is.environment(self@tools_env)) {
      "@tools_env must be an environment"
    } else if (!is.environment(self@data_env)) {
      "@data_env must be an environment"
    } else if (!is.environment(self@object_env)) {
      "@object_env must be an environment"  
    } else if (length(self@paths_string) != 1) {
      "@paths_string must be length 1"
    } else if (length(self@args_string) != 1) {
      "@args_string must be length 1"
    } else if (length(self@prompt_template) != 1) {
      "@prompt_template must be length 1"
    } else if (!is.list(self@summary_list)) {
      "@summary_list must be a list"
    } else if (length(self@final_summary_prompt) != 1) {
      "@final_summary_prompt must be length 1"
    } else if (!is.list(self@httr2_config)) {
      "@httr2_config must be a list"
    } else if (!is.environment(self@config_env)) {
      "@config_env must be an environment"
    } else {
      NULL
    }
  }
)

#' Extracts Property from ChatRBox S7 Objects
#' 
#' This function returns given properties from the \code{ChatRBox_obj} \code{S7} object.
#'
#' @param x \code{S7} object. A \code{ChatRBox_obj} \code{S7} object. Required.
#' @param ... Additional arguments including \code{property}, 
#' the property name to extract. Required.
#' @return The requested property in the associated format.
#' @importFrom S7 new_generic method prop
#' @example man/examples/examples_object.R
#' @export
get_property <- S7::new_generic("get_property", dispatch_args = "x")

S7::method(get_property, ChatRBox_obj) <- function(x, property, ...) {
  S7::prop(x, property)
}

#' Constructs Environment/List Paths to Client Functions
#' 
#' This function recursively searches a list of environments/lists and outputs the paths to every function using '$' notation. Paths are outputted as character vectors. 
#' 
#' This function is used within \code{\link{ChatRBox}} such that the top-level object of each path is hard-coded as \code{services}.  
#'
#' @param obj List. A list of environments/lists to search for functions. This may be nested. Required.
#' @param prefix Character. String used to build function paths during recursion. Defaults to empty string.
#' @return A character vector of function paths beginning with \code{services}, using '$' notation.  
#' @details
#' The output of this function documents the path to every function contained within a list/nested list. This function is applied to the list of services in \code{\link{ChatRBox}} to output a list of tool functions/API services paths. 
#' 
#' This is included as prompting in the AI request. Therefore, the LLM outputs the single most suitable function path to answer the input question. The equivalent function for function path extraction with arguments is \code{\link{get_function_args}}. Both listed outputs are included in the AI prompt. 
#' @seealso \code{\link{ChatRBox}}
#' @example man/examples/examples_services_list.R
#' @export
get_function_path <- function(obj, 
                              prefix = "") {
  
  # Empty character vector returned if obj is empty environment/list
  if (is.environment(obj) && length(ls(envir = obj)) == 0) return(character(0))
  if (is.list(obj) && length(obj) == 0) return(character(0))
  
  # Initializes empty character vector for path storage
  paths <- character()
  
  # Constructs path to each function in list/environment
  if (is.environment(obj)) {
    obj_names <- ls(envir = obj)
    fn_names <- obj_names[sapply(obj_names, function(nm) is.function(get(nm, envir = obj)))]
    for (fn in fn_names) {
      full_path <- if (prefix == "") paste0("services$", fn) else paste0("services$", prefix, "$", fn)
      paths <- c(paths, full_path)
    }
    
  } else if (is.list(obj)) {
    for (nm in names(obj)) {
      next_obj <- obj[[nm]]
      new_prefix <- if (prefix == "") nm else paste0(prefix, "$", nm)
      paths <- c(paths, get_function_path(next_obj, new_prefix))
    }
  }
  paths
}

#' Extracts Function Arguments 
#' 
#' This function retrieves the formal arguments of a function and outputs them, alongside their default values, as a string formatted like an R function signature.  
#'
#' @param fn Function. Function for argument extraction. Required.
#' @return A character string listing function arguments and default values.  
#' @details
#' This function is used within \code{\link{get_function_args}} to output a path to every function contained within a list/nested list, alongside function arguments.   
#' @seealso \code{\link{get_function_args}}
#' @example man/examples/examples_services_list.R
#' @export
get_args_str <- function(fn) {
  
  # Extracts formal arguments as named list
  fmls <- formals(fn)
  
  # Returns only argument for no default value
  if (is.null(fmls)) return("")
  
  # Returns all arguments as single, comma-separated string
  paste(sapply(names(fmls), function(arg) {
    val <- fmls[[arg]]
    if (missing(val) || (is.symbol(val) && identical(as.character(val), ""))) arg
    else paste0(arg, " = ", paste(deparse(val), collapse = ""))
  }), collapse = ", ")
}

#' Constructs Environment/List Paths to Client Functions with Arguments
#' 
#' This function recursively searches a list of environments/lists and outputs the paths to every function using '$' notation, alongside function arguments. Paths are outputted as character vectors. 
#' 
#' This function is used within \code{\link{ChatRBox}} such that the top-level object of each path is hard-coded as \code{services}.  
#'
#' @param obj List. A list of environments/lists to search for functions. This may be nested. Required.
#' @param prefix Character. String used to build function paths during recursion. Defaults to empty string.
#' @return A character vector of function paths with function arguments, beginning with \code{services} and using '$' notation.  
#' @details
#' The output of this function documents the path to every function contained within a list/nested list, alongside function arguments. This function is applied to the list of services in \code{\link{ChatRBox}} to output the tool function/API service arguments and defaults. 
#' 
#' This is included as prompting in the AI request. Therefore, the LLM outputs a JSON string of parameter values for the single most suitable function path, to answer the input question. The equivalent function for only function path extraction is \code{\link{get_function_path}}. Both listed outputs are included in the AI prompt.   
#' @seealso \code{\link{ChatRBox}}, \code{\link{get_function_path}}
#' @example man/examples/examples_services_list.R
#' @export
get_function_args <- function(obj, 
                              prefix = "") {
  
  # Empty character vector returned if obj is empty environment/list
  if (is.environment(obj) && length(ls(envir = obj)) == 0) return(character(0))
  if (is.list(obj) && length(obj) == 0) return(character(0))
  
  # Initializes empty character vector for argument storage
  args <- character()
  
  # Constructs path to each function in list/environment with arguments
  if (is.environment(obj)) {
    for (fn in ls(obj)) {
      f <- get(fn, obj)
      if (is.function(f)) {
        full_path <- paste0("services$", if(prefix!="") paste0(prefix,"$"), fn)
        args <- c(args, paste0(full_path, "(", ChatRBox::get_args_str(f), ")"))
      }
    }
    
  } else if (is.list(obj)) {
    for (nm in names(obj)) {
      next_obj <- obj[[nm]]
      new_prefix <- if(prefix=="") nm else paste0(prefix,"$",nm)
      args <- c(args, get_function_args(next_obj, new_prefix))
    }
  }
  args
}

#' Extracts Available Service Keys from a ChatRBox Services List
#'
#' This function returns the user-facing service keys present in a ChatRBox services list, namely every tool function name and every API service name. These keys correspond to the names a user supplies in \code{tools_list}, \code{services_list} and \code{openapi_list}, and are the keys used to match \code{summary_list} entries (and tokens/headers) to a given service.
#'
#' @param services List. A ChatRBox services list, with a \code{tools} environment and an \code{api_services} named list, as constructed in \code{\link{object_generate}}. Required.
#' @return A character vector of unique service keys (tool names and API service names). Returns \code{character(0)} when no services are present.
#' @details
#' This function is used to validate \code{summary_list} names against the available services in \code{\link{object_generate}} and \code{\link{property_generate}}, reusing the same checks applied to API tokens and headers (\code{\link{check_names}}, \code{\link{validate_name_match}}, \code{\link{check_duplicate_names}}). This facilitates the name-matching logic underpinning the ChatRBox workflow, whereby given summary prompts and token information are matched to the appropriate service based on user-supplied naming during chatbot initialization. Hence, errors will be thrown for incorrect or duplicated user naming conventions.
#' @seealso \code{\link{process_summary_list}}, \code{\link{object_generate}}, \code{\link{check_names}}, \code{\link{validate_name_match}}, \code{\link{check_duplicate_names}}
#' @example man/examples/examples_object.R
#' @export
get_service_keys <- function(services) {
  
  # Extracts tool names
  tool_keys <- character(0)
  if (!is.null(services$tools) && is.environment(services$tools)) {
    tool_keys <- ls(services$tools)
  }
  
  # Extracts API service names
  api_keys <- character(0)
  if (!is.null(services$api_services) && !is.null(names(services$api_services))) {
    api_keys <- names(services$api_services)
  }
  
  # Collates unique extracted names into character string
  unique(c(tool_keys, api_keys))
}

#' Constructs ChatRBox S7 Object for API Services/Tools 
#' 
#' This function creates an \code{S7} object containing the AI prompt, API services/tools, tools/data/storage/request environments, function path summaries and the inputted prompt template. Available API services/tools may be named and listed in \code{services_list} and \code{tools_list} respectively, alongside service-specific token and AI summary information which must be stored using identical naming. Whilst the \code{R6} object \code{\link{ChatRBox}} is the user-facing object that may be interacted with via \code{$talk()} calls, this function generates the underlying \code{S7} object that is kept within \code{\link{ChatRBox}} and responsible for storing all user-supplied arguments.     
#' 
#' This function also generates a summary of client function paths called \code{paths_string}, and a summary of client function arguments called \code{args_string}. These variables are contained in the returned \code{S7} object, but can be referenced in \code{prompt_template} using \code{{}} notation. This allows integration with the AI prompt using \pkg{glue}. Therefore, users may customize AI instruction regarding API services/tools.     
#' 
#' @param services_list List. Named list of API root URLs (without trailing slashes). Defaults to empty list.
#' @param tools_list List. Named list of tool functions. Defaults to empty list.
#' @param data_list List. Named list of data frames. Defaults to empty list.
#' @param openapi_list List. Named list of OpenAPI JSON schemas. Each value may be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. The \code{base_url} form is required when a pre-parsed schema's \code{servers[[1]].url} is relative or empty; a bare parsed list only works when that server URL is already absolute. Defaults to empty list.
#' @param tools_env Environment. Tool functions are added to this environment. Defaults to NULL which creates a new environment.
#' @param data_env Environment. Data frames are added to this environment. Defaults to NULL which creates a new environment.
#' @param object_env Environment. Past API service/tool outputs are stored in this environment as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to NULL which creates a new environment.
#' @param prompt_template Character. Customizable AI prompt to set behavior of AI model. Defaults to empty string.
#' @param summary_list List. Optional named list of instructions for AI-enabled summaries, assigned to specific services via name matching. Prompting may be provided via character strings or a Markdown file. Defaults to empty list.
#' @param final_summary_prompt Character. Single synthesis instruction steering \code{summarize = "final"}, as opposed to the per-service \code{summary_list} used by \code{summarize = TRUE}. Supplied as a literal string or the path to a Markdown file, resolved identically to \code{prompt_template}. Defaults to an empty string.
#' @param token_service Character. Optional named list of API tokens for \code{services_list}. Defaults to empty list.
#' @param header_service Character. Optional named list of API authorization headers for \code{services_list}. Defaults to empty list.
#' @param token_openapi Character. Optional named list of API tokens for \code{openapi_list}. Defaults to empty list.
#' @param header_openapi Character. Optional named list of API authorization headers for \code{openapi_list}. Defaults to empty list. 
#' @param httr2_config List. Optional named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings. These only relate to services provided via \code{openapi_list}.
#' @param config_env Environment. Optional environment caching the normalized \code{httr2_config} so it is shared by all generated client functions. Defaults to NULL, in which case it is created internally.
#' @return An \code{S7} object containing an AI prompt, API services/tools, tools/data/storage/request environments, function path summaries, inputted prompt template, listed AI-enabled summary prompts and listed \pkg{httr2} request preferences. Each property has a corresponding \code{S7} method for extraction. 
#' @details
#' This \code{S7} object is the underlying infrastructure of the \code{\link{ChatRBox}} \code{R6} object. Hence, to update the \code{R6} object parameters using \code{\link{ChatRBox_update}}, this \code{S7} object is updated in-place using \code{\link{object_update}}. 
#' 
#' @seealso \code{\link{ChatRBox}}
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom purrr imap
#' @example man/examples/examples_object.R
#' @export
object_generate <- function(services_list = list(),
                            tools_list = list(),
                            data_list = list(),
                            openapi_list = list(),
                            tools_env = NULL,
                            data_env = NULL,
                            object_env = NULL,
                            prompt_template = ChatRBox::load_prompt_template(),
                            summary_list = list(),
                            final_summary_prompt = "",
                            token_service = list(),
                            header_service = list(),
                            token_openapi = list(),
                            header_openapi = list(),
                            httr2_config = list(),
                            config_env = NULL) {
  
  # Set new environments where applicable
  if (is.null(tools_env)) {
    tools_env <- new.env()
  }
  
  if (is.null(data_env)) {
    data_env <- new.env()
  }
  
  if (is.null(object_env)) {
    object_env <- new.env()
  }
  
  if (is.null(config_env)) {
    config_env <- new.env()
  }
  
  # Normalize httr2 request options with default settings
  httr2_config <- ChatRBox::normalize_httr2_config(httr2_config)
  config_env$httr2_config <- httr2_config
  
  # Populate tools/data environments
  list2env(tools_list, envir = tools_env)
  list2env(data_list, envir = data_env)
  
  # Build client functions for openapi_list URLs
  api_client <- ChatRBox::get_api_client_env(
    services_list = services_list,
    token_service = token_service,
    header_service = header_service
  )
  
  build_api_client <- ChatRBox::build_api_client_env(
    openapi_list = openapi_list,
    token_openapi = token_openapi,
    header_openapi = header_openapi,
    httr2_config = httr2_config,
    config_env = config_env
  )
  
  all_names <- c(names(api_client), names(build_api_client))
  duplicated_names <- unique(all_names[duplicated(all_names)])
  
  # Check validity of user-supplied names for name matching logic
  if (length(duplicated_names) > 0L) {
    stop(
      "Duplicate service names found across services_list and openapi_list: ",
      paste(duplicated_names, collapse = ", "),
      ". Please use unique service names."
    )
  }
  
  all_api_client <- c(api_client, build_api_client)
  
  # Collate all tools_list, services_list and openapi_list services
  services <- list(
    tools = tools_env,
    api_services = all_api_client
  )
  
  # Extract service paths for AI prompt
  paths_vec <- ChatRBox::get_function_path(services)
  paths_string <- paste(paths_vec, collapse = "\n")
  
  args_vec <- ChatRBox::get_function_args(services)
  args_string <- paste(args_vec, collapse = "\n")
  
  # Interpolate past data uploads and API outputs into AI prompt
  past_outputs <- {
    env_strs <- Filter(nzchar, c(
      ChatRBox::env_to_str(data_env),
      ChatRBox::env_to_str(object_env)
    ))
    if (length(env_strs) == 0L) "" else paste(env_strs, collapse = "\n\n")
  }
  
  prompt <- glue::glue(prompt_template)
  
  # Assign user-supplied summary prompts to services via name matching
  summary_list <- ChatRBox::process_summary_list(
    summary_list = summary_list,
    service_keys = ChatRBox::get_service_keys(services)
  )
  
  # Outputs constructed S7 object
  ChatRBox_obj(
    prompt = prompt,
    services = services,
    tools_env = tools_env,
    data_env = data_env,
    object_env = object_env,
    paths_string = paths_string,
    args_string = args_string,
    prompt_template = prompt_template,
    summary_list = summary_list,
    final_summary_prompt = final_summary_prompt,
    httr2_config = httr2_config,
    config_env = config_env
  )
}

#' Constructs List of Updated ChatRBox S7 Object Properties 
#' 
#' This function is similar to \code{\link{object_generate}} in that it generates an AI prompt, API services/tools, tools/data/storage/request environments, function path summaries, the inputted prompt template, listed AI-enabled summary prompts and listed \pkg{httr2} request preferences, outputted in a named list. These properties are used within \code{\link{object_update}} in order to update the \code{ChatRBox_obj} \code{S7} object in-place. 
#' 
#' These updates are additive meaning new API services/tools will be added to the pre-existing object services list. Services will only be over-written if given identical names or added to new environments. New tools/data are added to existing tools/data environments by default. Other listed \code{R6} arguments like \code{summary_list} and \code{httr2_config} will be updated additively, whereby individual summary prompts or \pkg{httr2} arguments may be altered independently of others assigned upon initialization.   
#' 
#' New API services/tools may be listed in \code{services_list} and \code{tools_list}. This function generates a summary of client function paths called \code{paths_string}, and a summary of client function arguments called \code{args_string}. These variables are outputted alongside the AI prompt in the returned list, but can be referenced in \code{prompt_template} using \code{{}} notation. Therefore, users may customize AI instruction regarding API services/tools whilst the interpolated client paths in \code{prompt_template} are updated accordingly.       
#'
#' @param object Object. ChatRBox \code{S7} object to update in place. Required. 
#' @param services_list List. Named list of API root URLs (without trailing slashes). Defaults to empty list.
#' @param tools_list List. Named list of tool functions. Defaults to empty list.
#' @param data_list List. Named list of data frames. Defaults to empty list.
#' @param openapi_list List. Named list of OpenAPI JSON schemas. Each value may be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. The \code{base_url} form is required when a pre-parsed schema's \code{servers[[1]].url} is relative or empty; a bare parsed list only works when that server URL is already absolute. Defaults to empty list.
#' @param tools_env Environment. Tool functions are added to this environment. Defaults to NULL which creates a new environment.
#' @param data_env Environment. Data frames are added to this environment. Defaults to NULL which creates a new environment.
#' @param object_env Environment. Past API service/tool outputs are stored in this environment as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to NULL which creates a new environment.
#' @param prompt_template Character. Customizable AI prompt to set behavior of AI model. Defaults to empty string.
#' @param summary_list List. Optional named list of instructions for AI-enabled summaries, assigned to specific services via name matching. Prompting may be provided via character strings or a Markdown file. Defaults to empty list.
#' @param final_summary_prompt Character. Single synthesis instruction steering \code{summarize = "final"}, as opposed to the per-service \code{summary_list} used by \code{summarize = TRUE}. Supplied as a literal string or the path to a Markdown file, resolved identically to \code{prompt_template}. Defaults to the current object value.
#' @param token_service Character. Optional named list of API tokens for \code{services_list}. Defaults to empty list.
#' @param header_service Character. Optional named list of API authorization headers for \code{services_list}. Defaults to empty list.
#' @param token_openapi Character. Optional named list of API tokens for \code{openapi_list}. Defaults to empty list.
#' @param header_openapi Character. Optional named list of API authorization headers for \code{openapi_list}. Defaults to empty list. 
#' @param httr2_config List. Optional named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings. These only relate to services provided via \code{openapi_list}.
#' @param config_env Environment. Optional environment caching the normalized \code{httr2_config} so it is shared by all generated client functions. Defaults to NULL, in which case it is created internally.
#' @return A named list containing an AI prompt, API services/tools, tools/data/storage/request environments, function path summaries, inputted prompt template, listed AI-enabled summary prompts and listed \pkg{httr2} request preferences. Each property has a corresponding \code{S7} method for extraction.   
#' @details
#' This function and \code{\link{object_generate}} take the same arguments and have nearly identical workflows. However, whilst \code{\link{object_generate}} creates a \code{ChatRBox_obj} \code{S7} object, this function outputs the new \code{S7} object properties as a named list. Therefore, \code{\link{object_generate}} is used within \code{\link{ChatRBox}} in order to facilitate the AI-generated response.
#' 
#' This function is rather used within \code{\link{object_update}} such that the \code{ChatRBox_obj} \code{S7} object properties may be modified in-place, without the overwriting of environments or services. If replacement of object properties is preferred to modification, the \code{ChatRBox_obj} \code{S7} object may simply be regenerated using \code{\link{object_generate}}. With respect to the overarching and user-facing \code{R6} object, this translates to chatbot re-initialization using \code{ChatRBox$new()} rather than modification using \code{\link{ChatRBox_update}}.  
#' @seealso \code{\link{ChatRBox}}, \code{\link{object_generate}}, \code{\link{ChatRBox_update}}
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom purrr imap
#' @example man/examples/examples_object.R
#' @export
property_generate <- function(object,
                              services_list = list(),
                              tools_list = list(),
                              openapi_list = list(),
                              data_list = list(),
                              tools_env = NULL,
                              data_env = NULL,
                              object_env = NULL,
                              prompt_template = "",
                              summary_list = list(),
                              final_summary_prompt = object@final_summary_prompt,
                              token_service = list(),
                              header_service = list(),
                              token_openapi = list(),
                              header_openapi = list(),
                              httr2_config = list(),
                              config_env = NULL) {
  
  # Create new environments for tools/data/storage/request options
  if (is.null(tools_env)) tools_env <- new.env()
  if (is.null(data_env)) data_env <- new.env()
  if (is.null(object_env)) object_env <- new.env()
  if (is.null(config_env)) config_env <- new.env()
  
  # Assign updated httr2 request options based on user-supplied arguments
  base_httr2_config <- if (identical(config_env, object@config_env)) object@httr2_config else list()
  httr2_config <- utils::modifyList(base_httr2_config, httr2_config)
  httr2_config <- ChatRBox::normalize_httr2_config(httr2_config)
  config_env$httr2_config <- httr2_config
  
  # Add tool functions to tools_env
  list2env(tools_list, envir = tools_env)
  
  # Add data frames to data_env
  list2env(data_list, envir = data_env) 
  
  # Extract new API client functions
  api_update <- ChatRBox::get_api_client_env(services_list = services_list,
                                             token_service = token_service,
                                             header_service = header_service)
  
  # Generate new API client functions
  # This involves inclusion of newly defined httr2 request options
  build_api_update <- ChatRBox::build_api_client_env(openapi_list = openapi_list,
                                                     token_openapi = token_openapi,
                                                     header_openapi = header_openapi,
                                                     httr2_config = httr2_config,
                                                     config_env = config_env)
  
  # Collate updated API client functions
  all_api_update <- c(api_update, build_api_update)
  
  # Update API services list
  updated_services <- c(object@services$api_services, all_api_update)
  
  # Construct new services list 
  services <- list( 
    tools = tools_env,
    api_services = updated_services
  )
  
  # List of function paths for prompt
  paths_vec <- ChatRBox::get_function_path(services)
  paths_string <- paste(paths_vec, collapse = "\n")
  
  # List of function arguments for prompt
  args_vec <- ChatRBox::get_function_args(services)
  args_string <- paste(args_vec, collapse = "\n")
  
  # List of past data uploads and API outputs for prompt
  past_outputs <- {
    env_strs <- Filter(nzchar, c(
      ChatRBox::env_to_str(data_env),
      ChatRBox::env_to_str(object_env)
    ))
    if (length(env_strs) == 0L) "" else paste(env_strs, collapse = "\n\n")
  }
  
  # Create interpolated system prompt
  prompt <- glue::glue(prompt_template)
  
  # Merge new summary instructions with existing/default options
  summary_list <- ChatRBox::process_summary_list(
    summary_list = summary_list,
    service_keys = ChatRBox::get_service_keys(services),
    existing = object@summary_list
  )
  
  # Outputs list of S7 variables rather than S7 object itself
  list(
    prompt = prompt,
    services = services,
    tools_env = tools_env,
    data_env = data_env,
    object_env = object_env,
    paths_string = paths_string,
    args_string = args_string,
    prompt_template = prompt_template,
    summary_list = summary_list,
    final_summary_prompt = final_summary_prompt,
    httr2_config = httr2_config,
    config_env = config_env)
}

#' Updates ChatRBox_obj S7 Object Properties In-Place
#' 
#' This function uses \code{\link{property_generate}} to generate the updated \code{ChatRBox_obj} \code{S7} object properties, given new requirements. These replace the existing object properties. 
#' 
#' Updates are additive meaning new API services/tools will be added to the pre-existing object services list. Services will only be over-written if given identical names or added to new environments. New tools/data are added to existing tools/data environments by default, whilst the \code{prompt_template} is unchanged except for adjusted \code{paths_string} and \code{args_string} which update given new API services/tools. 
#'  
#' @param object Object. \code{ChatRBox_obj} \code{S7} object to update in place. Required.
#' @param services_list List. Named list of API root URLs (without trailing slashes). Defaults to empty list.
#' @param tools_list List. Named list of tool functions. Defaults to empty list.
#' @param data_list List. Named list of data frames. Defaults to empty list.
#' @param openapi_list List. Named list of OpenAPI JSON schemas. Each value may be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. The \code{base_url} form is required when a pre-parsed schema's \code{servers[[1]].url} is relative or empty; a bare parsed list only works when that server URL is already absolute. Defaults to empty list.
#' @param tools_env Environment. Tool functions are added to this environment. Defaults to existing object tools environment. Therefore, inputted tool functions add to, rather than replace, existing tool functions.
#' @param data_env Environment. Data frames are added to this environment. Defaults to existing object data environment. Therefore, inputted data frames add to, rather than replace, existing data frames.
#' @param object_env Environment. Past API service/tool outputs are stored in this environment as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to existing object object environment.
#' @param prompt_template Character. Customizable AI prompt to set behavior of AI model. Defaults to empty string.   
#' @param summary_list List. Optional named list of instructions for AI-enabled summaries, assigned to specific services via name matching. Prompting may be provided via character strings or a Markdown file. Defaults to empty list.
#' @param final_summary_prompt Character. Single synthesis instruction steering \code{summarize = "final"}, as opposed to the per-service \code{summary_list} used by \code{summarize = TRUE}. Supplied as a literal string or the path to a Markdown file, resolved identically to \code{prompt_template}. Updated additively; defaults to the current object value.
#' @param token_service Character. Optional named list of API tokens for \code{services_list}. Defaults to empty list.
#' @param header_service Character. Optional named list of API authorization headers for \code{services_list}. Defaults to empty list.
#' @param token_openapi Character. Optional named list of API tokens for \code{openapi_list}. Defaults to empty list.
#' @param header_openapi Character. Optional named list of API authorization headers for \code{openapi_list}. Defaults to empty list.
#' @param httr2_config List. Optional named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings. These only relate to services provided via \code{openapi_list}.
#' @param config_env Environment. Optional environment caching the normalized \code{httr2_config} so it is shared by all generated client functions. Defaults to NULL, in which case it is created internally.
#' @return An updated \code{ChatRBox_obj} \code{S7} object whereby each property has been regenerated and replaced, given the new system requirements. These updates occur in-place such that the modified object is outputted, rather than an object copy.   
#' @details
#' The default values in this function relate to the existing \code{S7} object. Hence, the inputted \code{ChatRBox_obj} \code{S7} object is essentially updated but can be regenerated using \code{\link{object_generate}}. 
#' 
#' This means that only the property to update need be used as an argument. For instance, if \code{tools_list} is not defined but \code{services_list} is, the new API services will be added but the original tool functions will remain, and appear in the prompt as a result. Updates are cumulative meaning only the service to add need be listed as an argument. 
#' 
#' Note that whilst individual property update functions are beneficial for most use cases, \code{ChatRBox_obj} \code{S7} objects have dependent properties. For example, \code{services_list} creates \code{args_string} and \code{paths_string}, which in turn inform \code{prompt_template}. Hence, a singular \code{\link{object_update}} function has been devised.   
#' @seealso \code{\link{property_generate}}
#' @example man/examples/examples_object.R
#' @export
object_update <- function(object, 
                          services_list = list(),
                          tools_list = list(),
                          openapi_list = list(),
                          data_list = list(),
                          tools_env = object@tools_env,
                          data_env = object@data_env,
                          object_env = object@object_env,
                          prompt_template = object@prompt_template,
                          summary_list = list(),
                          final_summary_prompt = object@final_summary_prompt,
                          token_service = list(),
                          header_service = list(),
                          token_openapi = list(),
                          header_openapi = list(),
                          httr2_config = list(),
                          config_env = object@config_env) {
  
  # Generates updated S7 object properties based on user inputs
  variables <- ChatRBox::property_generate(object = object,
                                           services_list = services_list,
                                           tools_list = tools_list,
                                           openapi_list = openapi_list,
                                           data_list = data_list,
                                           tools_env = tools_env,
                                           data_env = data_env,
                                           object_env = object_env,
                                           prompt_template = prompt_template,
                                           summary_list = summary_list,
                                           final_summary_prompt = final_summary_prompt,
                                           token_service = token_service,
                                           header_service = header_service,
                                           token_openapi = token_openapi,
                                           header_openapi = header_openapi,
                                           httr2_config = httr2_config,
                                           config_env = config_env)
  
  # Re-assigns variables within original S7 object
  object@prompt <- variables$prompt
  object@services <- variables$services
  object@tools_env <- variables$tools_env
  object@data_env <- variables$data_env
  object@object_env <- variables$object_env
  object@paths_string <- variables$paths_string
  object@args_string <- variables$args_string
  object@prompt_template <- variables$prompt_template
  object@summary_list <- variables$summary_list
  object@final_summary_prompt <- variables$final_summary_prompt
  object@httr2_config <- variables$httr2_config
  object@config_env <- variables$config_env
  
  object
}
