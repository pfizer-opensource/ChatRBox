#' Null-Coalescing Operator
#'
#' Returns the first argument if it is not \code{NULL}, otherwise returns the second argument. Often used to provide a default value if the first is missing or \code{NULL}.
#'
#' @param a An object. The primary value to test for \code{NULL}. Required.
#' @param b An object. The value to return if \code{a} is \code{NULL}. Required.
#' @return The value of \code{a} if it is not \code{NULL}; otherwise, the value of \code{b}.
#' @details
#' This function is used to provide default parameter values if none are supplied. This is used in \code{\link{build_api_client}} to create client functions for API services using the OpenAPI JSON schema.
#' @seealso \code{\link{build_api_client}}
#' @examples
#' result1 <- or_null(NULL, "default")
#' result2 <- or_null(42, "default")
#' @export
or_null <- function(a, b) if (!is.null(a)) a else b

#' Extracts Client Function Name from API Path String
#'
#' This function processes a file path or string, removing leading/trailing slashes and whitespace, inserting underscores between lowercase-uppercase character pairs, converting to lowercase, replacing all slashes or hyphens with underscores, and ensuring the result is a valid R object name.
#'
#' @param path Character. The API path string from which to extract and clean the client function name. Required.
#' @return A character string representing the cleaned and normalized client name, suitable for use as an R object name.
#' @details
#' This function is used in \code{\link{build_api_client}} to extract names for each endpoint in an OpenAPI JSON schema. These are used to name the generated client functions and interpolated into the AI prompt.
#' @seealso \code{\link{build_api_client}}
#' @example man/examples/examples_client_generation.R
#' @export
extract_client_name <- function(path) {
  clean <- gsub("^/|/$|\\s", "", path)
  clean <- gsub("([a-z])([A-Z])", "\\1_\\2", clean)
  clean <- tolower(gsub("/|-", "_", clean))
  clean <- gsub("[^a-z0-9_]", "", clean)
  make.names(clean)
}

#' Retrieves the Default Value for an API Parameter
#'
#' This function extracts the default value for an API parameter, using an OpenAPI JSON schema. If a default does not exist, this function returns an empty argument symbol, indicative of a required argument.
#'
#' @param param List. A list of parameter definitions, parsed from an OpenAPI JSON schema. Required.
#' @return The default values of each parameter, or an empty argument symbol if a parameter is required.
#' @details
#' This function is used within \code{\link{build_api_client}} to check for default values of each endpoint in an OpenAPI JSON schema. These are used within the client function definition, such that they are later interpolated into the AI prompt.
#' @seealso \code{\link{build_api_client}}
#' @example man/examples/examples_client_generation.R
#' @export
get_param_default <- function(param) {
  if (!is.null(param$default)) {
    param$default
  } else if (!is.null(param$schema) && !is.null(param$schema$default)) {
    param$schema$default
  } else {
    quote(expr = )
  }
}

#' Decodes API Responses into R Objects
#'
#' This function decodes an \pkg{httr2} response according to its declared content type. This function does not render, reshape, serialize, coerce, or otherwise assume what downstream APIs require.
#' @param resp Response body. \pkg{httr2} response object containing content type. Required.
#' @param simplify_json Logical. Passed to \code{jsonlite::fromJSON(simplifyVector = ...)}. Defaults to \code{TRUE}. 
#' @return Decoded response object.
#' @details
#' JSON is parsed into R objects. By default, \code{simplify_json = TRUE} preserves \pkg{jsonlite} default method, whereby homogeneous JSON arrays may become data frames. This functionality may be altered via modification of source code. This function is used within \code{\link{build_api_client}} as the final step of every generated client function, decoding the \pkg{httr2} response returned by \code{httr2::req_perform} according to its declared content type.   
#' @seealso \code{\link{build_api_client}} 
#' @export
decode_api_response <- function(resp, 
                                simplify_json = TRUE) {
  
  # Extracts content type
  content_type <- tryCatch(
    httr2::resp_content_type(resp),
    error = function(e) ""
  )
  
  if (is.null(content_type)) {
    content_type <- ""
  }
  
  content_type <- tolower(content_type)
  body <- httr2::resp_body_raw(resp)
  
  if (length(body) == 0L) {
    return(NULL)
  }
  
  # Binary/image responses are returned as raw bytes
  if (grepl("image/", content_type, fixed = TRUE) ||
      grepl("application/octet-stream", content_type, fixed = TRUE) ||
      grepl("application/pdf", content_type, fixed = TRUE)) {
    return(body)
  }
  
  # Parse JSON only when the API declares JSON content
  if (grepl("json", content_type, fixed = TRUE)) {
    txt <- rawToChar(body)
    
    if (!nzchar(txt)) {
      return(NULL)
    }
    
    parsed <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = simplify_json),
      error = function(e) NULL
    )
    
    return(if (is.null(parsed)) txt else parsed)
  }
  
  # Text-like responses stay as text
  if (grepl("text", content_type, fixed = TRUE) ||
      grepl("xml", content_type, fixed = TRUE) ||
      grepl("csv", content_type, fixed = TRUE) ||
      grepl("html", content_type, fixed = TRUE)) {
    return(rawToChar(body))
  }
  
  # Unknown content type
  if (is_image_bytes(body)) {
    return(body)
  }
  
  txt <- tryCatch(
    rawToChar(body),
    error = function(e) NULL
  )
  
  if (is.null(txt) || !validUTF8(txt)) {
    return(body)
  }
  
  if (!nzchar(txt)) {
    return(NULL)
  }
  
  txt
}

#' Creates Client Functions for Each Endpoint in an OpenAPI JSON Schema
#'
#'This function takes an OpenAPI JSON Schema URL or JSON list as input and creates client functions for each endpoint present. These functions are named identically to the endpoints and are stored in a named list. This function composes \pkg{httr2} client functions for any HTTP method by extracting the base URL, hostname, port and path from each API endpoint. 
#'
#' @param openapi The OpenAPI JSON schema. May be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. The \code{base_url} form supplies the host for a pre-parsed schema whose \code{servers[[1]].url} is relative or empty; a bare parsed list only works when that server URL is already absolute. Required.
#' @param token Character. Optional API authentication token. Defaults to the value of \code{Sys.getenv("ChatRBox_TOKEN")}), retrieved from the global environment. This default is also true for the \code{token} argument in \code{\link{get_api_client_fns}} (which is the equivalent to this function for base URLs rather than OpenAPI JSON schemas) since these are the helpers that extract client functions for a single API input. These functions later iterate over inputted URL lists so that users may upload multiple API services simultaneously.
#' @param auth_scheme Character. API authorization scheme. Defaults to \code{NULL}, given \code{token_header} is populated. The user-facing R6 object \code{\link{ChatRBox}} only contains the \code{token_header} argument but users may choose authorization methods via \code{auth_scheme} by modifying this function.    
#' @param token_header Character. API token header name if \code{auth_scheme} is \code{NULL}. Defaults to "X-API-Key".
#' @param httr2_config List. Optional named list of \pkg{httr2} request options applied to every generated client request, with elements \code{timeout} (seconds), \code{retry} (a list of arguments forwarded to \code{httr2::req_retry}, e.g. \code{list(max_tries = 2)}) and \code{verbose} (logical). Defaults to an empty list, in which case the package defaults from \code{\link{normalize_httr2_config}} are used. The configuration is applied by \code{\link{apply_httr2_config}}.
#' @param config_env Environment. Optional environment used to cache the normalized \code{httr2_config} (under \code{config_env$httr2_config}) so it is computed once and shared by every client function generated from \code{openapi}. Defaults to \code{NULL}, in which case a fresh environment is created and populated internally.
#' @param base_url Character. Optional base host (\code{scheme://host[:port]}) supplied alongside a pre-parsed schema so that a relative or empty \code{servers[[1]].url} can be anchored to an absolute URL. Defaults to \code{NULL}. When \code{openapi} is a \code{list(spec = <parsed>, base_url = "https://host")}, the embedded \code{base_url} is used unless this argument is supplied. Ignored for URL inputs, whose host is derived from the fetch URL.
#' @return 
#' This function returns a nested environment containing generated client functions for each endpoint in \code{openapi}.
#' @details
#' This function is analogous to \code{\link{get_api_client_fns}} but does not require the \code{client_fns} API endpoint. Therefore, users may provide the chatbot with external services without API modification via OpenAPI JSON schemas. Hence, this function facilitates token usage to access protected APIs just like \code{\link{get_api_client_fns}} and \code{\link{get_api_client_env}}. Notably, separate token and token header lists are required for APIs loaded as service URLs or OpenAPI JSON schemas.  
#' 
#' Every returned client function is named with the endpoint name and contains default parameters. The contents of the returned environment is collated with the client functions contained within \code{client_fns} endpoints in APIs provided using \code{services_list}. The aggregated list is then interpolated into the AI prompt such that chatbots may use any client function to answer input questions. This function acts as the main function to generate client functions but is used as a helper in \code{\link{build_api_client_env}}. The latter is used in other ChatRBox functions like \code{\link{object_generate}} and \code{\link{property_generate}} to iterate over URL lists.
#' @importFrom httr2 request req_headers req_perform req_method req_url_query req_body_json req_body_raw resp_content_type resp_body_raw resp_body_string url_parse
#' @importFrom jsonlite fromJSON
#' @seealso \code{\link{build_api_client_env}}, \code{\link{decode_api_response}}
#' @example man/examples/examples_client_generation.R
#' @export
build_api_client <- function(openapi = "",
                             token = Sys.getenv("ChatRBox_TOKEN", NA),
                             auth_scheme = NULL,
                             token_header = NULL,
                             httr2_config = list(),
                             config_env = NULL,
                             base_url = NULL) {
  
  if (is.null(config_env)) {
    config_env <- new.env()
  }
  
  if (is.null(config_env$httr2_config)) {
    config_env$httr2_config <- ChatRBox::normalize_httr2_config(httr2_config)
  }
  
  if (is.list(openapi) && !is.null(openapi$spec) && is.list(openapi$spec)) {
    if (is.null(base_url)) {
      base_url <- openapi$base_url
    }
    openapi <- openapi$spec
  }
  
  acquired <- acquire_openapi(
    openapi = openapi,
    base_url = base_url,
    token = token,
    auth_scheme = auth_scheme,
    token_header = token_header,
    config_env = config_env
  )
  
  base_url <- resolve_base_url(
    acquired$spec,
    acquired$spec_base,
    acquired$spec_base_path
  )
  
  generate_client_fns(
    spec = acquired$spec,
    base_url = base_url,
    token = token,
    auth_scheme = auth_scheme,
    token_header = token_header,
    config_env = config_env
  )
}

#' Resolves the Absolute Base URL from a Parsed OpenAPI Schema
#'
#' Helper that derives an absolute base URL from an already-parsed OpenAPI schema plus optional host context. Handles \code{servers[[1]]} with \code{{variable}} templating, anchoring of relative/empty server URLs against a supplied host, and the legacy \code{schemes} and \code{host} fallback.
#'
#' @param spec List. A parsed OpenAPI schema (as produced by \pkg{jsonlite}).
#' @param spec_base Character or \code{NULL}. Optional host context (scheme://host[:port]) used to anchor a relative or empty server URL.
#' @param spec_base_path Character or \code{NULL}. Optional base path derived from the same host context, used when the server URL is not root-anchored.
#' @return A character string containing an absolute base URL.
#' @keywords internal
#' @noRd
resolve_base_url <- function(spec, spec_base = NULL, spec_base_path = NULL) {
  
  # Determine base URL.
  if (!is.null(spec$servers) && length(spec$servers) >= 1L) {
    server <- spec$servers[[1]]
    base_url <- server$url
    
    if (!is.null(server$variables)) {
      for (var_name in names(server$variables)) {
        base_url <- gsub(
          paste0("{", var_name, "}"),
          server$variables[[var_name]]$default,
          base_url,
          fixed = TRUE
        )
      }
    }
    
    if (!grepl("^https?://", base_url) && !is.null(spec_base)) {
      if (startsWith(base_url, "/")) {
        base_url <- paste0(spec_base, base_url)
      } else {
        base_url <- paste0(spec_base, "/", spec_base_path, "/", base_url)
      }
      
      base_url <- sub("^(https?:)/+", "\\1//", base_url)
      base_url <- gsub("(?<!:)/{2,}", "/", base_url, perl = TRUE)
    }
    
  } else if (!is.null(spec$schemes) && !is.null(spec$host)) {
    base_url <- paste0(
      spec$schemes[[1]],
      "://",
      spec$host,
      {
        bp <- or_null(spec$basePath, "")
        if (nzchar(bp) && !startsWith(bp, "/")) paste0("/", bp) else bp
      }
    )
  } else {
    stop("Cannot determine base URL from OpenAPI schema")
  }
  
  if (!grepl("^https?://", base_url)) {
    if (is.null(spec_base)) {
      stop(
        "Cannot resolve an absolute base URL from the pre-parsed OpenAPI ",
        "schema: servers[[1]].url is '", base_url, "'. Supply the host via ",
        "list(spec = <parsed>, base_url = \"https://host\") or provide an ",
        "absolute servers URL in the schema."
      )
    }
    stop("Invalid base URL: '", base_url, "'. Must start with http:// or https://")
  }
  
  base_url
}

#' Acquires an OpenAPI Schema and Host Context
#'
#' Spec-acquisition adapter used by \code{\link{build_api_client}}. Fetches and parses a URL, or wraps an already-parsed schema list, always returning the same shape so downstream base-URL resolution and client generation are independent of how the spec was delivered.
#'
#' @param openapi Character URL or a parsed OpenAPI schema list.
#' @param base_url Character or \code{NULL}. Optional host (scheme://host[:port][/path]) supplied alongside a parsed schema so a relative or empty \code{servers[[1]].url} can be anchored.
#' @param token,auth_scheme,token_header Authentication inputs, forwarded to the fetch request for the URL adapter.
#' @param config_env Environment holding the normalized \pkg{httr2} config.
#' @return A list with elements \code{spec}, \code{spec_base} and \code{spec_base_path}.
#' @keywords internal
#' @noRd
acquire_openapi <- function(openapi,
                            base_url = NULL,
                            token = NULL,
                            auth_scheme = NULL,
                            token_header = NULL,
                            config_env = NULL) {
  
  # URL adapter fetches and parses schema to derive host context 
  if (is.character(openapi) && grepl("^https?://", openapi)) {
    req <- httr2::request(openapi)
    req <- apply_httr2_config(req, config_env$httr2_config)
    
    if (!is.null(token) && !is.na(token)) {
      if (!is.null(auth_scheme) && nzchar(auth_scheme)) {
        req <- httr2::req_headers(req, Authorization = paste(auth_scheme, token))
      } else if (!is.null(token_header) && nzchar(token_header)) {
        hdr <- stats::setNames(list(token), token_header)
        req <- do.call(httr2::req_headers, c(list(req), hdr))
      }
    }
    
    resp <- httr2::req_perform(req)
    
    spec <- jsonlite::fromJSON(
      httr2::resp_body_string(resp),
      simplifyVector = FALSE
    )
    
    parsed_url <- httr2::url_parse(openapi)
    spec_base <- paste0(parsed_url$scheme, "://", parsed_url$hostname)
    
    if (!is.null(parsed_url$port)) {
      spec_base <- paste0(spec_base, ":", parsed_url$port)
    }
    
    spec_base_path <- dirname(parsed_url$path)
    
    return(list(
      spec = spec,
      spec_base = spec_base,
      spec_base_path = spec_base_path
    ))
  }
  
  # List adapter wraps pre-parsed schema
  if (is.list(openapi)) {
    spec <- openapi
    
    if (!is.null(base_url)) {
      if (!is.character(base_url) ||
          length(base_url) != 1L ||
          !grepl("^https?://", base_url)) {
        stop("base_url must be a single URL starting with http:// or https://")
      }
      
      parsed_base <- httr2::url_parse(base_url)
      spec_base <- paste0(parsed_base$scheme, "://", parsed_base$hostname)
      
      if (!is.null(parsed_base$port)) {
        spec_base <- paste0(spec_base, ":", parsed_base$port)
      }
      
      spec_base_path <- or_null(parsed_base$path, "")
    } else {
      spec_base <- NULL
      spec_base_path <- NULL
    }
    
    return(list(
      spec = spec,
      spec_base = spec_base,
      spec_base_path = spec_base_path
    ))
  }
  
  stop("OpenAPI JSON schema must be a URL or JSON list")
}

#' Generates httr2 Client Functions from a Resolved OpenAPI Schema
#'
#' Pure client-generation stage used by \code{\link{build_api_client}}. Builds a named list of \pkg{httr2} client functions from an already-parsed schema and an already-resolved absolute base URL, so generation is independent of how the spec was acquired or how its base URL was resolved.
#'
#' @param spec List. A parsed OpenAPI schema.
#' @param base_url Character. An absolute base URL for every generated request.
#' @param token,auth_scheme,token_header Authentication inputs baked into each generated client function's defaults.
#' @param config_env Environment holding the normalized \pkg{httr2} config.
#' @return A named list of generated client functions.
#' @keywords internal
#' @noRd
generate_client_fns <- function(spec,
                                base_url,
                                token = NULL,
                                auth_scheme = NULL,
                                token_header = NULL,
                                config_env = NULL) {
  
  client_fns <- list()
  
  http_methods <- c(
    "get", "put", "post", "delete",
    "options", "head", "patch", "trace"
  )
  
  for (path in names(spec$paths)) {
    path_item <- spec$paths[[path]]
    methods <- intersect(names(path_item), http_methods)
    
    for (method in methods) {
      endpoint <- path_item[[method]]
      fun_name <- extract_client_name(path)
      
      if (fun_name %in% names(client_fns)) {
        fun_name <- paste0(fun_name, "_", tolower(method))
      }
      
      path_item_params <- or_null(path_item$parameters, list())
      operation_params <- or_null(endpoint$parameters, list())
      
      params <- c(path_item_params, operation_params)
      
      if (length(params) > 0L && !is.list(params[[1]])) {
        params <- list(params)
      }
      
      get_param <- function(typ) {
        Filter(function(p) identical(p$`in`, typ), params)
      }
      
      get_names <- function(pl) {
        vapply(pl, function(p) p$name, character(1))
      }
      
      path_params <- get_param("path")
      query_params <- get_param("query")
      
      path_param_names <- get_names(path_params)
      query_param_names <- get_names(query_params)
      
      method_upper <- toupper(method)
      has_request_body <- !is.null(endpoint$requestBody)
      body_required <- isTRUE(endpoint$requestBody$required)
      
      include_body <- has_request_body
      
      param_defaults <- list()
      
      for (p in c(path_params, query_params)) {
        n <- p$name
        
        if (!is.null(n) && nzchar(n)) {
          param_defaults[[n]] <- get_param_default(p)
        }
      }
      
      if (include_body) {
        if (body_required) {
          param_defaults[["body"]] <- quote(expr = )
        } else {
          param_defaults["body"] <- list(NULL)
        }
      }
      
      if (!is.null(token) && !is.na(token)) {
        param_defaults$token <- token
      }
      
      if (!is.null(auth_scheme) && !is.na(auth_scheme)) {
        param_defaults$auth_scheme <- auth_scheme
      }
      
      if (!is.null(token_header) && !is.na(token_header)) {
        param_defaults$token_header <- token_header
      }
      
      formal_args <- as.pairlist(param_defaults)
      formal_param_names <- names(param_defaults)
      
      fn <- function(base_url,
                     path,
                     path_param_names,
                     query_param_names,
                     method,
                     formal_args,
                     formal_param_names,
                     config_env) {
        
        force(base_url)
        force(path)
        force(path_param_names)
        force(query_param_names)
        force(method)
        force(formal_args)
        force(formal_param_names)
        force(config_env)
        
        fn_body <- function() {}
        
        body(fn_body) <- substitute({
          arg_list <- as.list(match.call())[-1]
          argnms <- names(arg_list)
          unnamed <- which(is.null(argnms) | argnms == "")
          
          if (length(unnamed) > 0L &&
              length(formal_param_names) >= length(arg_list)) {
            argnms[unnamed] <- formal_param_names[unnamed]
            names(arg_list) <- argnms
          }
          
          fn_env <- environment()
          for (fp in formal_param_names) {
            if (!fp %in% names(arg_list)) {
              val <- tryCatch(
                get(fp, envir = fn_env, inherits = FALSE),
                error = function(e) NULL
              )
              is_missing <- is.symbol(val) && !nzchar(as.character(val))
              if (!is.null(val) && !is_missing) {
                arg_list[[fp]] <- val
              }
            }
          }
          
          resolved_path <- path
          
          for (param in path_param_names) {
            if (!param %in% names(arg_list)) {
              stop("Missing path parameter: ", param)
            }
            
            resolved_path <- gsub(
              paste0("{", param, "}"),
              arg_list[[param]],
              resolved_path,
              fixed = TRUE
            )
          }
          
          http_method <- toupper(method)
          
          req <- httr2::request(paste0(base_url, resolved_path))
          req <- httr2::req_method(req, http_method)
          req <- apply_httr2_config(req, config_env$httr2_config)
          if (!is.null(arg_list$token) && !is.na(arg_list$token)) {
            if (!is.null(arg_list$auth_scheme) && nzchar(arg_list$auth_scheme)) {
              req <- httr2::req_headers(
                req,
                Authorization = paste(arg_list$auth_scheme, arg_list$token)
              )
            } else if (!is.null(arg_list$token_header) &&
                       nzchar(arg_list$token_header)) {
              hdr <- stats::setNames(list(arg_list$token), arg_list$token_header)
              req <- do.call(httr2::req_headers, c(list(req), hdr))
            }
          }
          
          query_values <- arg_list[names(arg_list) %in% query_param_names]
          
          if (length(query_values) > 0L) {
            query_values <- query_values[
              !vapply(query_values, is.null, logical(1))
            ]
          }
          
          if (length(query_values) > 0L) {
            req <- do.call(httr2::req_url_query, c(list(req), query_values))
          }
          
          # Only explicit body arguments become request body
          explicit_body <- if ("body" %in% names(arg_list)) {
            arg_list$body
          } else {
            NULL
          }
          
          if (!is.null(explicit_body)) {
            if (is.character(explicit_body) && length(explicit_body) == 1L) {
              req <- httr2::req_body_raw(
                req,
                charToRaw(explicit_body),
                type = "application/json"
              )
            } else if (is.raw(explicit_body)) {
              req <- httr2::req_body_raw(req, explicit_body)
            } else {
              req <- httr2::req_body_json(req, explicit_body)
            }
          }
          
          resp <- httr2::req_perform(req)
          decode_api_response(resp)
          
        }, list(
          path = path,
          path_param_names = path_param_names,
          query_param_names = query_param_names,
          method = method,
          base_url = base_url,
          formal_param_names = formal_param_names,
          config_env = config_env
        ))
        
        formals(fn_body) <- formal_args
        fn_body
      }
      
      client_fns[[fun_name]] <- fn(
        base_url = base_url,
        path = path,
        path_param_names = path_param_names,
        query_param_names = query_param_names,
        method = method,
        formal_args = formal_args,
        formal_param_names = formal_param_names,
        config_env = config_env
      )
    }
  }
  
  client_fns
}

#' Generates Client Functions for Each OpenAPI JSON Schema in a List
#'
#' This functions iterates over a list of OpenAPI JSON schemas and applies \code{\link{build_api_client}} to each, to generate client functions for every endpoint. These are stored in a named list of environments, whereby each environment is the output of \code{\link{build_api_client}} for a given OpenAPI JSON schema. Each schema may be supplied as an OpenAPI JSON URL, an already-parsed OpenAPI schema list, or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host, and all three forms may be mixed within the same \code{openapi_list}.   
#'
#' @param openapi_list List. A named list of OpenAPI JSON schemas. Each element's value may be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by parsing the JSON with \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. URL values are fetched and parsed, whereas pre-parsed schema values are used directly, avoiding a network fetch. The \code{list(spec=, base_url=)} form supplies the base host for a pre-parsed schema whose \code{servers[[1]].url} is relative or empty (as plumber2/FastAPI-style frameworks emit); a bare parsed list only works when that server URL is already absolute. All three forms may be mixed within the same list. Required.
#' @param token_openapi List. Named list of API authentication tokens, with names matching APIs in \code{openapi_list}. Each token is used only for the matching service via pair matching. Error messages will throw for mismatched, duplicated and missing names. Defaults to empty list. 
#' @param auth_scheme Character. API authorization scheme. Defaults to NULL, given \code{header_openapi} is populated. 
#' @param header_openapi Character. API token header name if \code{auth_scheme} is \code{NULL}. Defaults to "X-API-Key".
#' @param httr2_config List. Optional named list of \pkg{httr2} request options (\code{timeout}, \code{retry}, \code{verbose}) applied to every generated client request across all services in \code{openapi_list}. Defaults to an empty list, in which case the package defaults from \code{\link{normalize_httr2_config}} are used.
#' @param config_env Environment. Optional environment used to cache the normalized \code{httr2_config} so it is computed once and shared by every service built from \code{openapi_list}. Defaults to NULL, in which case a single shared environment is created internally and reused for each service.
#' @return 
#' A named list of environments, one for each service in \code{openapi_list}, where each environment contains the generated client functions for that service. Names correspond to those inputted in \code{openapi_list}. This nested environment structure prevents namespace conflict, especially for multiple external API services.
#' @details
#' This function applies \code{\link{build_api_client}} to each element in the inputted list of OpenAPI JSON files, wraps the resulting list of functions in an environment, and returns a list of these environments. Although \code{\link{build_api_client}} builds the relevant client functions, \code{\link{build_api_client}} acts as a helper to this function which is used when chatbots are initialized as R6 objects in  \code{\link{object_generate}}. 
#' 
#' This function may be used for OpenAPI JSON schemas that both require and do not require access tokens. When tokens are required, they may be passed to the \code{token_openapi} argument as a character string or environment variable, alongside the token header/scheme. Token and header lists are matched to services by name, so they apply identically whether a service is supplied as a URL or a pre-parsed schema list.  
#' @seealso \code{\link{build_api_client}}
#' @example man/examples/examples_client_generation.R
#' @export
build_api_client_env <- function(openapi_list,
                                 token_openapi = list(),
                                 auth_scheme = NULL,
                                 header_openapi = list(),
                                 httr2_config = list(),
                                 config_env = NULL) {
  
  if (length(openapi_list) == 0L) {
    return(list())
  }
  
  if (is.null(config_env)) {
    config_env <- new.env()
  }
  
  nm <- names(openapi_list)
  
  if (is.null(nm)) {
    stop("openapi_list must be a named list for name-based matching.")
  }
  
  check_names(token_openapi, "token_openapi", "openapi_list")
  check_names(header_openapi, "header_openapi", "openapi_list")
  
  validate_name_match(token_openapi, openapi_list, "token_openapi", "openapi_list")
  validate_name_match(header_openapi, openapi_list, "header_openapi", "openapi_list")
  
  check_duplicate_names(token_openapi, "token_openapi")
  check_duplicate_names(header_openapi, "header_openapi")
  
  env_list <- mapply(
    FUN = function(api_spec, api_name) {
      tk <- if (!is.null(names(token_openapi)) &&
                api_name %in% names(token_openapi)) {
        token_openapi[[api_name]]
      } else {
        NULL
      }
      
      th <- if (!is.null(names(header_openapi)) &&
                api_name %in% names(header_openapi)) {
        header_openapi[[api_name]]
      } else {
        NULL
      }
      
      api_base_url <- NULL
      if (is.list(api_spec) &&
          !is.null(api_spec$spec) && is.list(api_spec$spec)) {
        api_base_url <- api_spec$base_url
        api_spec <- api_spec$spec
      }
      
      fns <- build_api_client(
        openapi = api_spec,
        token = if (is.null(tk) || is.na(tk)) NULL else tk,
        token_header = if (!is.null(th) && !is.na(th)) th else NULL,
        auth_scheme = auth_scheme,
        httr2_config = httr2_config,
        config_env = config_env,
        base_url = api_base_url
      )
      
      list2env(fns)
    },
    api_spec = openapi_list,
    api_name = nm,
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  
  env_list
}
