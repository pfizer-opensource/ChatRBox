#' Builds Tool Documentation for AI Prompt Interpolation
#'
#' This function renders the help documentation of every tool function contained within a tools environment into a single Markdown-formatted character string. The result is intended for interpolation into the AI prompt via \code{\link{object_generate}}, providing the LLM with detailed reference documentation for each available tool alongside the service paths and arguments already supplied by \code{\link{get_function_path}} and \code{\link{get_function_args}}.
#'
#' For each tool function, the package documentation (Rd) is located and rendered to plain text. Where a function has no associated Rd documentation, or is not defined within a package namespace, the function falls back to its argument signature so that the LLM is never left without context.
#' @details
#' Documentation is extracted from the installed package Rd database using \code{tools::Rd_db} and rendered with \code{tools::Rd2txt}, so it reuses the tool author's own maintained documentation rather than duplicating it. The \code{\\examples} section is dropped by default to keep the interpolated prompt concise, and may be retained via \code{include_examples}. All curly braces in the assembled documentation are escaped exactly once so the returned string is safe to pass through \code{glue::glue()}, consistent with how \code{paths_string}, \code{args_string} and \code{past_outputs} are interpolated into \code{prompt_template} within \code{\link{object_generate}}. This function underpins the optional \code{tool_docs} argument of \code{\link{ChatRBox}}, \code{\link{object_generate}} and \code{\link{ChatRBox_update}}. When \code{tool_docs = TRUE}, the returned string is interpolated into the \code{{tool_docs}} placeholder of the default prompt template. Otherwise, the default prompt remains which only provides LLMs with service names and arguments. Notably, this function locates tool functions by searching the global environment, such that it was intended for package functions supplied to chatbots in R using \code{pkg::fn()} notation. Written functions and API endpoints do not have documentation suitable for rendering via this method.
#' @param tools_env Environment. Environment containing tool functions, as stored in the \code{tools_env} property of a \code{\link{ChatRBox_obj}} \code{S7} object. Defaults to \code{NULL}, which returns an empty string.
#' @param width Integer. Wrapping width passed to \code{tools::Rd2txt}. Defaults to \code{1000}.
#' @param include_examples Logical. Whether to retain the \code{\\examples} section of each tool's Rd documentation. Defaults to \code{FALSE} to keep the interpolated prompt concise.
#' @return A brace-escaped character string documenting each tool function, separated by horizontal rules. Returns \code{""} when \code{tools_env} is \code{NULL}, is not an environment, or contains no functions.
#' @seealso \code{\link{object_generate}}, \code{\link{get_function_path}}, \code{\link{get_function_args}}, \code{\link{load_prompt_template}}
#' @importFrom tools Rd_db Rd2txt
#' @importFrom utils capture.output
#' @example man/examples/examples_tool_docs.R
#' @export
build_tool_docs <- function(tools_env = NULL,
                            width = 1000L,
                            include_examples = FALSE) {
  
  if (is.null(tools_env) || !is.environment(tools_env)) return("")
  
  nms <- ls(envir = tools_env)
  nms <- nms[vapply(nms, function(n) is.function(get(n, envir = tools_env)),
                    logical(1))]
  if (length(nms) == 0L) return("")
  
  blocks <- vapply(nms, function(service_name) {
    fun  <- get(service_name, envir = tools_env)
    docs <- .tool_documentation(fun, width = width,
                                include_examples = include_examples)
    if (any_is_empty(docs)) {
      docs <- paste0("(No documentation available. Arguments: ",
                     paste(names(formals(fun)), collapse = ", "), ")")
    }
    paste0("### Service: `", service_name, "`\n\n", docs)
  }, character(1))
  
  .escape_braces(paste(blocks, collapse = "\n\n---\n\n"))
}

#' @noRd
.escape_braces <- function(text) {
  if (any_is_empty(text)) return("")
  text <- gsub("{", "{{", text, fixed = TRUE)
  text <- gsub("}", "}}", text, fixed = TRUE)
  text
}

#' Deliberately does NOT escape braces; that is done once in build_tool_docs().
#' @noRd
.clean_tool_docs <- function(text = NULL) {
  if (any_is_empty(text)) return("")
  text <- gsub("_\b", "", text, fixed = TRUE)
  text <- gsub("\b",  "", text, fixed = TRUE)
  text <- gsub("\n{3,}", "\n\n", text)
  trimws(text)
}

#' @noRd
.tool_documentation <- function(fun,
                                width = 1000L,
                                include_examples = FALSE) {
  if (!is.function(fun)) return(NULL)
  
  ns <- environment(fun)
  if (is.null(ns) || !isNamespace(ns)) {
    args <- paste(names(formals(fun)), collapse = ", ")
    return(.clean_tool_docs(paste0(
      "Arguments: ", if (nzchar(args)) args else "none",
      ". (No package documentation available.)")))
  }
  
  pkg <- getNamespaceName(ns)
  rd  <- tryCatch(.find_rd(pkg, fun), error = function(e) NULL)
  if (is.null(rd)) {
    args <- paste(names(formals(fun)), collapse = ", ")
    return(.clean_tool_docs(paste0(
      "Arguments: ", if (nzchar(args)) args else "none",
      ". (No Rd documentation in package ", pkg, ".)")))
  }
  
  if (!isTRUE(include_examples)) {
    keep <- vapply(rd, function(node) {
      tag <- attr(node, "Rd_tag", exact = TRUE)
      is.null(tag) || tag != "\\examples"
    }, logical(1))
    rd <- structure(rd[keep], class = class(rd), Rd_tag = attr(rd, "Rd_tag"))
  }
  
  txt <- utils::capture.output(
    tools::Rd2txt(rd, options = list(underline_titles = FALSE,
                                     width = width, code_quote = FALSE))
  )
  .clean_tool_docs(paste(txt, collapse = "\n"))
}

#' @noRd
.find_rd <- function(pkg, fun) {
  fun_name <- NULL
  ns <- asNamespace(pkg)
  for (nm in getNamespaceExports(pkg)) {
    obj <- tryCatch(get(nm, envir = ns), error = function(e) NULL)
    if (!is.null(obj) && identical(obj, fun)) {
      fun_name <- nm
      break
    }
  }
  if (is.null(fun_name)) return(NULL)
  db <- tryCatch(tools::Rd_db(pkg), error = function(e) NULL)
  if (is.null(db) || length(db) == 0L) return(NULL)
  aliases <- lapply(db, function(rd) {
    tags <- vapply(rd, function(node) {
      tag <- attr(node, "Rd_tag", exact = TRUE)
      if (is.null(tag)) "" else tag
    }, character(1))
    unlist(lapply(rd[tags == "\\alias"],
                  function(a) trimws(paste(unlist(a), collapse = ""))))
  })
  hit <- which(vapply(aliases, function(a) fun_name %in% a, logical(1)))
  if (length(hit) == 0L) return(NULL)
  db[[hit[1]]]
}
