#' Extracts the Service Name from a Client Function Path
#'
#' This function determines the user-facing service name associated with a '$'-delimited client function path, as produced by \code{\link{get_function_path}}. The service key corresponds to the names a user supplies in \code{services_list}, \code{openapi_list} or \code{tools_list}, and is therefore the key used to match \code{summary_list}, tokens and API headers to a given service. Hence, this function facilitates name matching and is corroborated by \code{\link{check_names}}, \code{\link{check_duplicate_names}} and \code{\link{validate_name_match}} in both the AI summary and token workflows, to ensure robustness in accurate service name extraction. 
#'
#' Tool functions take the form \code{services$tools$<tool_name>}, so the service key is the tool name. API service client functions take the form \code{services$api_services$<service_name>$<client_fn>}, so the service key is the service name. For any other shape, the penultimate path segment is returned as a sensible fallback. Both these tool and client path structures are hard-coded within the ChatRBox package, such that \code{tools} and \code{api_services} are also hard-coded in this function and accompanied by the expected path lengths. Note that the tool and service names are user-supplied and used for name matching. Users are advised to mitigate AI hallucinations by choosing intuitive and descriptive service names, although additional detail may be included in the AI prompt via the R6 argument \code{prompt_template}. 
#' @param path Character. The '$'-delimited client function path. Defaults to NULL.
#' @return A character scalar containing the service key, or \code{NA_character_} when one cannot be determined.
#' @details
#' This function is used within \code{\link{llm_api_result}} to tag each API/tool result with its originating service key, so that \code{$talk()} calls can select the matching \code{summary_list} summary instruction for the follow-up summary call performed by \code{\link{llm_followup_summary}}. Whilst API service paths (those supplied as URLs or OpenAPI schema URLs) and tool paths differ in length, penultimate position indexing ensures that the user-supplied service name is extracted in both cases.
#' @seealso \code{\link{extract_path}}, \code{\link{get_function_path}}, \code{\link{llm_api_result}}, \code{\link{llm_followup_summary}}
#' @example man/examples/examples_summarize.R
#' @export
get_service_key <- function(path = NULL) {
  
  if (any_is_empty(path)) {
    return(NA_character_)
  }
  
  segments <- trimws(strsplit(path, "\\$")[[1]])
  n <- length(segments)
  
  if (n < 2) {
    return(NA_character_)
  }
  
  # Tools are defined in R and have one fewer nested environment levels than API services
  if (n >= 3 && segments[2] == "tools") {
    return(segments[n])
  }
  
  if (n >= 4 && segments[2] == "api_services") {
    return(segments[3])
  }
  
  # This indexing returns the user-supplied service name for both tools or APIs
  segments[n - 1]
}

#' Validates and Resolves a Named List of Summary Prompts
#'
#' This function validates a \code{summary_list} of per-service summary instructions against the available service keys and resolves each entry to a character string. Summary instructions may be supplied either as literal strings or as the path to a Markdown file (resolved via \code{\link{resolve_summary_prompt}}), much like the \code{prompt_template} argument of \code{\link{ChatRBox}}.
#'
#' Names are validated exactly as API tokens and headers are: every entry must be named (\code{\link{check_names}}), names must be unique (\code{\link{check_duplicate_names}}) and each name must match an available service key (\code{\link{validate_name_match}}). This mirrors the token/header workflow so that summary instructions are matched to the correct services by exact name.
#'
#' @param summary_list List. Named list of summary instructions, keyed by service name (tool name or API service name). Each value is a literal instruction string or the path to a Markdown file. Defaults to an empty list.
#' @param service_keys Character. Vector of available service keys to validate \code{summary_list} names against, typically the output of \code{\link{get_service_keys}} based on provided tools/API services. Defaults to \code{character(0)}.
#' @param existing List. Optional existing named list of resolved summary instructions to merge with (used during updates so entries are additive). Defaults to an empty list.
#' @return A named list of resolved summary instruction strings, with any \code{existing} entries retained unless overridden by name.
#' @details
#' Validation is skipped for an empty \code{summary_list}, in which case \code{existing} is returned unchanged. This imparts the default summary instructions onto every provided tool/API service if the \code{$talk()} call includes \code{summarize = TRUE}. The default summary prompt may be found under \code{summary_prompt.md} in the inst/prompt folder and ensures that the chatbot calls no further services, whilst remaining generalizable to all output types. New entries override existing entries with the same name, matching the additive, name-based update semantics used throughout \code{\link{object_update}}. Hence, AI-written summary instructions may be similarly updated for individual services without re-initialization using \code{\link{ChatRBox_update}}. 
#' @seealso \code{\link{get_service_keys}}, \code{\link{resolve_summary_prompt}}, \code{\link{check_names}}, \code{\link{validate_name_match}}, \code{\link{check_duplicate_names}}
#' @importFrom stats setNames
#' @importFrom utils modifyList
#' @example man/examples/examples_summarize.R
#' @export
process_summary_list <- function(summary_list = list(),
                                 service_keys = character(0),
                                 existing = list()) {
  
  if (is.null(existing)) existing <- list()
  
  # No user-supplied summary instructions leads to default prompting
  if (is.null(summary_list) || length(summary_list) == 0) {
    return(existing)
  }
  
  # Checks that name matching is valid and throws unique errors for inconsistencies
  ChatRBox::check_names(summary_list, "summary_list", "services")
  ChatRBox::check_duplicate_names(summary_list, "summary_list")
  
  service_list <- stats::setNames(
    as.list(service_keys),
    service_keys
  )
  ChatRBox::validate_name_match(summary_list, service_list,
                                "summary_list", "services")
  
  resolved <- lapply(summary_list, ChatRBox::resolve_summary_prompt)
  
  # Updates existing summary list with user-supplied argument
  utils::modifyList(existing, resolved)
}

#' Resolves Summary Prompts to Character Strings
#'
#' This function normalizes a single summary instruction into a character string. It is used so that summary prompts may be supplied either as literal character strings or as the path to a Markdown file, much like the \code{prompt_template} argument of \code{\link{ChatRBox}}.
#'
#' If \code{x} is a length-one character string ending in \code{.md} that points to an existing file, the file is read using \code{\link{load_prompt_md}} and its contents returned as a character string. Otherwise \code{x} is returned unchanged, allowing literal instruction strings to pass through untouched.
#' @param x Character. A single summary instruction, either a literal prompt string or the path to a Markdown file containing the prompt. Required.
#' @return A character string containing the resolved summary instruction.
#' @details
#' This helper underpins the \code{summary_list} argument of \code{\link{ChatRBox}}, \code{\link{object_generate}}, \code{\link{property_generate}}, \code{\link{object_update}} and \code{\link{ChatRBox_update}}, and the \code{instruction} argument of \code{\link{llm_followup_summary}}. It enables users to store per-service summary instructions in Markdown files (mirroring \code{prompt.md}) whilst still accepting inline strings.
#' @seealso \code{\link{load_prompt_md}}, \code{\link{llm_followup_summary}}, \code{\link{object_generate}}
#' @example man/examples/examples_summarize.R
#' @export
resolve_summary_prompt <- function(x) {
  
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("\\.md$", x, ignore.case = TRUE) && file.exists(x)) {
    return(ChatRBox::load_prompt_md(x))
  }
  x
}

#' Performs Follow-up AI Summary Call Using Past Service Outputs
#'
#' This function performs an additional AI call to produce a written AI-generated summary of the most recently retrieved API service/tool output, regardless of its type (e.g. data table, narrative text, list, vector, nested/JSON-like structure or any other R object). This utilizes the now-updated system prompt that contains the latest outputs in \code{past_outputs}. This function facilitates chained summary calls, whereby a chatbot may retrieve any object from one API and produce an analytical narrative about it within a single user query, without an associated summary API endpoint.
#'
#' @param chat Object. The underlying AI chat object exposing a \code{$chat(input)} method. Defaults to \code{NULL}.
#' @param input Character. The original user question, supplied to the follow-up call for context only. This is interpolated into the default AI summary prompt. Defaults to \code{NULL}.
#' @param instruction Character. Optional per-service summary instructions. Like the \code{prompt_template} argument of \code{\link{ChatRBox}}, these may be supplied either as a character string or as the path to a Markdown file, later resolved via \code{\link{resolve_summary_prompt}}. The default summary instruction is located in \code{summary_prompt.md} in the inst/prompt folder, and any per-service instructions are appended to this default prompt.
#' @param object_name Character. The name of a specific past service output (as stored in \code{object_env}) to summarize. When supplied, the follow-up call summarizes only \code{object_name}, enabling per-result summaries when multiple services were invoked in a single \code{$talk()} call. Defaults to \code{NULL}.
#' @return This function returns a plain-text AI-generated summary following \code{summarize = TRUE} assignment in \code{$talk()}, with any JSON code blocks removed.
#' @details
#' Since asking chatbots for a summary of recently retrieved API/tool outputs presented a common ChatRBox use-case, this was encapsulated into the single \code{$talk()} argument \code{summarize}. \code{summarize} takes logical values and calls \code{llm_followup_summary} when set to \code{TRUE}. This function appends an AI-generated summary of API/tool results to LLM responses to user questions. Unless service-specific summary prompts have been supplied via \code{summary_list} during chatbot initialization or update, these AI-generated summaries are informed by the default summary prompt located in \code{summary_prompt.md} in the inst/prompt folder. 
#' 
#' The default summary prompt has a similar structure to the default overall prompt in that it uses \pkg{glue} interpolation of dynamic variables and sequential, listed instruction. It informs chatbots that they are in 'summary mode', whereby they are forbidden from invoking further services and must produce a summary that provides structural descriptions of outputs, alongside any notable relationships and contextual interpretations. Additional prompting supplied via \code{summary_list} is appended to the default prompt to maintain these guidelines. Note that AI-generated summaries are only available for API and tool outputs that store within \code{object_env}. However, any data uploaded via \code{data_list} and stored within \code{data_env} is automatically available to chatbots. Hence, users may ask chatbots to summarize these data without employing the \code{summarize} argument. 
#' @seealso \code{\link{ChatRBox}}, \code{\link{llm_api_result}}, \code{\link{resolve_summary_prompt}}
#' @importFrom glue glue
#' @example man/examples/examples_summarize.R
#' @export
llm_followup_summary <- function(chat = NULL,
                                 input = NULL,
                                 instruction = NULL,
                                 object_name = NULL) {
  
  if (is.null(chat)) {
    stop("chat is NULL")
  }
  
  if (any_is_empty(input)) {
    stop("input is empty")
  }
  
  # Resolve instruction if provided 
  if (!is.null(instruction) && !any_is_empty(instruction)) {
    instruction <- ChatRBox::resolve_summary_prompt(instruction)
  } else {
    instruction <- ""  
  }
  
  # Default summary prompt stored in inst/prompt folder 
  summary_template <- ChatRBox::load_prompt_template("summary_prompt")
  base_instruction <- as.character(glue::glue(summary_template))
  
  followup_input <- paste(base_instruction, input)
  
  # Performs AI call that results in AI summary per service output when summarize = TRUE
  followup_response <- chat$chat(followup_input)
  
  # Extracts string summary response
  followup_text <- ChatRBox::code_remove(string = followup_response,
                                         language = "json")
  
  # Returns summary per service for output alongside named list
  followup_text
}

#' Parses a Model-driven Display Directive
#'
#' This function extracts and parses an optional \code{display} directive from an LLM response, as produced during the \code{summarize = "final"} synthesis workflow of \code{\link{llm_final_answer}}. The directive is a fenced \code{display} code block containing a JSON object that names which stored service/tool output(s) the user wants to see and how they should be shown. It provides a deterministic, machine-readable render intent that a downstream UI or consumer can honour however it wishes.
#'
#' @param string Character. The raw LLM response potentially containing a fenced \code{display} code block. Required.
#' @return A named list with elements \code{show} (a character vector of stored output names to reveal) and \code{how} (a character scalar display hint, one of \code{"link"}, \code{"inline"} or \code{"table"}), or \code{NULL} when no valid directive is present.
#' @details
#' The \code{display} tag is a supported structured output language (see \code{.SUPPORTED_CODE_LANGUAGES} and CONTRIBUTING.md), so the directive is extracted with the same \code{\link{code_extract}} infrastructure used for JSON tool calls. The extracted block is parsed as JSON via \code{\link[jsonlite]{fromJSON}}. A directive is only returned when it names at least one output to \code{show}; the \code{how} hint defaults to \code{"table"} when absent or unrecognized. Parsing is deterministic and never invokes further services. This parser underpins the model-driven display channel of \code{\link{llm_final_answer}}, whereby the synthesized answer stands on its own by default and raw outputs are surfaced only when the user explicitly asks to see them.
#' @seealso \code{\link{llm_final_answer}}, \code{\link{code_extract}}, \code{\link{code_remove}}
#' @importFrom jsonlite fromJSON
#' @example man/examples/examples_final_answer.R
#' @export
parse_display_directive <- function(string = NULL) {
  
  if (any_is_empty(string)) {
    return(NULL)
  }
  
  block <- tryCatch(
    ChatRBox::code_extract(string = string, language = "display"),
    error = function(e) NA_character_
  )
  
  if (any_is_empty(block)) {
    return(NULL)
  }
  
  directive <- tryCatch(
    jsonlite::fromJSON(block, simplifyVector = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(directive) || !is.list(directive) || is.null(directive$show)) {
    return(NULL)
  }
  
  show <- as.character(unlist(directive$show, use.names = FALSE))
  show <- show[nzchar(trimws(show))]
  
  if (length(show) == 0) {
    return(NULL)
  }
  
  how <- if (!is.null(directive$how) && !any_is_empty(directive$how)) {
    as.character(directive$how)[1]
  } else {
    "table"
  }
  
  valid_hints <- c("link", "inline", "table")
  if (!how %in% valid_hints) {
    how <- "table"
  }
  
  list(show = show, how = how)
}

#' Resolves the Display Channel of an LLM Response Into Clean Prose and Intent
#'
#' This function is the single source of truth for ChatRBox's model-driven display channel. It takes a raw LLM response, parses any optional \code{display} directive out of it (via \code{\link{parse_display_directive}}), strips every machine-readable fenced block (\code{display} and \code{json}) via \code{\link{code_remove}}, and returns the remaining prose with the parsed render intent attached as the \code{"display"} attribute.
#'
#' @param llm_response Character. The raw LLM response, which may contain fenced \code{display} and/or \code{json} blocks. Required.
#' @return A character scalar of clean prose (with all \code{display} and \code{json} fences removed and trimmed) carrying a \code{"display"} attribute, which is either a normalized render intent (see \code{\link{parse_display_directive}}) or \code{NULL} when no valid directive is present.
#' @details
#' ChatRBox owns the display channel end-to-end: it defines the \code{display} tag (see \code{.SUPPORTED_CODE_LANGUAGES}), the prompt that emits it and the parser that reads it. This resolver makes that guarantee \emph{structural} rather than \emph{contextual}: every response-producing path (the \code{summarize = "final"} synthesis in \code{\link{llm_final_answer}}, and the base dialogue of every \code{$talk()} turn, including conversational turns with no tool call) is routed through it, so a raw \code{display} block can never reach the user as prose regardless of whether a tool ran. The consumer boundary therefore sits cleanly at rendering only: given a non-\code{NULL} \code{"display"} intent (\code{list(show, how)}), the consumer decides how to present the named outputs.
#' @seealso \code{\link{parse_display_directive}}, \code{\link{llm_final_answer}}, \code{\link{code_remove}}
#' @export
resolve_display <- function(llm_response = NULL) {
  
  if (any_is_empty(llm_response)) {
    return(structure("", display = NULL))
  }
  
  # Parse the optional directive before any fences are stripped
  directive <- ChatRBox::parse_display_directive(llm_response)
  
  # Strip every machine-readable fenced block, leaving prose only
  prose <- ChatRBox::code_remove(string = llm_response, language = "display")
  if (any_is_empty(prose)) {
    prose <- ""
  } else {
    prose <- ChatRBox::code_remove(string = prose, language = "json")
    if (any_is_empty(prose)) {
      prose <- ""
    }
  }
  
  prose <- trimws(paste(prose, collapse = "\n"))
  
  structure(prose, display = directive)
}

#' Renders Stored Outputs Named by a Display Intent
#'
#' This function is the rendering consumer for ChatRBox's model-driven display channel. Given a value carrying a \code{"display"} attribute (as produced by \code{\link{resolve_display}} and attached to every return value of \code{\link{llm_final_answer}} and every \code{$talk()} turn), it looks up each named output in \code{object_env} and prints it according to the \code{how} hint (\code{"table"}, \code{"inline"} or \code{"link"}). When the intent is \code{NULL} (i.e. the model did not ask to reveal anything), the function is a no-op. This is the single rendering entry point: all \code{$talk()} return paths (final, non-final with result, non-final no-tool) route through it so that a non-\code{NULL} display intent is always honoured.
#'
#' @param answer Any value returned by \code{$talk()} (or \code{\link{llm_final_answer}}). Must carry a \code{"display"} attribute produced by \code{\link{resolve_display}}, or \code{NULL}. Required.
#' @param object_env Environment. The session object environment in which stored outputs live (the same environment passed to \code{\link{llm_final_answer}} and \code{\link{llm_api_result}}). Required.
#' @return Invisibly returns \code{answer}, called primarily for its side-effect of printing named stored outputs to the console.
#' @details
#' For each name in \code{intent$show}, the function checks existence via \code{exists(..., inherits = FALSE)} and, if found, prints the object using \code{print()} (for \code{"table"} and \code{"inline"}) or emits a note (for \code{"link"}). Names that are absent from \code{object_env} produce a \code{[!] '<name>' not found} note rather than an error, so a missing output never silently swallows other outputs or crashes the session.
#' @seealso \code{\link{resolve_display}}, \code{\link{parse_display_directive}}, \code{\link{llm_final_answer}}, \code{\link{ChatRBox}}
#' @example man/examples/examples_final_answer.R
#' @export
render_display <- function(answer, object_env) {
  
  intent <- attr(answer, "display")
  if (is.null(intent)) {
    return(invisible(answer))
  }
  
  show <- intent$show
  how  <- if (!is.null(intent$how)) intent$how else "table"
  
  for (nm in show) {
    if (exists(nm, envir = object_env, inherits = FALSE)) {
      obj <- get(nm, envir = object_env, inherits = FALSE)
      if (identical(how, "link")) {
        cat(sprintf("[%s]: stored in session (use object_env$%s to retrieve)\n", nm, nm))
      } else {
        # "table" and "inline" both print the object
        cat(sprintf("%s:\n", nm))
        print(obj)
        cat("\n")
      }
    } else {
      cat(sprintf("[!] '%s' not found in object_env\n", nm))
    }
  }
  
  invisible(answer)
}

#' Produces a Single Synthesized Final Answer Across All Turn Outputs
#'
#' This function performs one additional AI call that answers the user's original question by synthesizing across ALL service/tool outputs generated during a single \code{$talk()} turn, rather than summarizing each output separately. It is the engine behind the \code{summarize = "final"} synthesis mode and is a sibling to \code{\link{llm_followup_summary}} (which it does not modify or replace). Unlike per-output summaries, this produces a single natural-language answer string that reads like a real assistant reply and directly answers the original input.
#'
#' @param chat Object. The underlying AI chat object exposing a \code{$chat(input)} method. Defaults to \code{NULL}.
#' @param input Character. The original user question, synthesized against all outputs and interpolated into the final answer prompt. Required.
#' @param result List. The full named result set produced during the turn (the return value of \code{\link{llm_api_result}}). Used to determine whether any outputs exist to synthesize. Defaults to \code{NULL}.
#' @param object_env Environment. The object environment holding the stored service/tool outputs for the turn, serialized via \code{\link{env_to_str}} (which converts image bytes and \code{NULL} to placeholders). Defaults to \code{NULL}.
#' @param instruction Character. Optional additional synthesis instructions, supplied either as a character string or the path to a Markdown file (resolved via \code{\link{resolve_summary_prompt}}). Defaults to \code{NULL}.
#' @return A single answer string synthesizing across every turn output. When the model emits a model-driven display directive, the parsed render intent (see \code{\link{parse_display_directive}}) is attached to the answer as the \code{"display"} attribute; otherwise the attribute is \code{NULL}.
#' @details
#' This function loads the generalizable synthesis prompt \code{final_answer_prompt.md} from the inst/prompt folder and interpolates the original \code{input}, the serialized \code{past_outputs} (built from \code{object_env} via \code{\link{env_to_str}}) and any additional \code{instruction} using \pkg{glue}, mirroring the structure of the default prompt and \code{summary_prompt.md}. It then makes a single LLM call whose prose answer and render intent are obtained by routing the raw response through \code{\link{resolve_display}}, the single source of truth for the display channel (which strips any fenced JSON and \code{display} blocks and attaches the parsed directive).
#'
#' The prompt requires ONE holistic assistant reply that synthesizes across all outputs, never references the internal stored output names, and never narrates the individual service/tool calls. It forbids further service invocation, forbids raw output dumps, and requires that any \code{"Image output"} placeholder be treated as an unreadable plot described only via the producing tool and readable data, never as if its pixels were seen. By default the raw outputs are not surfaced; the answer stands on its own. When (and only when) the user explicitly asks to see a specific output, the model appends a fenced \code{display} directive (with no accompanying prose describing it) that is parsed by \code{\link{parse_display_directive}} into a machine-readable render intent for a downstream UI/consumer to honor. All stored outputs remain in \code{object_env}, so memory and chained-call resolution continue to work exactly as before.
#' @seealso \code{\link{llm_followup_summary}}, \code{\link{resolve_display}}, \code{\link{parse_display_directive}}, \code{\link{env_to_str}}, \code{\link{ChatRBox}}
#' @importFrom glue glue
#' @example man/examples/examples_final_answer.R
#' @export
llm_final_answer <- function(chat = NULL,
                             input = NULL,
                             result = NULL,
                             object_env = NULL,
                             instruction = NULL) {
  
  if (is.null(chat)) {
    stop("chat is NULL")
  }
  
  if (any_is_empty(input)) {
    stop("input is empty")
  }
  
  # Resolve instruction if provided
  if (!is.null(instruction) && !any_is_empty(instruction)) {
    instruction <- ChatRBox::resolve_summary_prompt(instruction)
  } else {
    instruction <- ""
  }
  
  # Serialize all stored turn outputs (image bytes/NULL become placeholders)
  past_outputs <- if (!is.null(object_env)) {
    ChatRBox::env_to_str(object_env)
  } else {
    ""
  }
  
  # Default final answer prompt stored in inst/prompt folder
  final_template <- ChatRBox::load_prompt_template("final_answer_prompt")
  final_instruction <- as.character(glue::glue(final_template))
  
  # One synthesis call across all turn outputs
  final_response <- chat$chat(final_instruction)
  
  # Resolve the display channel: strip fences to prose, attach parsed intent
  answer <- ChatRBox::resolve_display(final_response)
  
  answer
}
