#' @title Conversational AI R6 Class for Tool/API Chat Interactions
#'
#' @description
#' This \code{R6} class facilitates an AI response to an input question using available tools/API services. It encompasses a \code{ChatRBox_obj} \code{S7} object, generated using \code{\link{object_generate}}. The AI model used in this \code{R6} class may be defined using \code{ai_provider} in the form of a function. Hence, whilst this \code{R6} class was written to facilitate \pkg{ellmer} integration, any external or internal API function with a similar structure may be utilized provided necessary arguments are defined upon object initialization. These are included in \code{...}. As such, \code{ai_provider} acts as a required argument with no default AI service. 
#' 
#' This \code{R6} class enables conversation between the AI and user, whereby the chatbot may refer to past inputs and answer follow-up questions. A user must initialize the chat session with their input parameters using \code{ChatRBox$new()} before interacting with the chatbot using \code{$talk()}.
#'
#' @details
#' This is the \code{R6} object used to facilitate AI conversation with any AI model. A chat session may be initialized via \code{session <- ChatRBox$new()}. This will create the chat object using the inputted parameters. Chatbot interaction is facilitated via \code{session$talk()}, with an input string. Hence, a user may interact with a given chatbot indefinitely using \code{session$talk()}. Chatbot memory will only be lost upon re-initializing the session.
#' 
#' A user can impart any functionality to their chatbot using API services/tools, instruct the AI via \code{prompt_template} by referencing \code{args_string} and \code{paths_string} but also have back-and-forth conversation. 
#' 
#' The default \code{prompt_template} instructs AI parameter extraction for the most suitable tool/API service. AI prompting can be redefined in \code{prompt_template}, with reference to \code{args_string} and \code{paths_string}. See \code{\link{object_generate}} to view how these variables are created, used for prompting and stored in the \code{ChatRBox_obj} \code{S7} object. \code{prompt_template} has been loaded from a Markdown file in the inst/prompt folder to allow for easy modification.  
#'
#' @seealso \code{\link{object_generate}}
#' @family ChatRBox
#' @importFrom R6 R6Class
#' @name R6
#' @example man/examples/examples_initialize.R
NULL

#' @rdname R6
#' @export
# R6 class: user-facing session interface.
# See CONTRIBUTING.md for the OOP architecture rationale.
ChatRBox <- R6::R6Class(
  "ChatRBox",
  public = list(
    
    #' @field chat (`any`)\cr
    chat = NULL,
    
    #' @field chat_object (`ChatRBox_obj`)\cr
    #' S7 object storing tools/services and environments for the chat session.
    chat_object = NULL,
    
    #' @description
    #' Initializes a new ChatRBox session using the provided parameters.
    #'
    #' @param ai_provider (`function`)\cr
    #'   Function of chosen AI provider. Required.
    #' @param services_list (`list()`)\cr
    #'   Named list of API root URLs (without trailing slashes). Defaults to empty list.
    #' @param tools_list (`list()`)\cr
    #'   Named list of tool functions. Defaults to empty list.
    #' @param openapi_list (`list()`)\cr
    #'   Named list of OpenAPI JSON schemas. Each value may be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. The \code{base_url} form is required when a pre-parsed schema's \code{servers[[1]].url} is relative or empty; a bare parsed list only works when that server URL is already absolute. Defaults to empty list.
    #' @param data_list (`list()`)\cr
    #'   Named list of data frames. Defaults to empty list.
    #' @param tools_env (`environment()`)\cr
    #'   Environment to which tool functions are added. Defaults to \code{NULL} (creates a new environment).
    #' @param data_env (`environment()`)\cr
    #'   Environment to which data frames are added. Defaults to \code{NULL} (creates a new environment).
    #' @param object_env (`environment()`)\cr
    #'   Environment in which past API service/tool outputs are stored as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to \code{NULL} (creates a new environment).
    #' @param prompt_template (`character(1)`)\cr
    #'   Prompt template for the AI, for parameter extraction and user interaction. Defaults to the ChatRBox conversational template.
    #' @param summary_list (`list()`)\cr
    #'   Optional named list of instructions for AI-enabled summaries, assigned to specific services via name matching. Prompting may be provided via character strings or a Markdown file. Defaults to empty list.
    #' @param final_summary_prompt (`character(1)`)\cr
    #'   Single synthesis instruction steering \code{summarize = "final"}, as opposed to the per-service \code{summary_list} used by \code{summarize = TRUE}. Supplied as a literal string or the path to a Markdown file, resolved identically to \code{prompt_template}. Defaults to empty string.
    #' @param token_service (`list()`)\cr
    #'   Optional named list of API tokens for \code{services_list}. Defaults to empty list.
    #' @param header_service (`list()`)\cr
    #'   Optional named list of API authorization headers for \code{services_list}. Defaults to empty list.
    #' @param token_openapi (`list()`)\cr
    #'   Optional named list of API tokens for \code{openapi_list}. Defaults to empty list.
    #' @param header_openapi (`list()`)\cr
    #'   Optional named list of API authorization headers for \code{openapi_list}. Defaults to empty list.
    #' @param httr2_config (`list()`)\cr
    #'   Optional named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings. These only relate to services provided via \code{openapi_list}. 
    #' @param config_env (`environment()`)\cr
    #'   Optional environment caching the normalized \code{httr2_config} so it is shared by all generated client functions. Defaults to NULL, in which case it is created internally.
    #' @param ... (any)\cr
    #'   Additional named arguments passed on to \code{ai_provider}.    
    #' @return (`ChatRBox`)\cr
    #'   An \code{R6} object of class \code{ChatRBox}.
    initialize = function(
    ai_provider,
    services_list = list(),
    tools_list = list(),
    openapi_list = list(),
    data_list = list(),
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
    config_env = NULL,
    ...) {
      
      # Creates the ChatRBox_obj S7 object
      self$chat_object <- ChatRBox::object_generate(
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
      
      # Extracts AI prompt
      system_prompt <- ChatRBox::get_property(self$chat_object, "prompt")
      
      # Creates chat object
      self$chat <- ai_provider(
        system_prompt = system_prompt,
        ...)
      
    },
    
    #' @description
    #' Sends a user question to the chatbot and returns the AI's response and any applicable tool/API output.
    #'
    #' @param input (`character(1)`)\cr
    #'   User question to send to the chatbot.
    #' @param summarize (scalar logical or character)\cr
    #'   Controls the post-processing synthesis mode for service/tool outputs. This parameter may be a logical, whereby \code{TRUE} appends a per-output AI summary to every result via \code{\link{llm_followup_summary}}, or a character string. When \code{summarize = TRUE} for multiple service calls, summaries will be returned in a named list, optionally informed by per-service summarization prompts applied positionally from \code{summary_list}. Setting \code{summarize} to \code{final} as a character string rather produces a single answer to user questions by synthesizing the outputs of all services called during a given turn. Hence, raw outputs are not surfaced by default but remain stored in \code{obj_env} for memory, chaining or display upon user request. This summarization mode is most representative of mainstream chatbots as LLM responses are curated across a range of information, enabling greater interpretation of service outputs.
    #' @return (`any`)\cr
    #'   The AI's dialogue response and tool/API service output, if applicable. When \code{summarize = "final"}, a single length-one character answer is returned: the holistic synthesized answer when available, otherwise the display-resolved base dialogue. In \code{final} mode the return value always carries a \code{"display"} attribute that is either \code{NULL} or a normalized render intent \code{list(show, how)}, so a downstream consumer reads the intent from exactly one place on every final-mode turn (see \code{\link{resolve_display}}).
    talk = function(input,
                    summarize = FALSE) {
      
      # Validate the synthesis mode: logical TRUE/FALSE or the string "final"
      final_mode <- is.character(summarize) && length(summarize) == 1L &&
        identical(summarize, "final")
      if (!final_mode &&
          !(is.logical(summarize) && length(summarize) == 1L && !is.na(summarize))) {
        stop('summarize must be TRUE, FALSE or "final".')
      }
      
      invisible(capture.output({
        llm_response <- self$chat$chat(input)
      }))
      
      # Resolve the display channel on every turn
      base_dialogue <- ChatRBox::resolve_display(llm_response)
      ai_dialogue <- as.character(base_dialogue)
      
      # AI API service/tool response 
      result <- ChatRBox::llm_api_result(llm_response = llm_response,
                                         object = self$chat_object)
      
      ChatRBox::ChatRBox_update(self)
      
      # Final synthesis mode involves single answer across all turns
      if (final_mode) {
        
        object_env <- ChatRBox::get_property(self$chat_object, "object_env")
        final_summary_prompt <- ChatRBox::get_property(self$chat_object, "final_summary_prompt")
        final_answer <- ""
        if (!is.null(result) && length(result) > 0L) {
          final_answer <- tryCatch({
            invisible(capture.output({
              text <- ChatRBox::llm_final_answer(
                chat        = self$chat,
                input       = input,
                result      = result,
                object_env  = object_env,
                instruction = final_summary_prompt
              )
            }))
            text
          }, error = function(e) "")
        }
        
        out <- if (!is.null(final_answer) &&
                   nzchar(trimws(as.character(final_answer)))) {
          final_answer
        } else {
          base_dialogue
        }
        
        cat(as.character(out), "\n")
        ChatRBox::render_display(out, object_env)
        return(invisible(out))
      }
      
      # summarize mode appends AI summary to each service output
      followup_texts <- list()
      if (isTRUE(summarize) && !is.null(result) && length(result) > 0L) {
        
        result_names <- names(result)
        if (is.null(result_names)) {
          result_names <- as.character(seq_along(result))
        }
        
        # Extracts summary instructions per service
        service_keys <- attr(result, "service_keys")
        summary_list <- ChatRBox::get_property(self$chat_object, "summary_list")
        
        for (i in seq_along(result)) {
          nm <- result_names[i]
          
          # Matches summary prompt to intended service using name matching
          instruction <- NULL
          service_key <- if (!is.null(service_keys) && nm %in% names(service_keys)) {
            service_keys[[nm]]
          } else {
            NA_character_
          }
          if (!is.null(summary_list) &&
              !is.na(service_key) &&
              !is.null(summary_list[[service_key]])) {
            instruction <- summary_list[[service_key]]
          }
          
          # Creates AI-enabled summary
          followup_texts[[nm]] <- tryCatch({
            invisible(capture.output({
              text <- ChatRBox::llm_followup_summary(
                chat        = self$chat,
                input       = input,
                instruction = instruction,
                object_name = nm
              )
            }))
            text
          }, error = function(e) NULL)
        }
      }
      
      # AI dialogue
      cat(ai_dialogue, "\n")
      
      # Results 
      if (!is.null(result) && length(result) > 0L) {
        
        result_names <- names(result)
        if (is.null(result_names)) {
          result_names <- as.character(seq_along(result))
        }
        
        for (i in seq_along(result)) {
          print(result[i])
          cat("\n")
          
          if (isTRUE(summarize)) {
            nm <- result_names[i]
            this_summary <- followup_texts[[nm]]
            if (!is.null(this_summary) && nzchar(trimws(this_summary))) {
              cat(this_summary, "\n\n")
            }
          }
        }
      }
      
      # Return result invisibly 
      display_intent <- attr(base_dialogue, "display")
      object_env <- ChatRBox::get_property(self$chat_object, "object_env")
      if (!is.null(result)) {
        attr(result, "display") <- display_intent
        ChatRBox::render_display(result, object_env)
        invisible(result)
      } else {
        ChatRBox::render_display(base_dialogue, object_env)
        invisible(base_dialogue)
      }
    }
  )
)

#' Updates Conversational AI R6 Class with Parameters
#'
#' @param object Object. \code{ChatRBox} \code{R6} object to update in place. Required.
#' @param services_list List. Named list of API root URLs (without trailing slashes). Defaults to empty list.  
#' @param tools_list List. Named list of tool functions. Defaults to empty list. 
#' @param data_list List. Named list of data frames. Defaults to empty list.
#' @param openapi_list List. Named list of OpenAPI JSON schemas. Each value may be an OpenAPI JSON URL (character string), a pre-parsed OpenAPI schema list (as produced by \pkg{jsonlite}), or a \code{list(spec = <parsed schema>, base_url = "https://host")} pairing a pre-parsed schema with its base host. The \code{base_url} form is required when a pre-parsed schema's \code{servers[[1]].url} is relative or empty; a bare parsed list only works when that server URL is already absolute. Defaults to empty list. 
#' @param tools_env Environment. Tool functions are added to this environment. Defaults to current \code{S7} object tools environment.
#' @param data_env Environment. Data frames are added to this environment. Defaults to current \code{S7} object data environment.
#' @param object_env Environment. Past API service/tool outputs are stored in this environment as named R objects of any type, used to resolve chained API calls and to inform the AI prompt via \code{past_outputs}. Defaults to current \code{S7} object object environment.
#' @param prompt_template Character. General AI prompt for outputting parameter key-value pairs for available API services/tools. Defaults to current \code{S7} object prompt, likely interpolating function paths and arguments.
#' @param summary_list List. Optional named list of instructions for AI-enabled summaries, assigned to specific services via name matching. Prompting may be provided via character strings or a Markdown file. Defaults to empty list.
#' @param final_summary_prompt Character. Single synthesis instruction steering \code{summarize = "final"}, as opposed to the per-service \code{summary_list} used by \code{summarize = TRUE}. Supplied as a literal string or the path to a Markdown file, resolved identically to \code{prompt_template}. Updated additively; defaults to the current object value.
#' @param token_service Character. Optional named list of API tokens for \code{services_list}. Defaults to empty list.
#' @param header_service Character. Optional named list of API authorization headers for \code{services_list}. Defaults to empty list.
#' @param token_openapi Character. Optional named list of API tokens for \code{openapi_list}. Defaults to empty list.
#' @param header_openapi Character. Optional named list of API authorization headers for \code{openapi_list}. Defaults to empty list. 
#' @param httr2_config List. Optional named list of request options that override default configurations. Defaults to empty list, resulting in the default \code{timeout}, \code{retry} and \code{verbose} settings. These only relate to services provided via \code{openapi_list}.
#' @param config_env Environment. Optional environment caching the normalized \code{httr2_config} so it is computed once and shared by all generated client functions. Defaults to NULL, in which case it is created internally.   
#' @details
#' This function is used to update the interactive ChatRBox \code{R6} object with additional API services, tool functions, data frames or AI prompts. The tools and data environments can also be replaced. 
#' 
#' Updates are cumulative meaning only the service/tool to add need be listed as an argument, appending to the current object functionality. This function handles both the underlying \code{ChatRBox_obj} \code{S7} object and ChatRBox \code{R6} object. The \code{R6} object is updated using the \pkg{ellmer} method \code{set_system_prompt}, but only when the regenerated system prompt differs from the current one (retrieved via \code{get_system_prompt} where the provider exposes it). This avoids redundant prompt resets on every \code{$talk()} turn. 
#' 
#' Users may initialize a chat session using \code{session <- ChatRBox$new()}, interact with this session via \code{session$talk()} and update this session via \code{ChatRBox_update(session, ...)}. Since object updates do not involve re-initialization, the updated chatbot can be interacted with via \code{session$talk()} with reference to past conversation.    
#' @return Invisibly returns the updated \code{ChatRBox} \code{R6} object \code{object}, modified in place. Called primarily for its side effect of synchronizing the \code{S7} prompt onto the \code{R6} chat object.
#' @seealso \code{\link{property_generate}}, \code{\link{object_update}}
#' @example man/examples/examples_update.R
#' @export
ChatRBox_update <- function(object,
                            services_list = list(),
                            tools_list = list(),
                            openapi_list = list(),
                            data_list = list(),
                            tools_env = object$chat_object@tools_env,
                            data_env = object$chat_object@data_env,
                            object_env = object$chat_object@object_env,
                            prompt_template = object$chat_object@prompt_template,
                            summary_list = list(),
                            final_summary_prompt = object$chat_object@final_summary_prompt,
                            token_service = list(),
                            header_service = list(),
                            token_openapi = list(),
                            header_openapi = list(),
                            httr2_config = list(),
                            config_env = object$chat_object@config_env) {
  
  # Update ChatRBox_obj S7 object 
  object$chat_object <- ChatRBox::object_update(object = object$chat_object,
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
  
  # Extract updated AI prompt
  system_prompt_new <- ChatRBox::get_property(object$chat_object, "prompt")
  
  # Read the current system prompt from the R6 object
  system_prompt_cur <- tryCatch(
    object$chat$get_system_prompt(),
    error = function(e) NULL
  )
  
  # Only replaces prompt when updated != current  
  if (!identical(system_prompt_new, system_prompt_cur)) {
    object$chat$set_system_prompt(system_prompt_new)
  }
  
  invisible(object)
}
