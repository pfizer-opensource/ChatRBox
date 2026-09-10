#' Locates ChatRBox File in inst/ Folder
#' 
#' This function is used to locate the system prompt Markdown files during ChatRBox package loading. These are converted into strings and used in \code{\link{ChatRBox}} and \code{\link{object_generate}} as the \code{prompt_template} argument. Hence, users may easily customize prompts as simple Markdown files as to control AI model behavior.   
#'  
#' @param ... Path within inst/. Required. 
#' @param package Package name. Defaults to "ChatRBox".
#' @param mustWork Error if file not found. Defaults to FALSE.
#' @return inst/ file path as character string.
#' @examples
#' file_path <- internal_system.file("prompt", paste0("prompt", ".md"), package = "ChatRBox")
#' file_path
#' @export
internal_system.file <- function(..., 
                                 package = "ChatRBox", 
                                 mustWork = FALSE) {
  
  system.file(..., package = package, mustWork = mustWork)
}

#' Loads ChatRBox Prompt Templates from inst/ Folder as String
#' 
#' This function loads the ChatRBox system prompt Markdown files in inst/prompt as character strings. 
#' 
#' This function is used to define the \code{prompt_template} default values in \code{\link{ChatRBox}} and \code{\link{object_generate}}. This prompt includes reference to \code{paths_string}, \code{args_string} and \code{past_outputs}, which can be interpolated after \code{ChatRBox_obj} \code{S7} object generation due to string conversion. This function is used similarly for the \code{summary_list} argument in \code{\link{ChatRBox}}, whereby any service without an equivalently named summary prompt (assuming the \code{$talk()} argument \code{summarize} is set to \code{TRUE}) is assigned the default summary prompt also located in the inst folder. Hence, this enables users to provide extra summary information and specific AI-generated summary instructions per provided service.        
#' 
#' @param prompt_name Character. Name of the prompt file (without .md). Defaults to "prompt".
#' @param package Character. Package name. Defaults to "ChatRBox".
#' @return Character string of the prompt template Markdown file.
#' @examples
#' # Loads default prompt from inst/prompt
#' prompt <- load_prompt_template()
#' prompt
#' @export
load_prompt_template <- function(prompt_name = "prompt", 
                                 package = "ChatRBox") {
  
  file_path <- ChatRBox::internal_system.file("prompt", paste0(prompt_name, ".md"), package = package)
  
  if (!file.exists(file_path)) stop("Prompt file not found: ", file_path)
  
  paste(readLines(file_path, warn = FALSE), collapse = "\n")
}

#' Loads Markdown File using Absolute Path
#' 
#' This function loads Markdown files from local environments via absolute paths. It is intended to be used as the \code{prompt_template} argument during chatbot initialization, setting \code{path} as the path to a Markdown file with an AI prompt. 
#' 
#' \code{path} should not include the working directory as this is extracted and prepended to the inputted path using \code{normalizePath}. For example, the path to the default prompt in the ChatRBox package is \code{inst/prompt/prompt.md}, which would act as the \code{path} argument. This has no reference to working directories, usernames or project titles such that this function may be universally used. This function enables users to personalize their prompts with ease but it is advised that users append their desired instruction to the default prompt provided. The default prompt concerns the background process which is required for any API service or R tool function workflow.   
#' 
#' @param path Character. The path to the location of the Markdown file as a string; the file is presumed to contain an AI prompt. Hence, \code{path} must finish with the name of the desired Markdown file, including \code{.md}. Required.
#' @return The text contained in the specified Markdown file as a character string. When \code{\link{load_prompt_md}} is used inside a \code{\link{ChatRBox}} chatbot object to load a prompt, this string will not only enable AI instruction but have \code{paths_string}, \code{args_string} and \code{past_outputs} interpolated into it, given that these names are present and encased within \code{{}}. 
#' @export
load_prompt_md <- function(path) {
  
  display_path <- path
  abs_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(abs_path)) {
    stop(sprintf(
      "Prompt file not found: %s\nWorking directory: %s",
      display_path, getwd()
    ))
  }
  paste(readLines(abs_path, warn = FALSE), collapse = "\n")
}
