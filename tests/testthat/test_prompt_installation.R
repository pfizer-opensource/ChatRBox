# Tests internal_system.file
file <- internal_system.file("prompt", paste0("prompt", ".md"), 
                             package = "ChatRBox")

# Expect no error
testthat::expect_type(file, "character")
testthat::expect_length(file, 1)
testthat::expect_true(is.character(file) && length(file) == 1)
testthat::expect_true(grepl("\\.md$", file)) # file should end in .md

# Expect error for non-existent file name
testthat::expect_error(
  internal_system.file("prompt", paste0("nonexistent", ".md"),
                       package = "ChatRBox", mustWork = TRUE),
  regexp = "file"
)

# Tests load_prompt_template
template <- load_prompt_template(prompt_name = "prompt", 
                                 package = "ChatRBox")

# Expect no error
testthat::expect_type(template, "character")
testthat::expect_gt(nchar(template), 0) # Should not be empty
testthat::expect_length(template, 1)
testthat::expect_true(is.character(template))

# Expect error for non-existent prompt file name
testthat::expect_error(
  load_prompt_template(prompt_name = "nonexistent_prompt", 
                       package = "ChatRBox"),
  regexp = "file"
)

# Tests load_prompt_md
# Create a temp markdown file
tmp_md <- tempfile(fileext = ".md")
writeLines(c("# Title", "", "Hello"), tmp_md)

# Expect no error and correct return type/shape
prompt <- load_prompt_md(tmp_md)
testthat::expect_type(prompt, "character")
testthat::expect_length(prompt, 1)
testthat::expect_true(is.character(prompt))
testthat::expect_gt(nchar(prompt), 0)

# Expect exact file content (joined with newline)
testthat::expect_identical(
  prompt,
  paste(c("# Title", "", "Hello"), collapse = "\n")
)

# Expect error for non-existent file
testthat::expect_error(
  load_prompt_md(file.path(tempdir(), "definitely_nonexistent_prompt_file.md")),
  regexp = "Prompt file not found:"
)

# Expect it works with a relative path (uses working directory)
old_wd <- getwd()
testthat::expect_true(dir.exists(old_wd))
tmp_dir <- tempdir()
setwd(tmp_dir)
on.exit(setwd(old_wd), add = TRUE)

rel_file <- "relative_prompt.md"
writeLines(c("a", "b"), rel_file)

rel_prompt <- load_prompt_md(rel_file)
testthat::expect_type(rel_prompt, "character")
testthat::expect_length(rel_prompt, 1)
testthat::expect_identical(rel_prompt, "a\nb")
