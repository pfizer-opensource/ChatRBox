# ChatRBox API Integration Prompt

Use this prompt with an LLM to add the `/client_fns` endpoint framework to your plumber2 API for use with ChatRBox.

---
  
## Prompt for LLM:
  
I have an existing plumber2 API defined in `plumber.R` and I need to add the `/client_fns` endpoint framework so it can be used with ChatRBox.

### Current API Structure:
[Paste your current plumber.R file here, or describe your endpoints]

### Requirements:

1. **Add a `/client_fns` endpoint** to my `plumber.R` file that:
- Reads client functions from a file at `client/client_fns. R`
- Dynamically determines the API base URL from the request or environment variable
- Handles both production and local development scenarios
- Replaces the placeholder `<API_BASE_URL>` in the client functions with the actual URL
- Returns the code as plain text with proper headers

2. **Generate a `client/client_fns.R` file** that contains:
- R client functions using `httr2` for each of my API endpoints
- A base URL getter/setter function
- A utility function to build requests
- Proper roxygen documentation
- The placeholder `<API_BASE_URL>` in the default base URL

### Pattern to Follow:

**For plumber.R**, add this endpoint:
  ```r
# /client_fns -------------------------------------------------------------

#* Returns R client functions for this API
#* @get /client_fns
#* @serializer text
function(request) {
  code <- readLines(file.path("client", "client_fns.R"))
  
  # Get base URL from environment or construct from the request
  base_url <- Sys.getenv("API_BASE_URL", unset = NA)
  if (is.na(base_url) || base_url == "") {
    # In plumber2 the request is a reqres Request object. `request$url` gives
    # the full URL of the incoming request, so strip the endpoint path to
    # recover the API root. This works for both local dev and production.
    base_url <- sub("/client_fns.*$", "", request$url)
  }
  
  # Replace placeholder with actual URL
  collapsed_code <- paste(code, collapse = "\n")
  gsub("<API_BASE_URL>", base_url, collapsed_code)
}
```

**For client/client_fns. R**, create functions following this pattern:
  ```r
# Base URL management
my_api_base_url <- function(url = NULL) {
  if (! is.null(url)) options(my_api_base_url = url)
  getOption("my_api_base_url", "<API_BASE_URL>")
}

# Utility to build requests
. my_api_req <- function(path, query = list(), .. .) {
  base <- my_api_base_url()
  req <- httr2::request(base) %>%
    httr2::req_url_path_append(path) %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_options(...)
  if (! is.null(query) && length(query) > 0) {
    query <- query[nzchar(query)]
    if (length(query) > 0) {
      req <- httr2::req_url_query(req, !!!query)
    }
  }
  req
}

# Client function for each endpoint
my_endpoint_name <- function(param1 = '', param2 = '', ...) {
  req <- .my_api_req(
    path = "endpoint-path",
    query = list(param1 = param1, param2 = param2)
  )
  resp <- httr2::req_perform(req, ...)
  if (httr2::resp_status(resp) != 200) {
    stop("API error: ", httr2::resp_body_string(resp))
  }
  jsonlite::fromJSON(httr2::resp_body_string(resp))
}
```

### My API Endpoints:
[List your endpoints with their paths, parameters, and what they return]

Example:
- GET /my-endpoint: Parameters: x (string), y (string), Returns: JSON
- GET /another-endpoint: Parameters: id (string), Returns: JSON

### Additional Instructions:
- Use meaningful function names based on my endpoint purposes
- Include roxygen documentation for all exported functions
- Handle errors appropriately
- Ensure the `<API_BASE_URL>` placeholder is used exactly as shown
- For endpoints that return images, return the raw image bytes with `httr2::resp_body_raw(resp)` (decode only). ChatRBox renders images centrally and consistently, so client functions should not render images themselves.
- For endpoints that return CSV, handle appropriately

Please generate:
1. The complete `/client_fns` endpoint code to add to my plumber.R
2. The complete client/client_fns.R file with all client functions
3. Instructions for testing the setup

---
  
  ## Testing Your Setup:
  
  After implementation, test with:
  
  ```r
library(ChatRBox)

# Test locally
# 1. Start your API
pr <- plumber2::api("plumber.R")
plumber2::api_run(pr, port = 8000)

# 2. In another R session, fetch client functions
client_env <- ChatRBox::get_api_client_fns("http://localhost:8000")
ls(client_env)

# 3. Test using a client function
client_env$your_function_name(param1 = "value1")

# 4. Use with ChatRBox
session <- ChatRBox::ChatRBox$new(
  ai_provider = your_provider,
  services_list = list(my_service = "http://localhost:8000")
)

session$talk("Your question here")
```

## Environment Variable for Production:

When deploying, you can override the base URL:
  
  ```r
# In your . Rprofile or deployment configuration
Sys.setenv(API_BASE_URL = "https://your-production-domain.com/your-api-path")
```
