# Example httr2_config assignments for timeout, retry and verbose
# These may be defined during chatbot initialization
default <- list()

partial <- list(timeout = 60,
                verbose = TRUE)

custom = list(timeout = 45,
              retry = list(max_tries = 3),
              verbose = FALSE)

# httr2_config is normalized with default settings
# User preferences override default assignments
default_normal <- normalize_httr2_config(default)
default_normal

partial_normal <- normalize_httr2_config(partial)
partial_normal

custom_normal <- normalize_httr2_config(custom)
custom_normal 

# Normalized configurations are applied to httr2 request bodies during 
# client function generation
# This URL represents an OpenAPI JSON schema URL
req <- httr2::request("https://api.example.com/data")
constructed_body <- apply_httr2_config(req, custom_normal)
constructed_body 

# Normalized configurations are also formatted for error message display
custom_format <- format_httr2_config(custom_normal)
custom_format
