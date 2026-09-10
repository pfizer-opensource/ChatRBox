# A dynamic error message outputs for any failed AI-facilitated do.call()
# This contains attempted client path and parameters, API HTTP error, httr2 
# configuration used by the client function and suggested next steps  

# Example HTTP error status
error_condition <- structure(
  list(status_code = 414),
  class = "error"
)

# Extracts error code to assign ChatRBox-specific guidance
status_code <- http_status_from_condition(error_condition)
status_code

# Assigns ChatRBox guidance to extracted code
guidance <- http_status_guidance(status_code)

# Example external API error message
api_error <- "URL too long"

# Final error message depicts both general API warning and assigned 
# ChatRBox guidance
error <- add_http_status_guidance(error_condition, api_error) 
