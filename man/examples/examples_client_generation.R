# API services provided as API URLs are supplied via the services_list argument
# It is assumed that these APIs include an endpoint named client_fns 
# containing client function source code

# Hence, the following code concerns API OpenAPI schema URLs provided via 
# the openapi_list argument
# Client functions are automatically generated for these API services

# This is a fake OpenAPI JSON schema
synthetic_openapi <- list(
  openapi = "3.0.0",
  info = list(
    title = "Example API",
    version = "1.0.0"
  ),
  servers = list(
    list(url = "https://api.example.com/v1")
  ),
  paths = list(
    "/users" = list(
      get = list(
        summary = "List users",
        parameters = list(
          list(
            name = "page",
            `in` = "query",
            schema = list(type = "integer", default = 1)
          )
        ),
        responses = list(`200` = list(description = "Successful response"))
      ),
      post = list(
        summary = "Create a user",
        requestBody = list(
          required = TRUE,
          content = list(
            `application/json` = list(
              schema = list(type = "object")
            )
          )
        ),
        responses = list(`201` = list(description = "User created"))
      )
    ),
    "/users/{userId}" = list(
      get = list(
        summary = "Get user by ID",
        parameters = list(
          list(
            name = "userId",
            `in` = "path",
            required = TRUE,
            schema = list(type = "string")
          )
        ),
        responses = list(`200` = list(description = "User details"))
      )
    )
  )
)

# Builds client functions based on information within synthetic_openapi
client_fns <- build_api_client(synthetic_openapi)
client_fns

# Generated client functions are named based on endpoints
names(client_fns)

# ChatRBox generates client functions for every entry in a list of 
# OpenAPI schemas
openapi_list <- list(
  example_api = synthetic_openapi
)

# Every generated client function is stored in a nested environment 
client_env_list <- build_api_client_env(openapi_list)
ls(client_env_list$example_api)

# These helper functions are used during client function generation
# Extracts names for each endpoint by removing slashes and whitespace
extract_client_name("/ api/ v1/ user-profiles")

# Example parameter assignment within OpenAPI JSON schema
param <- list(
  name = "limit", 
  schema = list(default = 10)
)

# Extracts only the default parameters
# These are used by chatbots in the absence of user supplies
get_param_default(param)
