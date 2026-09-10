testthat::test_that("or_null returns a or b as expected", {
  testthat::expect_equal(or_null(NULL, 5), 5)
  testthat::expect_equal(or_null(10, 5), 10)
})

testthat::test_that("extract_client_name normalizes path string", {
  testthat::expect_equal(extract_client_name("/api/fooBar/123-test"), "api_foo_bar_123_test")
})

testthat::test_that("get_param_default returns the correct default value", {
  testthat::expect_equal(get_param_default(list(default = 5)), 5)
  testthat::expect_equal(get_param_default(list(schema = list(default = "hello"))), "hello")
})

testthat::test_that("build_api_client errors for invalid OpenAPI input", {
  testthat::expect_error(build_api_client(123), "OpenAPI JSON schema must be a URL or JSON list")
})

testthat::test_that("build_api_client errors for missing servers/host/schemes", {
  openapi_list <- list(paths = list())
  testthat::expect_error(build_api_client(openapi_list), "Cannot determine base URL from OpenAPI schema")
})

testthat::test_that("build_api_client_env errors on unnamed openapi_list", {
  testthat::expect_error(build_api_client_env(list("foo")), "openapi_list must be a named list")
})

testthat::test_that("build_api_client_env returns empty list for empty input", {
  testthat::expect_equal(build_api_client_env(list()), list())
})

test_that("build_api_client and build_api_client_env work with a local webfakes OpenAPI endpoint", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  # Minimal OpenAPI schema 
  openapi_spec <- jsonlite::toJSON(
    list(
      openapi = "3.0.0",
      servers = list(list(url = "")),  
      paths = list(
        "/echo" = list(
          get = list(
            summary = "Echo",
            parameters = list(
              list(name="msg", `in`="query", schema=list(type="string"), required=TRUE)
            ),
            responses = list(default = list(description="result"))
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
  
  # Set up fake web server
  app <- webfakes::new_app()
  app$get("/openapi.json", function(req, res) {
    res$set_type("application/json")$send(openapi_spec)
  })
  app$get("/echo", function(req, res) {
    res$set_type("application/json")$send(jsonlite::toJSON(
      list(msg = req$query$msg),
      auto_unbox = TRUE
    ))
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  # Patch OpenAPI servers url dynamically 
  openapi_url <- paste0(server$url(), "/openapi.json")
  openapi_list <- list(echo = openapi_url)
  
  # Test build_api_client() workflow 
  client_fns <- build_api_client(openapi = openapi_url)
  expect_true(is.list(client_fns), info = "build_api_client should return a list")
  expect_true("echo" %in% names(client_fns), info = "Client function for echo should exist in the list")
  expect_true(is.function(client_fns$echo), info = "echo should be a function")
  
  # Call generated client function 
  res1 <- client_fns$echo(msg = "hello")
  expect_equal(res1$msg, "hello")
  
  # Test build_api_client_env() workflow 
  env_list <- build_api_client_env(openapi_list = openapi_list)
  expect_true(is.list(env_list), info = "build_api_client_env should return a list")
  expect_true("echo" %in% names(env_list), info = "Environment for echo should exist in env_list")
  client_env <- env_list$echo
  expect_true(is.environment(client_env), info = "Each client_env should be an environment")
  
  # Check if function is present in the environment
  expect_true(exists("echo", envir = client_env), info = "echo should exist in the environment")
  expect_true(is.function(get("echo", envir = client_env)), info = "echo should be a function in env")
  
  # Call function from environment
  res2 <- client_env$echo(msg = "world")
  expect_equal(res2$msg, "world")
})

test_that("decode_api_response handles text-like and unknown content types", {
  text_body <- charToRaw("Sample text content")
  text_types <- c("text/plain", "application/xml", "text/csv", "text/html")
  
  for (content_type in text_types) {
    resp <- structure(
      list(
        headers = list("content-type" = content_type),
        body = text_body
      ),
      class = "httr2_response"
    )
    local_mocked_bindings(
      resp_content_type = function(...) content_type,
      resp_body_raw = function(resp) resp$body,
      is_image_bytes = function(...) FALSE
    )
    withr::local_options(
      "ChatRBox:::validUTF8" = function(...) TRUE
    )
    expect_equal(decode_api_response(resp), "Sample text content")
  }
})

testthat::test_that("build_api_client applies auth_scheme header when token is provided", {
  skip_if_not_installed("webfakes")
  
  openapi_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "http://localhost:3000")),
    paths = list(
      "/secure" = list(
        get = list(
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  captured_headers <- NULL
  
  local_mocked_bindings(
    req_headers = function(req, ...) {
      captured_headers <<- list(...)
      req
    },
    req_perform = function(req) {
      structure(
        list(headers = list("content-type" = "application/json"), body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_raw = function(resp) resp$body,
    resp_content_type = function(resp) "application/json",
    request = function(url) list(url = url),
    req_method = function(req, method) req
  )
  
  client_fns <- build_api_client(
    openapi = openapi_spec,
    token = "test-token-123",
    auth_scheme = "Bearer"
  )
  
  testthat::expect_true("secure" %in% names(client_fns))
})

testthat::test_that("build_api_client substitutes server variables in base URL", {
  openapi_spec <- list(
    openapi = "3.0.0",
    servers = list(
      list(
        url = "https://{environment}.api.example.com",
        variables = list(
          environment = list(
            default = "prod",
            enum = list("dev", "staging", "prod")
          )
        )
      )
    ),
    paths = list(
      "/endpoint" = list(
        get = list(
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  local_mocked_bindings(
    req_headers = function(req, ...) req,
    req_perform = function(req) {
      structure(
        list(headers = list("content-type" = "application/json"), body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_raw = function(resp) resp$body,
    resp_content_type = function(resp) "application/json",
    request = function(url) {
      testthat::expect_match(url, "prod.api.example.com", fixed = TRUE)
      list(url = url)
    },
    req_method = function(req, method) req
  )
  
  client_fns <- build_api_client(openapi = openapi_spec)
  
  testthat::expect_true("endpoint" %in% names(client_fns))
})

testthat::test_that("decode_api_response returns raw bytes for unknown content type with invalid UTF8", {
  invalid_utf8_body <- as.raw(c(0xFF, 0xFE, 0xFD, 0xFC))
  
  resp <- structure(
    list(
      headers = list("content-type" = "application/unknown"),
      body = invalid_utf8_body
    ),
    class = "httr2_response"
  )
  
  local_mocked_bindings(
    resp_content_type = function(...) "application/unknown",
    resp_body_raw = function(resp) resp$body,
    is_image_bytes = function(body) FALSE
  )
  result <- decode_api_response(resp)
  testthat::expect_type(result, "raw")
  testthat::expect_equal(result, invalid_utf8_body)
})

testthat::test_that("decode_api_response handles valid JSON body", {
  json_body <- charToRaw('{"key":"value"}')
  resp_json <- structure(
    list(headers = list("content-type" = "application/json"), body = json_body),
    class = "httr2_response"
  )
  
  local_mocked_bindings(
    resp_content_type = function(...) "application/json",
    resp_body_raw = function(resp) resp$body
  )
  
  result_json <- decode_api_response(resp_json)
  testthat::expect_type(result_json, "list")
  testthat::expect_equal(result_json$key, "value")
})

testthat::test_that("decode_api_response handles binary body", {
  binary_body <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  resp_binary <- structure(
    list(headers = list("content-type" = "application/octet-stream"), body = binary_body),
    class = "httr2_response"
  )
  
  local_mocked_bindings(
    resp_content_type = function(...) "application/octet-stream",
    resp_body_raw = function(resp) resp$body
  )
  
  result_binary <- decode_api_response(resp_binary)
  testthat::expect_type(result_binary, "raw")
  testthat::expect_equal(result_binary, binary_body)
})

testthat::test_that("generated client seeds token_header default into arg_list at call time", {
  openapi_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "http://localhost:3000")),
    paths = list(
      "/secure" = list(
        get = list(
          parameters = list(
            list(name = "question", `in` = "query",
                 required = TRUE, schema = list(type = "string"))
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  client_fns <- build_api_client(
    openapi = openapi_spec,
    token = "test-token-123",
    token_header = "Authorization"
  )
  captured_hdr <- NULL
  testthat::with_mocked_bindings(
    request       = function(url) list(url = url),
    req_method    = function(req, method) req,
    req_timeout   = function(req, ...) req,   
    req_retry     = function(req, ...) req,
    req_verbose   = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_headers   = function(req, ...) {
      captured_hdr <<- list(...)
      req
    },
    req_perform   = function(req) {
      structure(
        list(headers = list("content-type" = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_raw     = function(resp) resp$body,
    resp_content_type = function(resp) "application/json",
    .package = "httr2",
    {
      client_fns$secure(question = "hello")
    }
  )
  testthat::expect_false(is.null(captured_hdr))
  testthat::expect_true("Authorization" %in% names(captured_hdr))
  testthat::expect_equal(captured_hdr[["Authorization"]], "test-token-123")
})

testthat::test_that("generated client seeds auth_scheme default into arg_list at call time", {
  openapi_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "http://localhost:3000")),
    paths = list(
      "/secure" = list(
        get = list(
          parameters = list(
            list(name = "question", `in` = "query",
                 schema = list(type = "string"))
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  client_fns <- build_api_client(
    openapi = openapi_spec,
    token = "abc123",
    auth_scheme = "Bearer"
  )
  captured_hdr <- NULL
  testthat::with_mocked_bindings(
    request       = function(url) list(url = url),
    req_method    = function(req, method) req,
    req_timeout   = function(req, ...) req,
    req_retry     = function(req, ...) req,
    req_verbose   = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_headers   = function(req, ...) {
      captured_hdr <<- list(...)
      req
    },
    req_perform   = function(req) {
      structure(
        list(headers = list("content-type" = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_raw     = function(resp) resp$body,
    resp_content_type = function(resp) "application/json",
    .package = "httr2",
    {
      client_fns$secure(question = "q")
    }
  )
  testthat::expect_false(is.null(captured_hdr))
  testthat::expect_true("Authorization" %in% names(captured_hdr))
  testthat::expect_equal(captured_hdr[["Authorization"]], "Bearer abc123")
})

test_that("build_api_client_env builds client functions from a pre-parsed schema list", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  app <- webfakes::new_app()
  app$get("/echo", function(req, res) {
    res$set_type("application/json")$send(jsonlite::toJSON(
      list(msg = req$query$msg),
      auto_unbox = TRUE
    ))
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = server$url())),
    paths = list(
      "/echo" = list(
        get = list(
          summary = "Echo",
          parameters = list(
            list(name = "msg", `in` = "query",
                 schema = list(type = "string"), required = TRUE)
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  env_list <- build_api_client_env(openapi_list = list(echo = parsed_spec))
  expect_true(is.list(env_list))
  expect_true("echo" %in% names(env_list))
  client_env <- env_list$echo
  expect_true(is.environment(client_env))
  expect_true(is.function(get("echo", envir = client_env)))
  
  res <- client_env$echo(msg = "parsed")
  expect_equal(res$msg, "parsed")
})

test_that("build_api_client_env supports a mixed URL and pre-parsed schema list", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  openapi_spec <- jsonlite::toJSON(
    list(
      openapi = "3.0.0",
      servers = list(list(url = "")),
      paths = list(
        "/echo" = list(
          get = list(
            summary = "Echo",
            parameters = list(
              list(name = "msg", `in` = "query",
                   schema = list(type = "string"), required = TRUE)
            ),
            responses = list(default = list(description = "result"))
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
  
  app <- webfakes::new_app()
  app$get("/openapi.json", function(req, res) {
    res$set_type("application/json")$send(openapi_spec)
  })
  app$get("/echo", function(req, res) {
    res$set_type("application/json")$send(jsonlite::toJSON(
      list(msg = req$query$msg),
      auto_unbox = TRUE
    ))
  })
  app$get("/ping", function(req, res) {
    res$set_type("application/json")$send(jsonlite::toJSON(
      list(pong = req$query$val),
      auto_unbox = TRUE
    ))
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  url_spec <- paste0(server$url(), "/openapi.json")
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = server$url())),
    paths = list(
      "/ping" = list(
        get = list(
          summary = "Ping",
          parameters = list(
            list(name = "val", `in` = "query",
                 schema = list(type = "string"), required = TRUE)
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  env_list <- build_api_client_env(
    openapi_list = list(echo = url_spec, ping = parsed_spec)
  )
  
  expect_true(all(c("echo", "ping") %in% names(env_list)))
  expect_equal(env_list$echo$echo(msg = "from-url")$msg, "from-url")
  expect_equal(env_list$ping$ping(val = "from-parsed")$pong, "from-parsed")
})

testthat::test_that("build_api_client_env applies name-based token matching to a parsed-list element", {
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "http://localhost:3000")),
    paths = list(
      "/secure" = list(
        get = list(
          parameters = list(
            list(name = "question", `in` = "query",
                 required = TRUE, schema = list(type = "string"))
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  env_list <- build_api_client_env(
    openapi_list  = list(secure = parsed_spec),
    token_openapi = list(secure = "test-token-123"),
    header_openapi = list(secure = "Authorization")
  )
  
  captured_hdr <- NULL
  testthat::with_mocked_bindings(
    request       = function(url) list(url = url),
    req_method    = function(req, method) req,
    req_timeout   = function(req, ...) req,
    req_retry     = function(req, ...) req,
    req_verbose   = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_headers   = function(req, ...) {
      captured_hdr <<- list(...)
      req
    },
    req_perform   = function(req) {
      structure(
        list(headers = list("content-type" = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_raw     = function(resp) resp$body,
    resp_content_type = function(resp) "application/json",
    .package = "httr2",
    {
      env_list$secure$secure(question = "hello")
    }
  )
  testthat::expect_false(is.null(captured_hdr))
  testthat::expect_true("Authorization" %in% names(captured_hdr))
  testthat::expect_equal(captured_hdr[["Authorization"]], "test-token-123")
})

test_that("build_api_client builds a working client from list(spec=, base_url=) with relative server", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  app <- webfakes::new_app()
  app$get("/echo", function(req, res) {
    res$set_type("application/json")$send(jsonlite::toJSON(
      list(msg = req$query$msg),
      auto_unbox = TRUE
    ))
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list(
      "/echo" = list(
        get = list(
          summary = "Echo",
          parameters = list(
            list(name = "msg", `in` = "query",
                 schema = list(type = "string"), required = TRUE)
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  client_fns <- build_api_client(
    openapi = list(spec = parsed_spec, base_url = sub("/$", "", server$url()))
  )
  
  expect_true("echo" %in% names(client_fns))
  res <- client_fns$echo(msg = "anchored")
  expect_equal(res$msg, "anchored")
})

test_that("URL and list(spec=, base_url=) forms produce identical clients", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  spec_r <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list(
      "/echo" = list(
        get = list(
          summary = "Echo",
          parameters = list(
            list(name = "msg", `in` = "query",
                 schema = list(type = "string"), required = TRUE)
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  openapi_spec <- jsonlite::toJSON(spec_r, auto_unbox = TRUE)
  
  app <- webfakes::new_app()
  app$get("/openapi.json", function(req, res) {
    res$set_type("application/json")$send(openapi_spec)
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  url_fns <- build_api_client(openapi = paste0(server$url(), "/openapi.json"))
  parsed <- jsonlite::fromJSON(openapi_spec, simplifyVector = FALSE)
  parsed_fns <- build_api_client(
    openapi = list(spec = parsed, base_url = sub("/$", "", server$url()))
  )
  
  expect_equal(sort(names(url_fns)), sort(names(parsed_fns)))
  for (nm in names(url_fns)) {
    expect_equal(get_args_str(parsed_fns[[nm]]), get_args_str(url_fns[[nm]]))
  }
})

test_that("object_generate exposes clients from list(spec=, base_url=) in api_services", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  app <- webfakes::new_app()
  app$get("/echo", function(req, res) {
    res$set_type("application/json")$send(jsonlite::toJSON(
      list(msg = req$query$msg),
      auto_unbox = TRUE
    ))
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list(
      "/echo" = list(
        get = list(
          summary = "Echo",
          parameters = list(
            list(name = "msg", `in` = "query",
                 schema = list(type = "string"), required = TRUE)
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  obj <- object_generate(
    openapi_list = list(
      svc = list(spec = parsed_spec, base_url = sub("/$", "", server$url()))
    )
  )
  
  services <- get_property(obj, "services")
  expect_true("svc" %in% names(services$api_services))
  svc_env <- services$api_services$svc
  expect_true(is.function(get("echo", envir = svc_env)))
  expect_equal(svc_env$echo(msg = "viaobject")$msg, "viaobject")
})

testthat::test_that("build_api_client_env applies token matching to a list(spec=, base_url=) element", {
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list(
      "/secure" = list(
        get = list(
          parameters = list(
            list(name = "question", `in` = "query",
                 required = TRUE, schema = list(type = "string"))
          ),
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  env_list <- build_api_client_env(
    openapi_list  = list(
      secure = list(spec = parsed_spec, base_url = "http://localhost:3000")
    ),
    token_openapi = list(secure = "test-token-123"),
    header_openapi = list(secure = "Authorization")
  )
  
  captured_hdr <- NULL
  testthat::with_mocked_bindings(
    request       = function(url) list(url = url),
    req_method    = function(req, method) req,
    req_timeout   = function(req, ...) req,
    req_retry     = function(req, ...) req,
    req_verbose   = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_headers   = function(req, ...) {
      captured_hdr <<- list(...)
      req
    },
    req_perform   = function(req) {
      structure(
        list(headers = list("content-type" = "application/json"),
             body = charToRaw("{}")),
        class = "httr2_response"
      )
    },
    resp_body_raw     = function(resp) resp$body,
    resp_content_type = function(resp) "application/json",
    .package = "httr2",
    {
      env_list$secure$secure(question = "hello")
    }
  )
  testthat::expect_false(is.null(captured_hdr))
  testthat::expect_true("Authorization" %in% names(captured_hdr))
  testthat::expect_equal(captured_hdr[["Authorization"]], "test-token-123")
})

testthat::test_that("parsed spec with relative server and no base_url gives an actionable error", {
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list(
      "/echo" = list(
        get = list(
          responses = list(default = list(description = "result"))
        )
      )
    )
  )
  
  testthat::expect_error(
    build_api_client(openapi = parsed_spec),
    "base_url"
  )
  testthat::expect_error(
    build_api_client(openapi = parsed_spec),
    "servers"
  )
})

testthat::test_that("build_api_client rejects a non-URL base_url", {
  parsed_spec <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list()
  )
  
  testthat::expect_error(
    build_api_client(openapi = list(spec = parsed_spec, base_url = "not-a-url")),
    "base_url must be a single URL"
  )
})

testthat::test_that("resolve_base_url anchors a relative server URL to spec_base", {
  spec <- list(servers = list(list(url = "/chained_data")))
  testthat::expect_equal(
    resolve_base_url(spec, "https://host", "/api"),
    "https://host/chained_data"
  )
})

testthat::test_that("resolve_base_url returns an absolute server URL unchanged", {
  spec <- list(servers = list(list(url = "https://abs.example.com/v1")))
  testthat::expect_equal(resolve_base_url(spec), "https://abs.example.com/v1")
})

testthat::test_that("resolve_base_url anchors an empty server URL to supplied spec_base", {
  spec <- list(servers = list(list(url = "")))
  testthat::expect_equal(
    resolve_base_url(spec, "https://host:3000", ""),
    "https://host:3000/"
  )
})

testthat::test_that("resolve_base_url substitutes server variables", {
  spec <- list(servers = list(list(
    url = "https://{host}/v{ver}",
    variables = list(
      host = list(default = "api.example.com"),
      ver = list(default = "2")
    )
  )))
  testthat::expect_equal(resolve_base_url(spec), "https://api.example.com/v2")
})

testthat::test_that("resolve_base_url uses the schemes + host fallback", {
  spec <- list(schemes = list("https"), host = "legacy.example.com", basePath = "api")
  testthat::expect_equal(
    resolve_base_url(spec),
    "https://legacy.example.com/api"
  )
})

testthat::test_that("resolve_base_url errors on a relative server with no spec_base", {
  spec <- list(servers = list(list(url = "/chained_data")))
  testthat::expect_error(resolve_base_url(spec), "base_url")
  testthat::expect_error(
    resolve_base_url(spec),
    "Cannot resolve an absolute base URL"
  )
})

testthat::test_that("resolve_base_url errors when no servers, schemes or host are present", {
  testthat::expect_error(
    resolve_base_url(list()),
    "Cannot determine base URL from OpenAPI schema"
  )
})

testthat::test_that("acquire_openapi list adapter derives host context from base_url", {
  spec <- list(servers = list(list(url = "")), paths = list())
  acq <- acquire_openapi(spec, base_url = "http://localhost:8000")
  testthat::expect_identical(acq$spec, spec)
  testthat::expect_equal(acq$spec_base, "http://localhost:8000")
})

testthat::test_that("acquire_openapi list adapter returns NULL host context with no base_url", {
  spec <- list(servers = list(list(url = "https://abs.example.com")), paths = list())
  acq <- acquire_openapi(spec)
  testthat::expect_identical(acq$spec, spec)
  testthat::expect_null(acq$spec_base)
  testthat::expect_null(acq$spec_base_path)
})

testthat::test_that("acquire_openapi rejects an invalid input type", {
  testthat::expect_error(
    acquire_openapi(123),
    "OpenAPI JSON schema must be a URL or JSON list"
  )
})

testthat::test_that("URL and list-adapter forms resolve to the same base URL", {
  skip_if_not_installed("webfakes")
  skip_on_cran()
  
  spec_r <- list(
    openapi = "3.0.0",
    servers = list(list(url = "")),
    paths = list()
  )
  openapi_spec <- jsonlite::toJSON(spec_r, auto_unbox = TRUE)
  
  app <- webfakes::new_app()
  app$get("/openapi.json", function(req, res) {
    res$set_type("application/json")$send(openapi_spec)
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  
  url_acq <- acquire_openapi(paste0(server$url(), "openapi.json"))
  parsed <- jsonlite::fromJSON(openapi_spec, simplifyVector = FALSE)
  list_acq <- acquire_openapi(parsed, base_url = sub("/$", "", server$url()))
  
  testthat::expect_equal(
    resolve_base_url(url_acq$spec, url_acq$spec_base, url_acq$spec_base_path),
    resolve_base_url(list_acq$spec, list_acq$spec_base, list_acq$spec_base_path)
  )
})
