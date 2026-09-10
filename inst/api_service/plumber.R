# plumber.R
#
# Example API service for ChatRBox, written for the plumber2 package. plumber2 is the successor to plumber and introduces multiple breaking changes. For example, the query and body parameters are no longer passed as direct handler arguments, but accessed through `query` and `body`, and the response and request objects are named `response` and `request` respectively. Serializers and parsers are declared with tags and errors are raised with reqres `abort_*()` helpers.
#* @post /plot
#* @parser form
#* @parser json
#* @body table_text:string("") JSON string representing data frame. Required.
#* @body x_col:string("") Name of x column. Defaults to first column in bivariate data set.
#* @body y_col:string("") Name of y column. Defaults to second column in bivariate data set.
#* @body title:string("") Plot title
#* @serializer png
#* @description This is an example API endpoint that outputs a scatter plot between two numeric variables, with a line of best fit. These may be defined columns in a data frame but default to the first two numeric columns. This API service has a corresponding client function that is listed in the \code{client_fns} endpoint. As these are requirements for package compatibility, this plotting service acts as a guideline for providing plumber2 APIs to an AI chatbot via \code{services_list}. Although this process involves remote API access, \code{\link{start_api_service}} demonstrates the workflow via local deployment. In plumber2 the request body parameters are supplied through the \code{body} argument rather than as direct function arguments.  
plot <- function(body) {
  
  table_text <- body$table_text
  x_col <- body$x_col
  y_col <- body$y_col
  title <- body$title
  
  if (is.null(table_text) || table_text == "") plumber2::abort_bad_request("No data table provided.")
  df <- jsonlite::fromJSON(table_text)
  if (!is.data.frame(df)) plumber2::abort_bad_request("Input must be a data frame.")
  
  if (is.null(x_col) || x_col == "" || is.null(y_col) || y_col == "") {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) < 2) plumber2::abort_bad_request("Data frame must have at least two numeric columns.")
    if (is.null(x_col) || x_col == "") x_col <- num_cols[1]
    if (is.null(y_col) || y_col == "") y_col <- num_cols[2]
  }
  
  if (!(x_col %in% colnames(df))) plumber2::abort_bad_request(sprintf("Column '%s' not found.", x_col))
  if (!(y_col %in% colnames(df))) plumber2::abort_bad_request(sprintf("Column '%s' not found.", y_col))
  if (!is.numeric(df[[x_col]])) plumber2::abort_bad_request(sprintf("Column '%s' must be numeric.", x_col))
  if (!is.numeric(df[[y_col]])) plumber2::abort_bad_request(sprintf("Column '%s' must be numeric.", y_col))
  
  plot_df <- df[complete.cases(df[, c(x_col, y_col)]), c(x_col, y_col)]
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggplot2::labs(title = title, x = x_col, y = y_col)

  p
}

# This is the API client function that accesses the locally deployed plotting service. The listed URL includes the plotting endpoint. Notably, this includes a POST request and returns the raw image bytes. Image rendering is handled centrally by ChatRBox.
client_plot <- function(
    df,
    x_col = "",
    y_col = "",
    title = "",
    api_url = "http://127.0.0.1:8000/plot",
    token = NULL
) {
  
  body_list <- list(
    table_text = jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE),
    x_col = x_col,
    y_col = y_col,
    title = title
  )
  body_list <- body_list[body_list != ""] 
  
  req <- httr2::request(api_url)
  if (!is.null(token)) {
    req <- httr2::req_auth_bearer_token(req, token)
  }
  req <- do.call(httr2::req_body_form, c(list(req), body_list))
  req <- httr2::req_method(req, "POST")
  
  resp <- httr2::req_perform(req)
  
  if (httr2::resp_status(resp) != 200 ||
      !grepl("image/png", httr2::resp_content_type(resp))) {
    err <- tryCatch({
      content <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (is.list(content) && !is.null(content$error)) {
        content$error
      } else if (is.list(content) && !is.null(content$message)) {
        content$message
      } else {
        "Unknown error"
      }
    }, error = function(e) paste("API request failed with status", httr2::resp_status(resp)))
    stop("API request failed: ", err)
  }
  
  # Return raw image bytes
  httr2::resp_body_raw(resp)
}

#* @get /client_fns
#* @serializer text
#* @description This endpoint is a requirement for any plumber2 API provided to an AI chatbot. It outputs the source code for each client function which are subsequently made available to the chatbot. This endpoint must be named \code{client_fns} exactly when providing API services via \code{services_list}. 
client_fns <- function() {
  client_list <- c("client_plot")
  api_env <- parent.env(environment()) 
  client_code <- paste(
    lapply(client_list, function(fname) {
      paste(fname, "<-", paste(deparse(get(fname, envir = api_env)), collapse = "\n"))
    }),
    collapse = "\n\n"
  )
  client_code
}
