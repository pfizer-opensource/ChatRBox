# Example AI response using linear plotting API, 
# consisting of dialogue and code sections
response <- '```json
{ 
  "path": "services$api_services$plot$client_plot",
  "name": "linearplotting",
  "df": "example_data",
  "x_col": "",
  "y_col": "",
  "title": "",
  "api_url": "http://127.0.0.1:8000/plot",
  "token": null
}
```
The linear plotting API allows for the visualization of 
bivariate data sets, summarized by a line of best fit!'

# Extracts dialogue 
dialogue <- code_remove(string = response, language = "json")
dialogue 

# Extracts code
code <- code_extract(string = response, language = "json")
code

# Retrieves name key value
name <- get_name(json_str = code)
name

# Retrieves latter two components of path string
# This is used to create dynamic error message for failed do.call()
path <- extract_path("services$api_services$plot$client_plot")
path
