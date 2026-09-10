# Simulates deployment of remote API service using the linear plotting API, 
# stored locally as plumber.R in inst/api_service 
proc <- start_api_service()

# Allows time for API service to launch in background R process
Sys.sleep(2) 

# Returns TRUE if process is created
proc$is_alive()

# Retrieves client functions from API URL (default host/port)
\dontrun{
  client_envir <- get_api_client_fns("http://127.0.0.1:8000")
  ls(client_envir)
}
