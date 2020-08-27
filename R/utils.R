# Utilities functions ======

# OS functions -----

# Find git repos
# @param prefix prefix for the folders
# @param folder which folder to search in
find_repositories <- function(prefix, folder = ".") {
  
  if (missing(prefix)) stop("repository prefix required", call. = FALSE)
  found <- list.files(folder, pattern = glue("^{prefix}"), recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
  
  # check for dirs
  found <- found[map_lgl(found, ~dir.exists(file.path(.x, ".git")))]
  
  # info
  glue("Info: found {length(found)} git repositories with the prefix '{prefix}'") %>% 
    message()
  
  return(found)
}

# Git functions -----

# Execute command in each folder
# Convenience function to execute a bash command in a list of folders.
# @param folders absolute paths or paths relative to current working directory
exec_command <- function(folders, command, info = TRUE, output = TRUE, indent = 6) {
  spaces <- rep(" ", indent) %>% glue::glue_collapse()
  map2(folders, 1:length(folders), function(folder, i) {
    if (info) 
      glue("Info: repository {i}/{length(folders)} ({folder})...") %>% message()
    cmd_output <- glue("cd \"{folder}\" && {command} 2>&1") %>% system(intern = TRUE)
    if (output && length(cmd_output) > 0) 
      message(spaces, glue::glue_collapse(cmd_output, sep = str_c("\n", spaces)))
  })
  invisible(NULL)
}

# GraphQL functions -----

#' Authenticate for GraphQL queries
#' 
#' Helper function to authenticate the GitHub account for running GraphQL (gql) queries. This function is exported for users who want to run custom GraphQL queries against the GitHub server but does not need to be called directly for standard use of the ghctools package. Note that the validity of the authentication token will only be tested once a query (\link{ghc_run_gql}) is run.
#' 
#' @param token authentication token for github API (https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
#' @return \link[ghql]{GraphqlClient} object with authentication headers
#' @family GitHub query functions
#' @export
ghc_authenticate_gql <- function(token) {
  GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = list(Authorization = paste0("Bearer ", token))
  )
}

#' Run a GrahQL query
#' 
#' Run a GraphQL query against the github graphql node. Stops if errors are encountered during query execution. This function is exported for users who want to run custom GraphQL queries against the GitHub server but does not need to be called directly for standard use of the ghctools package.
#' 
#' @param query GraphQL query
#' @param gql_client a GraphQL client object - will be generated based on the token by default
#' @param token authentication token (only needed if \code{gql_client} is not provided) 
#' @return returns the $data of the query
#' @family GitHub query functions
#' @export
ghc_run_gql <- function(query, query_name = "query", gql_client = ghc_authenticate_gql(token), token = NULL) {
  qry <- Query$new()
  qry$query(query_name, query)
  tryCatch(
    results <- gql_client$exec(qry$queries[[query_name]]),
    error = function(e) {
      if (str_detect(e$message, "Unauthorized")) 
        stop("invalid access token - ", e$message, call. = FALSE)
      else
        stop("could not execute GraphQL query - ", e$message, call. = FALSE)
    })
  # parse JSON
  results <- jsonlite::parse_json(results)
  # check for errors
  if (is.null(results$data)) {
    message("There were errors:")
    print(results$errors)
    stop("cannot process further", call. = FALSE)
  }
  return(results$data)
}