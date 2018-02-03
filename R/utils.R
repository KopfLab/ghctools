# utilities functions ---

# File system functions ---

#' Find local repositories
#' 
#' @param prefix repository prefix (typically this would be a github classroom assignment prefix)
#' @param folder the target directory where to look for matching git repositories (searches recursively in all subdirectories)
#' @return paths to the found repositories (relative to the working directory unless param \code{folder} is an absolute path)
#' @export
ghc_find_local_repositories <- function(prefix, folder = ".") {
  
  if (missing(prefix)) stop("repository prefix required", call. = FALSE)
  found <- list.files(folder, pattern = glue("^{prefix}"), recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
  
  # check for dirs
  found <- found[map_lgl(found, ~dir.exists(file.path(.x, ".git")))]
  
  # info
  glue("Info: found {length(found)} git repositories with the prefix '{prefix}'") %>% 
    message()
  
  return(found)
}

# GraphQL functions --

#' Authenticate for GraphQL queries
#' 
#' Helper function to authenticate the GitHub account for running GraphQL (gql) queries. This function is exported for users who want to run custom GraphQL queries against the GitHub server but does not need to be called directly for standard use of the ghctools package.
#' 
#' @param token authentication token for github API (https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
#' @return \link[ghql]{GraphqlClient} object with authentication headers
#' @family GitHub query functions
#' @export
ghc_authenticate_gql <- function(token) {
  GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = add_headers(Authorization = paste0("Bearer ", token))
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
  results <- gql_client$exec(qry$queries[[query_name]])
  if (is.null(results$data)) {
    message("There were errors:")
    print(results$errors)
    stop("cannot process further", call. = FALSE)
  }
  
  return(results$data)
}