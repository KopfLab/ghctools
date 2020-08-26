# information functionality -----

#' Get local information on GitHub Classroom repositories
#' 
#' @param prefix repository prefix (typically this would be a github classroom assignment prefix)
#' @param folder the target directory where to look for matching git repositories (searches recursively in all subdirectories)
#' @return paths to the found repositories (relative to the working directory unless param \code{folder} is an absolute path)
#' @family information functions
#' @export
ghc_repos_get_local_information <- function(prefix, folder = ".") {
  
  found <- find_repositories(prefix, folder = folder)
  
  # FIXME: continue here
  # use git branch, git status -b --porcelain, etc.
  # to get information on active branch, all branches, status relative to origin (commits - 3 ahead/behind up to date), and modified files
  
  # return a data frame
  # use this information to build a real test scenario (test-scenario) for the package
  # add travis and appveyor
  # add documentation with packagedown
  # add badges that show [docs|link to docs], [cran], [travis], etc.
  
  return(found)
}

#' Get information on GitHub Classroom from GitHub
#' 
#' @inheritParams ghc_authenticate_gql
#' @param org organization name (i.e. organization github user)
#' @param prefix assignment prefix (i.e. repository prefix set for github classroom assignment)
#' @param max maximum number of repositories to retrieve information for
#' @return data frame with repository names, urls, last commits, last pull request
#' @family information functions
#' @export
ghc_repos_get_github_information <- function (prefix, org, token, max = 100) {
  
  # security checks
  if (missing(prefix)) stop("assignment prefix required", call. = FALSE)
  if (missing(org)) stop("organization required", call. = FALSE)
  if (missing(token)) stop("access token required for github queries", call. = FALSE)
  
  # run repositories query
  results <- 
    ghc_run_gql(
      query_name = "classroom_repos",
      token = token,
      query = str_interp(
"query{
  search(first: $[d]{max_n}, query: \"user:${user} ${prefix}\", type: REPOSITORY){
      repositoryCount
      nodes{
        ... on Repository {
          name
          createdAt
          pushedAt
          isPrivate
          pullRequests (last: 1) {
            nodes{
              publishedAt
              author {
                login
              }
            }
          }
          url
        }
      }
  }
}", list(max_n = max, user = org, prefix = prefix)))
  
  # check if got anything
  if (length(results$search$nodes) > 0) {
  
    # data
    results$search$nodes$pullRequests <- as.list(results$search$nodes$pullRequests$nodes)
    df <- results$search$nodes %>% 
      # dplyr data frame
      tbl_df() %>% 
      # add last pull request to data frame
      mutate(
        last_pr_created = map_chr(pullRequests, ~.x$publishedAt %||% NA),
        last_pr_login = map_chr(pullRequests, ~.x$author$login %||% NA)
      ) %>% 
      select(-pullRequests) %>% 
      # format date columns
      mutate_at(c("createdAt", "pushedAt", "last_pr_created"), ymd_hms) %>% 
      # names
      rename(
        private = isPrivate,
        repository = name,
        created = createdAt,
        last_pushed = pushedAt) %>% 
      # make sure only repositories that start with the prefix
      filter(str_detect(repository, str_c("^", prefix))) %>% 
      # url at the end
      select(repository, private, matches("[^(url)]"), url)
      
  } else {
    # no results for the query
    df <- data_frame()
  }
  
  # info 
  glue("Info: found {nrow(df)} repositories with the prefix '{prefix}' for the organization {org}.") %>% 
    message()
  
  # return data frame
  return(df)
}
