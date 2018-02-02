# information functionality -----

#' Find GitHub Classroom repositories for an assignment
#' 
#' @inheritParams ghc_authenticate_gql
#' @param org organization name (i.e. organization github user)
#' @param prefix assignment prefix (i.e. repository prefix set for github classroom assignment)
#' @param max maximum number of repositories to retrieve information for
#' @return data frame with repository names, urls, last commits, last pull requests
#' @export
ghc_find_assignment_repositories <- function (org, prefix, token, max = 100) {
  
  # security checks
  if (missing(org)) stop("organization required", call. = FALSE)
  if (missing(prefix)) stop("assignment prefix required", call. = FALSE)
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
      filter(grepl(str_c("^", prefix), repository)) %>% 
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
