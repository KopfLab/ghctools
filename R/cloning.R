# add new functions
# - 1: copy a solutions file to all student repos
# - 2: push the master branch of all student repos
# - 3: create a submit branch based on a specific commit for all student repos and push the branch to origin
#     - git branch submit <commit-shae>
#     - git push origin submit
# - 4: MAYBE: together with #3, delete existing submit branches to re-create based on a specific commit
# - 5: when updating respositories, allow changes to be discarded
#     - git checkout -- .


#' Clone assignment repositories
#' 
#' Clone all students' repositories for a specific assignment. Token with private repo access privileges is required if the students' repositories are private. This function can also be used to update existing local repositories for an assignment (i.e. after initial cloning if the students have made updates in the mean time). If updates are not desired, set \code{pull=TRUE}.
#' 
#' @inheritParams ghc_find_assignment_repositories
#' @inheritParams ghc_clone_repositories
#' @return repositories data frame, see \link{ghc_find_repositories} for details
#' @export
ghc_clone_assignment_repositories <- function(org, prefix, folder = ".", token = NULL, pull = TRUE) {
  
  # security checks
  if (missing(org)) stop("organization required", call. = FALSE)
  if (missing(prefix)) stop("assignment prefix required", call. = FALSE)
  
  # find repositories
  ghc_find_assignment_repositories(org = org, prefix = prefix, token = token) %>% 
    # clone repos
    ghc_clone_repositories(folder = folder, token = token, pull = pull)
}

#' Clone repositories
#' @inheritParams ghc_clone_repository
#' @param repos data frame with, at minimum, columns 'repository' (name) and 'url'
#' @param folder the target directory where to clone all the repositories to
#' @return the \code{repos} data frame (unaltered) for easy piping
#' @export 
ghc_clone_repositories <- function(repos, folder = ".", token = NULL, pull = TRUE) {
  if (missing(repos)) stop("repos data frame required", call. = FALSE)
  if (nrow(repos) > 0) {
    if (!"url" %in% names(repos)) stop("missing column 'url'", call. = FALSE)
    if (!"repository" %in% names(repos)) stop("missing column 'repos'", call. = FALSE)
    pmap(
      list(url = repos$url, 
           path = file.path(folder, repos$repository), 
           token = token, 
           pull = pull
      ),
      ghc_clone_repository)
  }
  return(invisible(repos))
}

#' Clone a repository to the target directory
#' @param url the https://... repository 
#' @param path the directory to clone to, if not provided, clones into the current working directory with the repository name
#' @param token the authentication token (only required for private repositories if global credentials are not set)
#' @param pull if the repository already exists, whether to update it from the remote
#' @export
ghc_clone_repository <- function(url, path = NULL, token = NULL, pull = TRUE) {
  
  # token
  if (!is.null(token)) url <- str_c("https://", token, "@", str_replace(url, "^https://", "")) 
  if (is.null(path)) path <- basename(url)
  
  # parent directory
  if (!dir.exists(dirname(path)))  {
    message("Creating parent directory: ", dirname(path))
    dir.create(dirname(path), recursive = TRUE)
  } 
  
  # different cases
  if (!dir.exists(path)) {
    message(sprintf("'%s': cloning repository for the first time", basename(path)))
    github_call <- "git clone \"${url}\" \"${path}\""
  } else if (dir.exists(path) && pull) {
    message(sprintf("'%s': repository already exists -> pulling changes from remote", basename(path)))
    github_call <- "cd \"${path}\" && git pull \"${url}\""
  } else {
    message(sprintf("'%s': repository already exists but NOT pulling changes from remote -> no action", basename(path)))
    return(FALSE)
  } 
  
  # run github call
  github_call <- str_interp(github_call, list(path = path, url = url))
  invisible(system(github_call))
}
