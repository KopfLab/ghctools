# repository commands ----

# non-git command functions ----

#' Copy a file or folder
#' 
#' Copy a file or folder to a number of repositories with the same prefix (typically those form the same assignment). Note that this function will overwrite existing files/folders that have the same name. It will also make sure that the copied file/folder is tracked by git (\code{\link{ghc_repos_git_add}}) unless parameter \code{git_add=FALSE}.
#' 
#' @inheritParams ghc_repos_get_local_information
#' @param from the path to the file (or folder) that is to be copied to the respositories (absolute path or relative to the current working directory)
#' @param to the relative path where in the target repositories the \code{from} file/folder should be copied to (default is the same name as the \code{from})
#' @param git_add whether to make sure the copied file/folder is tracked by git
#' @return returns the \code{prefix} invisibly to facilitate piping
#' @family command functions
#' @export
ghc_repos_copy_to <- function(prefix, from, to = basename(from), folder = ".", git_add = TRUE) {
  
  folders <- suppressMessages(find_repositories(prefix, folder = folder))
  if (!file.exists(from)) stop("'from' is not an existing file or folder", call. = FALSE)
  
  #info
  is_dir <- dir.exists(from)
  glue("Info: copying {if (is_dir) 'folder' else 'file'} '{from}' ",
       "to '{file.path('[REPO]', to)}' ",
       "{if (git_add) 'and adding it to git ' else ''}",
       "for {length(folders)} repositories (prefix '{prefix}'):") %>% 
    message()
  
  # folder files
  if (is_dir)
    from <- list.files(from, full.names = TRUE, recursive = TRUE)
  
  # copy files
  map2(folders, 1:length(folders), function(folder, i) {
    glue("Info: repository {i}/{length(folders)} ({folder})...") %>% message()
    to_path <- file.path(folder, to)
    if (is_dir && !dir.exists(to_path)) dir.create(to_path, recursive = TRUE)
    file.copy(from = from, to = to_path, recursive = is_dir, overwrite = TRUE)
    if (git_add) exec_command(folder, glue("git add -f \"{to}\""), info = FALSE)
  })
  
  return(invisible(prefix))
}

# general git command function ----

#' Run a git command
#' 
#' Generic function to run a git command in a number of repositories with the same prefix (typically those form the same assignment).
#' 
#' @inheritParams ghc_repos_get_local_information
#' @param command a git command ("git" prefix will be automatically added if not present)
#' @return returns the \code{prefix} invisibly to facilitate piping
#' @family command functions
#' @export
ghc_repos_run_git_command <- function(prefix, command, folder = ".") {
  if (missing(command)) stop("git command required", call. = FALSE)
  folders <- suppressMessages(find_repositories(prefix, folder = folder))
  # make sure command has a git prefix
  command <- str_replace(command, "^git ", "") %>% str_c("git ", .)
  
  #info
  glue("Info: executing command '{command}' for {length(folders)} repositories (prefix '{prefix}'):") %>% 
    message()
  exec_command(folders, command)
  return(invisible(prefix))
}

# cloning commands ----

#' Clone assignment repositories
#' 
#' Clone all students' repositories for a specific assignment. Token with private repo access privileges is required if the students' repositories are private. This function can also be used to update existing local repositories for an assignment (i.e. after initial cloning if the students have made updates in the mean time). If updates are not desired, set \code{pull=TRUE}.
#' 
#' @inheritParams ghc_repos_get_github_information
#' @inheritParams ghc_clone_repositories
#' @inherit ghc_repos_run_git_command return
#' @family command functions
#' @return repositories data frame, see \link{ghc_repos_git_find_repositories} for details
#' @export
ghc_repos_git_clone <- function(prefix, org, folder = ".", token = NULL, pull = TRUE) {
  
  # security checks
  if (missing(org)) stop("organization required", call. = FALSE)
  if (missing(prefix)) stop("assignment prefix required", call. = FALSE)
  
  # find repositories
  ghc_repos_get_github_information(org = org, prefix = prefix, token = token) %>% 
    # clone repos
    ghc_clone_repositories(folder = folder, token = token, pull = pull)
  
  return(invisible(prefix))
}


#' Clone repositories
#' @inheritParams ghc_clone_repository
#' @param repos data frame with, at minimum, columns 'repository' (name) and 'url'
#' @param folder the target directory where to clone all the repositories to
#' @return the \code{repos} data frame (unaltered) for easy piping
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
#' @param pull if the repository already exists, whether to update it from the remote (see \code{\link{ghc_repos_git_pull}})
#' @return whether the repository was cloned/pulled
ghc_clone_repository <- function(url, path = NULL, token = NULL, pull = TRUE) {
  
  # token
  if (!is.null(token)) url <- str_c("https://", token, "@", str_replace(url, "^https://", "")) 
  if (is.null(path)) path <- basename(url)
  
  # parent directory
  if (!dir.exists(dirname(path)))  {
    message("Info: creating parent directory: ", dirname(path))
    dir.create(dirname(path), recursive = TRUE)
  } 
  
  # different cases
  folder <- basename(path)
  if (!dir.exists(path)) {
    message(glue("Info: '{folder}' - cloning repository for the first time"))
    cmd_folder <- "."
    cmd <- glue("git clone \"{url}\" \"{path}\"")
  } else if (dir.exists(path) && !dir.exists(file.path(path, ".git"))) {
    message(glue("Info: '{folder}' - directory already exists but is NOT a github repository -> no action"))
    return(FALSE)
  } else if (dir.exists(path) && pull) {
    message(glue("Info: '{folder}' - repository already exists -> pulling changes from remote"))
    cmd_folder <- path
    cmd <- "git pull"
  } else {
    message(glue("Info: '{folder}' - repository already exists but NOT pulling changes from remote -> no action"))
    return(FALSE)
  } 
  
  # run github call
  exec_command(cmd_folder, cmd, info = FALSE, output = TRUE)
  return(TRUE)
}


# other common git commands ----

#' Git Status
#' 
#' Shows the repository status in the active branch for all repositories that match the \code{prefix}.
#' 
#' @inherit ghc_repos_run_git_command params return
#' @family command functions
#' @export
ghc_repos_git_status <- function(prefix, folder = ".") {
  ghc_repos_run_git_command(prefix, command = "git status", folder = folder)
}

#' Git Pull
#' 
#' Pulls changes from the server to update the active branch for all repositories that match the \code{prefix}.
#' 
#' @inherit ghc_repos_run_git_command params return
#' @family command functions
#' @export
ghc_repos_git_pull <- function(prefix, folder = ".") {
  ghc_repos_run_git_command(prefix, command = "git pull", folder = folder)
}

#' Git Push
#' 
#' Pushes committed changes in the active branch (see \code{\link{ghc_repos_git_commit}}) for all repositories that match the \code{prefix} to GitHub. Note that this requires the repositories to have been cloned with a valid access token, otherwise this operation will fail due to lack of permissions. GitHub will also not allow \code{git push} if there are unpulled changes on the server. Use \code{\link{ghc_repos_git_pull}} followed by \code{\link{ghc_repos_git_push}} in this case (unless there are merging issues). 
#' 
#' @inherit ghc_repos_run_git_command params return
#' @family command functions
#' @export
ghc_repos_git_push <- function(prefix, folder = ".") {
  ghc_repos_run_git_command(prefix, command = "git push", folder = folder)
}

#' Git Commit
#' 
#' Commits changes for all modified files in the active branch for all repositories that match the \code{prefix}. Use \code{\link{ghc_repos_git_add}} and \code{\link{ghc_repos_git_remove}} prior to \code{\link{ghc_repos_git_commit}} to include new files (not tracked by git yet) / remove tracked files in the commit. Use \code{\link{ghc_repos_git_push}} to push the committed changes back to the GitHub server.
#' 
#' @inherit ghc_repos_run_git_command params return
#' @family command functions
#' @export
ghc_repos_git_commit <- function(prefix, message, folder = ".") {
  if (missing(message)) stop("commit message required", call. = FALSE)
  ghc_repos_run_git_command(prefix, command = glue("git commit -a -m \"{message}\" && git status"), folder = folder)
}

#' Git Add
#' 
#' Adds the specified file to the active branch for all repositories that match the \code{prefix}. Use \code{\link{ghc_repos_git_commit}} to commit added files, followed by \code{\link{ghc_repos_git_push}} to push them to the server. Note that this function will always add the specific filed and overwrite .gitignore rules if necessary. You can also use this function to tag an existing file to be included in the next commit (this happens automatically for all modified files if you use the \code{\link{ghc_repos_git_commit}} function to commit changes).
#' 
#' @inherit ghc_repos_run_git_command params return
#' @family command functions
#' @param filepath new file to start tracking with git. The filepath is relative to the main repository directory within each repository.
#' @export
ghc_repos_git_add <- function(prefix, filepath, folder = ".") {
  if (missing(filepath)) stop("file path is required", call. = FALSE)
  ghc_repos_run_git_command(prefix, command = glue("git add -f \"{filepath}\" && git status"), folder = folder)
}

#' Git Remove
#' 
#' Removes the specified file from the active branch for all repositories that match the \code{prefix}. Use \code{\link{ghc_repos_git_commit}} to commit removed files, followed by \code{\link{ghc_repos_git_push}} to push the change to the server. 
#' 
#' @inherit ghc_repos_run_git_command params return
#' @inheritParams ghc_repos_git_add
#' @family command functions
#' @export
ghc_repos_git_remove <- function(prefix, filepath, folder = ".") {
  if (missing(filepath)) stop("file path is required", call. = FALSE)
  ghc_repos_run_git_command(prefix, command = glue("git rm \"{filepath}\" && git status"), folder = folder)
}

#' Git Create Branch
#' 
#' Creates a new branch and switches to it for all repositories that match the \code{prefix}. Note that the git command used for this is \code{git checkout}. Also be warned that unless parameter \code{publish = FALSE}, the new branch is published to the GitHub remote right away.
#' 
#' @inherit ghc_repos_run_git_command params return
#' @param branch branch name (for commands that interact with branches)
#' @param parent parent branch name or commit SHAE to use for creating the new branch
#' @param publish whether to publish the new branch, i.e. link it to the remote repository
#' @family command functions
#' @export
ghc_repos_git_create_branch <- function(prefix, branch, parent = "master", publish = TRUE, folder = ".") {
  if (missing(branch)) stop("branch name required", call. = FALSE)
  ghc_repos_run_git_command(prefix, command = glue("git checkout -b {branch} {parent} && git push -u origin {branch}"), folder = folder)
}

#' Git Delete Branch
#' 
#' Deletes an existing branch for all repositories that match the \code{prefix}. Be warned that with this function the branch is deleted both locally and on the server so be careful when using it. Can be followed by \code{\link{ghc_repos_git_create_branch}} to recreate a branch with the same name but based on different commit (e.g. for rebranching an assignment branch off a specific commit).
#' 
#' @inherit ghc_repos_run_git_command params return
#' @inheritParams ghc_repos_git_create_branch
#' @family command functions
#' @export
ghc_repos_git_delete_branch <- function(prefix, branch, publish = TRUE, folder = ".") {
  if (missing(branch)) stop("branch name required", call. = FALSE)
  ghc_repos_run_git_command(prefix, command = glue("git branch -D {branch} && git push origin -d {branch}"), folder = folder)
}

#' Git Switch Branch
#' 
#' Lets you switch to a different branch that already exists for all repositories that match the \code{prefix}. Note that the git command used for this is \code{git checkout}.
#' 
#' @inherit ghc_repos_run_git_command params return
#' @inheritParams ghc_repos_git_create_branch
#' @family command functions
#' @export
ghc_repos_git_switch_branch <- function(prefix, branch = "master", folder = ".") {
  ghc_repos_run_git_command(prefix, command = glue("git checkout {branch}"), folder = folder)
}

#' Git Discard Changes
#' 
#' Discards all unstaged(!) changes for repositories that match the \code{prefix}. This does NOT affect changes that have already been staged/committed (you would have to use \code{git reset} to do that). Note that this function uses the \code{git checkout} command, there is no \code{git discard}. 
#' 
#' @inherit ghc_repos_run_git_command params return
#' @family command functions
#' @export
ghc_repos_git_discard_changes <- function(prefix, folder = ".") {
  ghc_repos_run_git_command(prefix, command = "git checkout -- . && git status", folder = folder)
}
