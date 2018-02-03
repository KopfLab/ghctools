# general git command functions ---

# Execute command in each folder
# 
# Convenience function to execute a bash command in a list of folders.
# @param folders absolute paths or paths relative to current working directory
exec_command <- function(folders, command, info = TRUE) {
  map(folders, function(folder) {
    if (info) message(glue("Folder: {folder}"))
    glue("cd \"{folder}\" && {command}") %>% system()
  })
  invisible(NULL)
}


#' Run a git command
#' 
#' Generic function to run a git command in a number of repositories with the same prefix (typically those form the same assignment).
#' 
#' @inheritParams ghc_find_local_repositories
#' @param command a git command ("git" prefix will be automatically added if not present)
#' @return returns the prefix invisibly to facilitate piping
#' @export
ghc_run_git_command <- function(prefix, command, folder = ".") {
  if (missing(command)) stop("git command required", call. = FALSE)
  folders <- suppressMessages(ghc_find_local_repositories(prefix, folder = folder))
  # make sure command has a git prefix
  command <- str_replace(command, "^git ", "") %>% str_c("git ", .)
  
  #info
  glue("Info: executing command '{command}' for {length(folders)} git repositories (prefix '{prefix}'):") %>% 
    message()
  exec_command(folders, command)
  return(invisible(prefix))
}

#' Run standard git commands
#' 
#' Run standard git commands for a set of student repositories (typically those from the same assignment). More specific commands can be executed with \code{\link{ghc_run_git_command}}.
#' 
#' @name ghc_git_commands
#' @inheritParams ghc_find_local_repositories
#' @return all standard command functions return the prefix invisibly to facilitate piping
NULL

#' @rdname ghc_git_commands
#' @details \code{ghc_git_status} shows the repository status in the active branch for all repositories that match the \code{prefix}.
#' @export
ghc_git_status <- function(prefix, folder = ".") {
  ghc_run_git_command(prefix, command = "git status", folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_pull} pulls changes from the server to update the active branch for all repositories that match the \code{prefix}.
#' @export
ghc_git_pull <- function(prefix, folder = ".") {
  ghc_run_git_command(prefix, command = "git pull", folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_push} pushes committed changes in the active branch (see \code{ghc_git_commit}) for all repositories that match the \code{prefix} to GitHub. Note that this requires the repositories to have been cloned with a valid access token, otherwise this operation will fail due to lack of permissions. GitHub will also not allow \code{git push} if there are unpulled changes on the server. Use \code{ghc_git_pull} followed by \code{ghc_git_push} in this case (unless there are merging issues). 
#' @export
ghc_git_push <- function(prefix, folder = ".") {
  ghc_run_git_command(prefix, command = "git push", folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_commit} commits changes for all modified files in the active branch for all repositories that match the \code{prefix}. Use \code{ghc_git_add} and \code{ghc_git_remove} prior to \code{ghc_git_commit} to include new files (not tracked by git yet) / remove tracked files in the commit. Use \code{ghc_git_push} to push the committed changes back to the GitHub server.
#' @export
ghc_git_commit <- function(prefix, message, folder = ".") {
  if (missing(message)) stop("commit message required", call. = FALSE)
  ghc_run_git_command(prefix, command = glue("git commit -a -m \"{message}\" && git status"), folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_add} adds the specified file to the active branch for all repositories that match the \code{prefix}. Use \code{ghc_git_commit} to commit added files, followed by \code{ghc_git_push} to push them to the server. Note that this function will always add the specific filed and overwrite .gitignore rules if necessary. You can also use this function to tag an existing file to be included in the next commit (this happens automatically for all modified files if you use the \code{ghc_git_commit} function to commit changes).
#' @param filepath new file to start tracking with git. The filepath is relative to the main repository directory within each repository.
#' @export
ghc_git_add <- function(prefix, filepath, folder = ".") {
  if (missing(filepath)) stop("file path is required", call. = FALSE)
  ghc_run_git_command(prefix, command = glue("git add -f \"{filepath}\" && git status"), folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_remove} removes the specified file from the active branch for all repositories that match the \code{prefix}. Use \code{ghc_git_commit} to commit removed files, followed by \code{ghc_git_push} to push the change to the server. 
#' @inheritParams ghc_git_add
#' @export
ghc_git_remove <- function(prefix, filepath, folder = ".") {
  if (missing(filepath)) stop("file path is required", call. = FALSE)
  ghc_run_git_command(prefix, command = glue("git rm \"{filepath}\" && git status"), folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_create_branch} creates a new branch and switches to it for all repositories that match the \code{prefix}. Note that the git command used for this is \code{git checkout}. Also be warned that unless parameter \code{publish = FALSE}, the new branch is published to the GitHub remote right away.
#' @param branch branch name (for commands that interact with branches)
#' @param parent parent branch name or commit SHAE to use for creating the new branch (\code{ghc_git_create_branch} only)
#' @param publish whether to publish the new branch, i.e. link it to the remote repository (\code{ghc_git_create_branch} only)
#' @export
ghc_git_create_branch <- function(prefix, branch, parent = "master", publish = TRUE, folder = ".") {
  if (missing(branch)) stop("branch name required", call. = FALSE)
  ghc_run_git_command(prefix, command = glue("git checkout -b {branch} {parent} && git push -u origin {branch}"), folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_switch_branch} lets you switch to a different branch that already exists for all repositories that match the \code{prefix}. Note that the git command used for this is \code{git checkout}.
#' @inheritParams ghc_git_create_branch
#' @export
ghc_git_switch_branch <- function(prefix, branch = "master", folder = ".") {
  ghc_run_git_command(prefix, command = glue("git checkout {branch}"), folder = folder)
}

#' @rdname ghc_git_commands
#' @details \code{ghc_git_discard_changes} discards all unstaged(!) changes for repositories that match the \code{prefix}. This does NOT affect changes that have already been staged/committed (you would have to use \code{git reset} to do that). Note that this function uses the \code{git checkout} command, there is no \code{git discard}. 
#' @export
ghc_git_discard_changes <- function(prefix, folder = ".") {
  ghc_run_git_command(prefix, command = "git checkout -- . && git status", folder = folder)
}
