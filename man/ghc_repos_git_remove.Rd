% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commands.R
\name{ghc_repos_git_remove}
\alias{ghc_repos_git_remove}
\title{Git Remove}
\usage{
ghc_repos_git_remove(prefix, filepath, folder = ".")
}
\arguments{
\item{prefix}{repository prefix (typically this would be a github classroom assignment prefix)}

\item{filepath}{new file to start tracking with git. The filepath is relative to the main repository directory within each repository.}

\item{folder}{the target directory where to look for matching git repositories (searches recursively in all subdirectories)}
}
\value{
returns the \code{prefix} invisibly to facilitate piping
}
\description{
Removes the specified file from the active branch for all repositories that match the \code{prefix}. Use \code{\link{ghc_repos_git_commit}} to commit removed files, followed by \code{\link{ghc_repos_git_push}} to push the change to the server.
}
\seealso{
Other command functions: 
\code{\link{ghc_repos_copy_to}()},
\code{\link{ghc_repos_git_add}()},
\code{\link{ghc_repos_git_clone}()},
\code{\link{ghc_repos_git_commit}()},
\code{\link{ghc_repos_git_create_branch}()},
\code{\link{ghc_repos_git_delete_branch}()},
\code{\link{ghc_repos_git_discard_changes}()},
\code{\link{ghc_repos_git_pull}()},
\code{\link{ghc_repos_git_push}()},
\code{\link{ghc_repos_git_status}()},
\code{\link{ghc_repos_git_switch_branch}()},
\code{\link{ghc_repos_run_git_command}()}
}
\concept{command functions}
