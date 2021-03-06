% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commands.R
\name{ghc_repos_git_delete_branch}
\alias{ghc_repos_git_delete_branch}
\title{Git Delete Branch}
\usage{
ghc_repos_git_delete_branch(prefix, branch, publish = TRUE, folder = ".")
}
\arguments{
\item{prefix}{repository prefix (typically this would be a github classroom assignment prefix)}

\item{branch}{branch name (for commands that interact with branches)}

\item{publish}{whether to publish the new branch, i.e. link it to the remote repository}

\item{folder}{the target directory where to look for matching git repositories (searches recursively in all subdirectories)}
}
\value{
returns the \code{prefix} invisibly to facilitate piping
}
\description{
Deletes an existing branch for all repositories that match the \code{prefix}. Be warned that with this function the branch is deleted both locally and on the server so be careful when using it. Can be followed by \code{\link{ghc_repos_git_create_branch}} to recreate a branch with the same name but based on different commit (e.g. for rebranching an assignment branch off a specific commit).
}
\seealso{
Other command functions: 
\code{\link{ghc_repos_copy_to}()},
\code{\link{ghc_repos_git_add}()},
\code{\link{ghc_repos_git_clone}()},
\code{\link{ghc_repos_git_commit}()},
\code{\link{ghc_repos_git_create_branch}()},
\code{\link{ghc_repos_git_discard_changes}()},
\code{\link{ghc_repos_git_pull}()},
\code{\link{ghc_repos_git_push}()},
\code{\link{ghc_repos_git_remove}()},
\code{\link{ghc_repos_git_status}()},
\code{\link{ghc_repos_git_switch_branch}()},
\code{\link{ghc_repos_run_git_command}()}
}
\concept{command functions}
