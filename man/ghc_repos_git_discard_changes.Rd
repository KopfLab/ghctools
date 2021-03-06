% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commands.R
\name{ghc_repos_git_discard_changes}
\alias{ghc_repos_git_discard_changes}
\title{Git Discard Changes}
\usage{
ghc_repos_git_discard_changes(prefix, folder = ".")
}
\arguments{
\item{prefix}{repository prefix (typically this would be a github classroom assignment prefix)}

\item{folder}{the target directory where to look for matching git repositories (searches recursively in all subdirectories)}
}
\value{
returns the \code{prefix} invisibly to facilitate piping
}
\description{
Discards all unstaged(!) changes for repositories that match the \code{prefix}. This does NOT affect changes that have already been staged/committed (you would have to use \code{git reset} to do that). Note that this function uses the \code{git checkout} command, there is no \code{git discard}.
}
\seealso{
Other command functions: 
\code{\link{ghc_repos_copy_to}()},
\code{\link{ghc_repos_git_add}()},
\code{\link{ghc_repos_git_clone}()},
\code{\link{ghc_repos_git_commit}()},
\code{\link{ghc_repos_git_create_branch}()},
\code{\link{ghc_repos_git_delete_branch}()},
\code{\link{ghc_repos_git_pull}()},
\code{\link{ghc_repos_git_push}()},
\code{\link{ghc_repos_git_remove}()},
\code{\link{ghc_repos_git_status}()},
\code{\link{ghc_repos_git_switch_branch}()},
\code{\link{ghc_repos_run_git_command}()}
}
\concept{command functions}
