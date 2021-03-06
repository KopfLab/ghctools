% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commands.R
\name{ghc_clone_repository}
\alias{ghc_clone_repository}
\title{Clone a repository to the target directory}
\usage{
ghc_clone_repository(url, path = NULL, token = NULL, pull = TRUE)
}
\arguments{
\item{url}{the https://... repository}

\item{path}{the directory to clone to, if not provided, clones into the current working directory with the repository name}

\item{token}{the authentication token (only required for private repositories if global credentials are not set)}

\item{pull}{if the repository already exists, whether to update it from the remote (see \code{\link{ghc_repos_git_pull}})}
}
\value{
whether the repository was cloned/pulled
}
\description{
Clone a repository to the target directory
}
