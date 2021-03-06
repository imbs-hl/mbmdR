% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{waitForFiles}
\alias{waitForFiles}
\title{Wait for files on NFS mounts}
\usage{
waitForFiles(fns, timeout = NA_real_)
}
\arguments{
\item{fns}{[\code{character}]\cr
Vector of file paths to wait for.}

\item{timeout}{[\code{number}]\cr
Time in seconds to wait for files to appear.#'}
}
\value{
\code{TRUE} if all files are available. Otherwise throws an error
}
\description{
Wait for files on NFS mounts
}
\details{
Use \code{\link{list.files}} here as this seems to trick the nfs cache; see https://github.com/mllg/batchtools/issues/85.
}
