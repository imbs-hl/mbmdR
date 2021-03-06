% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singleThread.R
\name{runSingleThread}
\alias{runSingleThread}
\title{MB-MDR Analysis}
\usage{
runSingleThread(file, trait, out = file.path(work.dir,
  paste(basename(file_path_sans_ext(file)), "result", sep = ".")),
  log = file.path(work.dir, paste(basename(file_path_sans_ext(file)), "log",
  sep = ".")), mod = file.path(work.dir,
  paste(basename(file_path_sans_ext(file)), "models", sep = ".")),
  work.dir = getwd())
}
\arguments{
\item{file}{[\code{string}]\cr
File path of input MB-MDR file.}

\item{trait}{[\code{string}]\cr
Type of trait. "binary", "continuous" or "survival".}

\item{out}{[\code{string}]\cr
Sets the output file name. Defaults to <\code{work.dir}>/<\code{file}>.result.}

\item{log}{[\code{string}]\cr
Sets the log file name. Defaults to <\code{work.dir}>/<\code{file}>.log.}

\item{mod}{[\code{string}]\cr
Sets the models file name. Defaults to <\code{work.dir}>/<\code{file}>.models.}

\item{work.dir}{[\code{string}]\cr
Working directory for MB-MDR. Defaults to current working directory.}
}
\value{
System output of MB-MDR executable.
}
\description{
Non parallel MB-MDR analysis.
}
