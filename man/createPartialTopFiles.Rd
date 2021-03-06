% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topfiles.R
\name{createPartialTopFiles}
\alias{createPartialTopFiles}
\title{Create partial top files}
\usage{
createPartialTopFiles(file, trait, cpus, work.dir = getwd(),
  out.prefix = file.path(work.dir, "topfiles",
  paste(basename(file_path_sans_ext(file)), "top", sep = "_")))
}
\arguments{
\item{file}{[\code{string}]\cr
File path of input MB-MDR file.}

\item{trait}{[\code{string}]\cr
Type of trait. "binary", "continuous" or "survival".}

\item{cpus}{[\code{integer}]\cr
Total amount of CPUs to be used.}

\item{work.dir}{[\code{string}]\cr
Working directory for MB-MDR. Defaults to current working directory.}

\item{out.prefix}{[\code{string}]\cr
Path for saving the partial topfiles. Defaults to <\code{work.dir}>/topfiles/<\code{file}>_top.}
}
\value{
System output of MB-MDR executable.
}
\description{
First step of parallel workflow of MB-MDR. Creates partial topfiles on multiple CPUs.
}
