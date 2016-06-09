#' @title Resume parallel workflow step
#'
#' @description
#' Resumes a step of a parallel workflow of a crashed session.
#'
#' @param file.path [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param cpus [\code{string}]\cr
#'   How many CPUs should be used for BatchJobs?
#'
#' @return BatchJobs registry object.
resumeStep <- function(file.path, file, cpus) {
  # Load old registry
  reg <- BatchJobs::loadRegistry(file.path, adjust.paths = TRUE)

  # Reset not yet finished jobs
  ids <- BatchJobs::resetJobs(reg, BatchJobs::findDone(reg))

  # Submit not yet finished jobs
  if(length(ids)) {
    submitJobs(reg, chunk(ids, chunk.size = cpus),
               chunks.as.arrayjobs = getConfig()$ssh,
               resources = list(nodes = 1,
                                ppn = 1,
                                mem = paste0(1+ceiling(2*file.size(file)/1024^3), "g")),
               job.delay = TRUE)
  }

  return(reg)

}
