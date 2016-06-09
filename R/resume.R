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
