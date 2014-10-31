#' @title Create partial top files
#'
#' @description
#' First step of parallel workflow of MB-MDR. Creates partial topfiles on multiple CPUs.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param cpus [\code{integer}]\cr
#'   Total amount of CPUs to be used.
#'
#' @param reg.id [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}. Defaults to "partialTopFiles".
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param reg.dir [\code{string}]\cr
#'   Path for saving the \link{BatchJobs} \link{Registry}. Defaults to <\code{work.dir}>/registries/<\code{reg.id}>.
#'
#' @param out.prefix [\code{string}]\cr
#'   Path for saving the partial topfiles. Defaults to <\code{work.dir}>/topfiles/<\code{file}>_top.
#'
#' @param skip [\code{logical}]\cr
#'   Skip creation of a new registry if a registry is found in file.dir. Defaults to TRUE.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
createPartialTopFiles <- function(file,
                                  trait,
                                  cpus,
                                  reg.id = "partialTopFiles",
                                  work.dir = getwd(),
                                  reg.dir = file.path(work.dir, "registries", reg.id),
                                  out.prefix = file.path(work.dir,
                                                         "topfiles",
                                                         paste0(gsub('(.+)\\.(.*)',
                                                                     '\\1',
                                                                     basename(file)),
                                                                "_top")),
                                  skip = TRUE) {

  assertFile(file)
  assertChoice(trait, c("binary", "continuous", "survival"))
  assertNumber(cpus)
  assertString(reg.id)
  assertDirectory(work.dir)
  if(!testDirectory(reg.dir)) {
    warning(paste(checkDirectory(reg.dir), "will be created!", sep = ", "))
    dir.create(reg.dir, recursive = TRUE)
  }
  assertDirectory(reg.dir)
  if(!testDirectory(dirname(out.prefix))) {
    warning(paste(checkDirectory(dirname(out.prefix)), "will be created!", sep = ", "))
    dir.create(dirname(out.prefix), recursive = TRUE)
  }
  assertDirectory(dirname(out.prefix))
  assertLogical(skip)

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      packages = c('mbmdR'))

  ids <- 1:cpus

  jobs <- batchMap(reg, gammastep1,
                   ids, more.args = list(file = file,
                                         trait = trait,
                                         cpus = cpus,
                                         ti = out.prefix,
                                         options = getOption("mbmdr")))

  submitJobs(reg, chunk(jobs, chunk.size = 1000),
             chunks.as.arrayjobs = getConfig()$ssh,
             job.delay = TRUE)

  return(reg)

}

#' @title Combine partial top files
#'
#' @description
#' Second step of parallel workflow of MB-MDR. Creates topfiles from multiple partial topfiles.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param cpus [\code{integer}]\cr
#'   Sets the total amount of CPUs used in \link{createPartialTopFiles}.
#'
#' @param reg.id [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}. Defaults to "combineTopFiles".
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param reg.dir [\code{string}]\cr
#'   Path for saving the \link{BatchJobs} \link{Registry}. Defaults to <\code{work.dir}>/registries/<\code{reg.id}>.
#'
#' @param topfiles.prefix [\code{string}]\cr
#'   Path of partial topfiles. Defaults to <\code{work.dir}>/topfiles/<\code{file}>_top.
#'
#' @param out [\code{string}]\cr
#'   Path for saving the final topfile. Defaults to <\code{work.dir}>/<\code{file}>.topfile.
#'
#' @param skip [\code{logical}]\cr
#'   Skip creation of a new registry if a registry is found in file.dir. Defaults to TRUE.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
combinePartialTopFiles <- function(file,
                                   trait,
                                   cpus,
                                   reg.id = "combineTopFiles",
                                   work.dir = getwd(),
                                   reg.dir = file.path(work.dir, "registries", reg.id),
                                   topfiles.prefix = file.path(work.dir,
                                                               "topfiles",
                                                               paste0(gsub('(.+)\\.(.*)',
                                                                           '\\1',
                                                                           basename(file)),
                                                                      "_top")),
                                   out = file.path(work.dir,
                                                          paste0(gsub('(.+)\\.(.*)',
                                                                      '\\1',
                                                                      basename(file)),
                                                                 ".topfile")),
                                   skip = TRUE) {

  assertFile(file)
  assertChoice(trait, c("binary", "continuous", "survival"))
  assertInt(cpus)
  assertString(reg.id)
  assertDirectory(work.dir)
  if(!testDirectory(reg.dir)) {
    warning(paste(checkDirectory(reg.dir), "will be created!", sep = ", "))
    dir.create(reg.dir, recursive = TRUE)
  }
  assertDirectory(reg.dir)
  if(!testDirectory(dirname(out))) {
    warning(paste(checkDirectory(dirname(out)), "will be created!", sep = ", "))
    dir.create(dirname(out), recursive = TRUE)
  }
  assertDirectory(dirname(topfiles.prefix))
  assertDirectory(dirname(out))
  assertLogical(skip)

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      packages = c('mbmdR'))

  jobs <- batchMap(reg, gammastep2,
                   file, more.args = list(trait = trait,
                                          cpus = cpus,
                                          ti = topfiles.prefix,
                                          t = out,
                                          options = getOption("mbmdr")))

  submitJobs(reg, chunk(jobs, chunk.size = 1000),
             chunks.as.arrayjobs = getConfig()$ssh,
             job.delay = TRUE)

  return(reg)

}

gammastep1 <- function(file, trait, id, cpus, ti, options) {

  check.options(options)

  cmd <- paste(options$exec,
               " --", trait,
               " --gammastep1",
               " -i ", id,
               " -N ", cpus,
               " -ti ", ti,
               " -n ", options$n,
               " -m ", options$m,
               " -a ", options$a,
               ifelse(testCharacter(options$e),
                      paste(" -e ", paste(options$e, collapse = ",")), ""),
               ifelse(testString(options$E),
                      paste(" -E ", options$E), ""),
               ifelse(testCharacter(options$filter),
                      paste(" -f ", paste(options$filter, collapse = ",")), ""),
               ifelse(testString(options$filter.file),
                      paste(" -F ", options$filter.file), ""),
               " -if ", options$input.format,
               ifelse(trait == "continuous", paste0(" -rt ", options$rt), ""),
               " -pb ", options$pb,
               " ", file,
               sep = "")

  print(cmd)

  system(cmd)

}

gammastep2 <- function(file, trait, cpus, ti, t, options) {

  check.options(options)

  cmd <- paste(options$exec,
               " --", trait,
               " --gammastep2",
               " -N ", cpus,
               " -ti ", ti,
               " -t ", t,
               " -n ", options$n,
               " -m ", options$m,
               " -a ", options$a,
               ifelse(testCharacter(options$e),
                      paste(" -e ", paste(options$e, collapse = ",")), ""),
               ifelse(testString(options$E),
                      paste(" -E ", options$E), ""),
               ifelse(testCharacter(options$filter),
                      paste(" -f ", paste(options$filter, collapse = ",")), ""),
               ifelse(testString(options$filter.file),
                      paste(" -F ", options$filter.file), ""),
               " -v ", options$v,
               " -if ", options$input.format,
               ifelse(trait == "continuous", paste0(" -rt ", options$rt), ""),
               " -pb ", options$pb,
               " ", file,
               sep = "")

  print(cmd)

  system(cmd)

}
