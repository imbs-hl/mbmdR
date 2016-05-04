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
#' @return BatchJobs registry object.
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
                                                         paste(basename(file_path_sans_ext(file)),
                                                               "top", sep = "_")),
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

  options <- getOption("mbmdr")

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      seed = options$r,
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
#' @return BatchJobs registry object.
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
                                                               paste(basename(file_path_sans_ext(file)),
                                                                     "top", sep = "_")),
                                   out = file.path(work.dir,
                                                   paste(basename(file_path_sans_ext(file)),
                                                         "topfile", sep = ".")),
                                   mod = file.path(work.dir,
                                                   paste(basename(file_path_sans_ext(file)),
                                                         "models", sep = ".")),
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
  assertFile(paste0(topfiles.prefix, 1:cpus, ".txt"))
  assertDirectory(dirname(out))
  assertDirectory(dirname(mod))
  assertLogical(skip)

  options <- getOption("mbmdr")

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      seed = options$r,
                      packages = c('mbmdR'))

  jobs <- batchMap(reg, gammastep2,
                   file, more.args = list(trait = trait,
                                          cpus = cpus,
                                          ti = topfiles.prefix,
                                          t = out,
                                          o2 = mod,
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
               " -i ", sprintf("%d", id),
               " -N ", sprintf("%d", cpus),
               " -ti ", ti,
               " -n ", sprintf("%d", options$n),
               " -m ", sprintf("%d", options$m),
               " -at ", sprintf("%d", options$at),
               " -ct ", sprintf("%d", options$ct),
               " -ac ", sprintf("%d", options$ac),
               " -x ", sprintf("%f", options$x),
               " -a ", options$a,
               " -rc ", options$rc,
               ifelse(testCharacter(options$e),
                      paste(" -e ", paste(options$e, collapse = ",")), ""),
               ifelse(testString(options$E),
                      paste(" -E ", options$E), ""),
               ifelse(testCharacter(options$filter),
                      paste(" -f ", paste(options$filter, collapse = ",")), ""),
               ifelse(testString(options$filter.file),
                      paste(" -F ", options$filter.file), ""),
               ifelse(testCharacter(options$k),
                      paste(" -k ", paste(options$k, collapse = ",")), ""),
               ifelse(testString(options$K),
                      paste(" -K ", options$K), ""),
               ifelse(testString(options$s),
                      paste(" -s ", options$s), ""),
               " -if ", options$input.format,
               ifelse(trait == "continuous", paste0(" -rt ", options$rt), ""),
               " -pb ", options$pb,
               " ", file,
               sep = "")

  print(cmd)

  system(cmd)

}

gammastep2 <- function(file, trait, cpus, ti, t, o2, options) {

  check.options(options)

  cmd <- paste(options$exec,
               " --", trait,
               " --gammastep2",
               " -N ", sprintf("%d", cpus),
               " -ti ", ti,
               " -t ", t,
               " -o2 ", o2,
               " -n ", sprintf("%d", options$n),
               " -m ", sprintf("%d", options$m),
               " -at ", sprintf("%d", options$at),
               " -ct ", sprintf("%d", options$ct),
               " -ac ", sprintf("%d", options$ac),
               " -a ", options$a,
               " -rc ", options$rc,
               " -x ", sprintf("%f", options$x),
               ifelse(testCharacter(options$e),
                      paste(" -e ", paste(options$e, collapse = ",")), ""),
               ifelse(testString(options$E),
                      paste(" -E ", options$E), ""),
               ifelse(testCharacter(options$filter),
                      paste(" -f ", paste(options$filter, collapse = ",")), ""),
               ifelse(testString(options$filter.file),
                      paste(" -F ", options$filter.file), ""),
               ifelse(testCharacter(options$k),
                      paste(" -k ", paste(options$k, collapse = ",")), ""),
               ifelse(testString(options$K),
                      paste(" -K ", options$K), ""),
               ifelse(testString(options$s),
                      paste(" -s ", options$s), ""),
               " -v ", options$v,
               " -if ", options$input.format,
               ifelse(trait == "continuous", paste0(" -rt ", options$rt), ""),
               " -pb ", options$pb,
               " ", file,
               sep = "")

  print(cmd)

  system(cmd)

}
