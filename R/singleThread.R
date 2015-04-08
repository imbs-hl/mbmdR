#' @title MB-MDR Analysis
#'
#' @description
#' Non parallel MB-MDR analysis.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param out [\code{string}]\cr
#'   Sets the output file name. Defaults to <\code{work.dir}>/<\code{file}>.result.
#'
#' @param log [\code{string}]\cr
#'   Sets the log file name. Defaults to <\code{work.dir}>/<\code{file}>.log.
#'
#' @param reg.id [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}. Defaults to "singleThread".
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param reg.dir [\code{string}]\cr
#'   Path for saving the \link{BatchJobs} \link{Registry}. Defaults to <\code{work.dir}>/registries/<\code{reg.id}>.
#'
#' @param skip [\code{logical}]\cr
#'   Skip creation of a new registry if a registry is found in file.dir. Defaults to TRUE.
#'
#' @return \link{BatchJobs} \link{Registry} object.
#'
#' @export
runSingleThread <- function(file,
                            trait,
                            out = file.path(work.dir,
                                            paste(basename(file_path_sans_ext(file)),
                                                  "result", sep = ".")),
                            log = file.path(work.dir,
                                            paste(basename(file_path_sans_ext(file)),
                                                  "log", sep = ".")),
                            reg.id = "singleThread",
                            work.dir = getwd(),
                            reg.dir = file.path(work.dir, "registries", reg.id),
                            skip = TRUE) {

  assertFile(file)
  assertChoice(trait, c("binary", "continuous", "survival"))
  assertString(reg.id)
  assertDirectory(work.dir)
  if(!testDirectory(reg.dir)) {
    warning(paste(checkDirectory(reg.dir), "will be created!", sep = ", "))
    dir.create(reg.dir, recursive = TRUE)
  }
  assertDirectory(reg.dir)
  assertString(out)
  if(!testDirectory(dirname(out))) {
    warning(paste(checkDirectory(dirname(out)), "will be created!", sep = ", "))
    dir.create(dirname(out), recursive = TRUE)
  }
  assertDirectory(dirname(out))
  assertLogical(skip)

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      packages = c('mbmdR'))

  options <- getOption("mbmdr")

  jobs <- batchMap(reg, singleThread,
                   file = file,
                   more.args = list(trait = trait,
                                    o = out,
                                    log = log,
                                    options = getOption("mbmdr")))

  retries <- 0
  while(!testFile(file.path(reg.dir, "BatchJobs.db")) & retries < 100) {
    Sys.sleep(1)
    retries <- retries + 1
  }
  assertFile(file.path(reg.dir, "BatchJobs.db"))

  submitJobs(reg, chunk(jobs, chunk.size = 1000),
             chunks.as.arrayjobs = getConfig()$ssh,
             job.delay = TRUE)

  return(reg)

}

singleThread <- function(file, trait, o, log, options) {

  check.options(options)

  cmd <- paste(options$exec,
               " --", trait,
               " -n ", sprintf("%d", options$n),
               " -p ", sprintf("%d", options$p),
               ifelse(testNull(options$r), "", paste0(" -r ", sprintf("%d", options$r))),
               " -m ", sprintf("%d", options$m),
               " -x ", sprintf("%f", options$x),
               " -mt ", options$mt,
               " -o ", o,
               ifelse(testNull(options$r), "", paste0(" -r ", options$r)),
               " -a ", options$a,
               " -d ", options$d,
               " -v ", options$v,
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
               " > ", log,
               sep = "")

  print(cmd)

  system(cmd)

}
