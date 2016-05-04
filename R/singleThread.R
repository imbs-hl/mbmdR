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
#' @param mod [\code{string}]\cr
#'   Sets the models file name. Defaults to <\code{work.dir}>/<\code{file}>.models.
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
                            mod = file.path(work.dir,
                                            paste(basename(file_path_sans_ext(file)),
                                                  "models", sep = ".")),
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

  options <- getOption("mbmdr")

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      seed = options$r,
                      packages = c('mbmdR'))

  jobs <- batchMap(reg, singleThread,
                   file = file,
                   more.args = list(trait = trait,
                                    o = out,
                                    log = log,
                                    mod = mod,
                                    options = getOption("mbmdr")))

  submitJobs(reg, chunk(jobs, chunk.size = 1000),
             chunks.as.arrayjobs = getConfig()$ssh,
             job.delay = TRUE)

  return(reg)

}

#' @title MB-MDR Wrapper
#'
#' @description
#' Wrapper function for external function call.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param o [\code{string}]\cr
#'   Sets the output file name.
#'
#' @param log [\code{string}]\cr
#'   Sets the log file name.
#'
#' @param mod [\code{string}]\cr
#'   Sets the models file name.
#'
#' @param options [\code{list}]
#'   MB-MDR options set by \code{\link{configure}}.
#'
#' @export
singleThread <- function(file, trait, o, log, mod, options) {

  check.options(options)

  args <- paste(paste0("--", trait),
               "-n", sprintf("%d", options$n),
               "-p", sprintf("%d", options$p),
               "-r", runif(1, 0, .Machine$integer.max),
               "-m", sprintf("%d", options$m),
               "-at", sprintf("%d", options$at),
               "-ct", sprintf("%d", options$ct),
               "-ac", sprintf("%d", options$ac),
               "-x", sprintf("%f", options$x),
               "-mt", options$mt,
               "-o", o,
               "-o2", mod,
               ifelse(testNull(options$r),
                      "",
                      paste0("-r", options$r)),
               "-a", options$a,
               "-rc", options$rc,
               "-d", options$d,
               "-v", options$v,
               ifelse(testCharacter(options$e),
                      paste("-e",
                            paste(options$e, collapse = ",")),
                      ""),
               ifelse(testString(options$E),
                      paste("-E", options$E),
                      ""),
               ifelse(testCharacter(options$filter),
                      paste("-f", paste(options$filter, collapse = ",")),
                      ""),
               ifelse(testString(options$filter.file),
                      paste("-F", options$filter.file),
                      ""),
               ifelse(testCharacter(options$k),
                      paste("-k", paste(options$k, collapse = ",")),
                      ""),
               ifelse(testString(options$K),
                      paste("-K", options$K),
                      ""),
               ifelse(testString(options$s),
                      paste("-s", options$s),
                      ""),
               "-if", options$input.format,
               ifelse(trait == "continuous",
                      paste0("-rt", options$rt),
                      ""),
               "-pb", options$pb,
               file,
               " > ", log)

  print(paste(options$exec, paste(args, collapse = " ")))

  BBmisc::system3(command = options$exec,
                  args = args,
                  stdout = TRUE,
                  stderr = TRUE,
                  stop.on.exit.code = TRUE)

}
