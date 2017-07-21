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
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @return System output of MB-MDR executable.
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
                            work.dir = getwd()) {

  checkmate::assertFile(file)
  checkmate::assertChoice(trait, c("binary", "continuous", "survival"))
  checkmate::assertDirectory(work.dir)
  checkmate::assertString(out)
  if(!checkmate::testDirectory(dirname(out))) {
    warning(paste(checkmate::checkDirectory(dirname(out)), "will be created!", sep = ", "))
    dir.create(dirname(out), recursive = TRUE)
  }
  checkmate::assertDirectory(dirname(out))

  sysOut <- parallelMap::parallelMap(singleThread,
                                     file = file,
                                     more.args = list(trait = trait,
                                                      o = out,
                                                      log = log,
                                                      mod = mod,
                                                      options = getOption("mbmdr")))

  return(sysOut)

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
                ifelse(is.null(options$r),
                       "",
                       paste("-r", sprintf("%d", options$r))),
                "-m", sprintf("%d", options$m),
                "-at", sprintf("%d", options$at),
                "-ct", sprintf("%d", options$ct),
                "-ac", sprintf("%d", options$ac),
                "-x", sprintf("%f", options$x),
                "-mt", options$mt,
                "-o", o,
                "-o2", mod,
                "-a", options$a,
                "-rc", options$rc,
                "-d", options$d,
                "-v", options$v,
                ifelse(checkmate::testCharacter(options$e),
                       paste("-e",
                             paste(options$e, collapse = ",")),
                       ""),
                ifelse(checkmate::testString(options$E),
                       paste("-E", options$E),
                       ""),
                ifelse(checkmate::testCharacter(options$filter),
                       paste("-f", paste(options$filter, collapse = ",")),
                       ""),
                ifelse(checkmate::testString(options$filter.file),
                       paste("-F", options$filter.file),
                       ""),
                ifelse(checkmate::testCharacter(options$k),
                       paste("-k", paste(options$k, collapse = ",")),
                       ""),
                ifelse(checkmate::testString(options$K),
                       paste("-K", options$K),
                       ""),
                ifelse(checkmate::testString(options$s),
                       paste("-s", options$s),
                       ""),
                "-if", options$input.format,
                ifelse(trait == "continuous",
                       paste0("-rt", options$rt),
                       ""),
                "-pb", options$pb,
                file,
                " > ", log)

  BBmisc::system3(command = options$exec,
                  args = args,
                  stdout = TRUE,
                  stderr = TRUE,
                  stop.on.exit.code = TRUE)

}
