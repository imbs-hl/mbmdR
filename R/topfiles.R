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
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param out.prefix [\code{string}]\cr
#'   Path for saving the partial topfiles. Defaults to <\code{work.dir}>/topfiles/<\code{file}>_top.
#'
#' @return System output of MB-MDR executable.
#'
#' @export
createPartialTopFiles <- function(file,
                                  trait,
                                  cpus,
                                  work.dir = getwd(),
                                  out.prefix = file.path(work.dir,
                                                         "topfiles",
                                                         paste(basename(file_path_sans_ext(file)),
                                                               "top", sep = "_"))) {

  checkmate::assertFile(file)
  checkmate::assertChoice(trait, c("binary", "continuous", "survival"))
  checkmate::assertNumber(cpus)
  checkmate::assertDirectory(work.dir)
  if(!checkmate::testDirectory(dirname(out.prefix))) {
    warning(paste(checkmate::checkDirectory(dirname(out.prefix)), "will be created!", sep = ", "))
    dir.create(dirname(out.prefix), recursive = TRUE)
  }
  checkmate::assertDirectory(dirname(out.prefix))

  options <- getOption("mbmdr")

  sysOut <- parallelMap::parallelMap(gammastep1,
                                     id = 1:cpus, level = "mbmdR.parallelSteps",
                                     more.args = list(file = file,
                                                      trait = trait,
                                                      cpus = cpus,
                                                      ti = out.prefix,
                                                      options = options))

  waitForFiles(fns = sprintf("%s%d.txt", out.prefix, 1:cpus), timeout = options$fs.latency)

  return(sysOut)

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
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param topfiles.prefix [\code{string}]\cr
#'   Path of partial topfiles. Defaults to <\code{work.dir}>/topfiles/<\code{file}>_top.
#'
#' @param out [\code{string}]\cr
#'   Path for saving the final topfile. Defaults to <\code{work.dir}>/<\code{file}>.topfile.
#'
#' @param mod [\code{string}]\cr
#'   Path for saving the models file. Defaults to <\code{work.dir}>/<\code{file}>.models.
#'
#' @return System output of MB-MDR executable.
#'
#' @export
combinePartialTopFiles <- function(file,
                                   trait,
                                   cpus,
                                   work.dir = getwd(),
                                   topfiles.prefix = file.path(work.dir,
                                                               "topfiles",
                                                               paste(basename(tools::file_path_sans_ext(file)),
                                                                     "top", sep = "_")),
                                   out = file.path(work.dir,
                                                   paste(basename(tools::file_path_sans_ext(file)),
                                                         "topfile", sep = ".")),
                                   mod = file.path(work.dir,
                                                   paste(basename(tools::file_path_sans_ext(file)),
                                                         "models", sep = "."))) {

  checkmate::assertFile(file)
  checkmate::assertChoice(trait, c("binary", "continuous", "survival"))
  checkmate::assertInt(cpus)
  checkmate::assertDirectory(work.dir)
  if(!checkmate::testDirectory(dirname(out))) {
    warning(paste(checkmate::checkDirectory(dirname(out)), "will be created!", sep = ", "))
    dir.create(dirname(out), recursive = TRUE)
  }
  checkmate::assertDirectory(dirname(topfiles.prefix))
  checkmate::assertFile(paste0(topfiles.prefix, 1:cpus, ".txt"))
  checkmate::assertDirectory(dirname(out))
  checkmate::assertDirectory(dirname(mod))

  options <- getOption("mbmdr")

  sysOut <- parallelMap::parallelMap(gammastep2,
                                     file,
                                     more.args = list(trait = trait,
                                                      cpus = cpus,
                                                      ti = topfiles.prefix,
                                                      t = out,
                                                      o2 = mod,
                                                      options = options))

  waitForFiles(fns = c(out, mod), timeout = options$fs.latency)

  return(sysOut)

}

gammastep1 <- function(file, trait, id, cpus, ti, options) {

  check.options(options)

  args <- c(paste0("--", trait),
            "--gammastep1",
            "-i ", sprintf("%d", id),
            "-N ", sprintf("%d", cpus),
            "-ti ", shQuote(ti),
            "-n ", sprintf("%d", options$n),
            "-m ", sprintf("%d", options$m),
            "-at ", sprintf("%d", options$at),
            "-ct ", sprintf("%d", options$ct),
            "-ac ", sprintf("%d", options$ac),
            "-x ", sprintf("%f", options$x),
            "-a ", options$a,
            "-rc ", options$rc,
            ifelse(testCharacter(options$e),
                   paste("-e", paste(options$e, collapse = ",")),
                   ""),
            ifelse(testString(options$E),
                   paste("-E", shQuote(options$E)),
                   ""),
            ifelse(testCharacter(options$filter),
                   paste("-f", paste(options$filter, collapse = ",")),
                   ""),
            ifelse(testString(options$filter.file),
                   paste("-F", shQuote(options$filter.file)),
                   ""),
            ifelse(testCharacter(options$k),
                   paste("-k", paste(options$k, collapse = ",")),
                   ""),
            ifelse(testString(options$K),
                   paste("-K", shQuote(options$K)),
                   ""),
            "-if", options$input.format,
            ifelse(trait == "continuous",
                   paste("-rt", options$rt),
                   ""),
            "-pb", options$pb,
            shQuote(file),
            "&>", shQuote(sprintf("%s%d.log", ti, id)))

  sysOut <- BBmisc::system3(command = options$exec,
                            args = args,
                            stdout = TRUE,
                            stderr = TRUE,
                            stop.on.exit.code = TRUE)

  waitForFiles(fns = sprintf("%s%d.txt", ti, 1:cpus), timeout = options$fs.latency)

  return(sysOut)

}

gammastep2 <- function(file, trait, cpus, ti, t, o2, options) {

  check.options(options)

  args <- paste(paste0("--", trait),
                "--gammastep2",
                "-N", sprintf("%d", cpus),
                "-ti", shQuote(ti),
                "-t", shQuote(t),
                "-o2", shQuote(o2),
                "-n", sprintf("%d", options$n),
                "-m", sprintf("%d", options$m),
                "-at", sprintf("%d", options$at),
                "-ct", sprintf("%d", options$ct),
                "-ac", sprintf("%d", options$ac),
                "-a", options$a,
                "-rc", options$rc,
                "-x", sprintf("%f", options$x),
                ifelse(testCharacter(options$e),
                       paste("-e", paste(options$e, collapse = ",")),
                       ""),
                ifelse(testString(options$E),
                       paste("-E", shQuote(options$E)),
                       ""),
                ifelse(testCharacter(options$filter),
                       paste("-f", paste(options$filter, collapse = ",")),
                       ""),
                ifelse(testString(options$filter.file),
                       paste("-F", shQuote(options$filter.file)),
                       ""),
                ifelse(testCharacter(options$k),
                       paste("-k", paste(options$k, collapse = ",")),
                       ""),
                ifelse(testString(options$K),
                       paste("-K", shQuote(options$K)),
                       ""),
                "-v", options$v,
                "-if", options$input.format,
                ifelse(trait == "continuous",
                       paste("-rt", options$rt),
                       ""),
                "-pb", options$pb,
                shQuote(file),
                "&>", shQuote(sprintf("%s.log", t)))

  sysOut <- BBmisc::system3(command = options$exec,
                            args = args,
                            stdout = TRUE,
                            stderr = TRUE,
                            stop.on.exit.code = TRUE)

  waitForFiles(fns = c(t, o2), timeout = options$fs.latency)

  return(sysOut)

}
