#' @title Create final output file
#'
#' @description
#' Fourth step of parallel workflow of MB-MDR. Creates final output file on one CPU.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param cpus [\code{integer}]\cr
#'   Sets the total amount of CPUs used in \link{runPermutations}.
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param topfile [\code{string}]\cr
#'   Path of topfile. Defaults to <\code{work.dir}>/<\code{file}>.topfile.
#'
#' @param perm.prefix [\code{string}]\cr
#'   Path of the permutation files. Defaults to <\code{work.dir}>/permutations/<\code{file}>_perm.
#'
#' @param out [\code{string}]\cr
#'   Sets the output file name. Defaults to <\code{work.dir}>/<\code{file}>.result.
#'
#' @return BatchJobs registry object.
#'
#' @export
createOutput <- function(file,
                         trait,
                         cpus,
                         work.dir = getwd(),
                         topfile = file.path(work.dir,
                                             paste(basename(file_path_sans_ext(file)),
                                                   "topfile", sep = ".")),
                         perm.prefix = file.path(work.dir,
                                                 "permutations",
                                                 paste(basename(file_path_sans_ext(file)),
                                                       "perm", sep = "_")),
                         out = file.path(work.dir,
                                         paste(basename(file_path_sans_ext(file)),
                                               "result", sep = "."))) {

  checkmate::assertFile(file)
  checkmate::assertChoice(trait, c("binary", "continuous", "survival"))
  checkmate::assertNumber(cpus)
  checkmate::assertDirectory(work.dir)
  if(!checkmate::testDirectory(dirname(out))) {
    warning(paste(checkmate::checkDirectory(dirname(out)), "will be created!", sep = ", "))
    dir.create(dirname(out), recursive = TRUE)
  }
  checkmate::assertDirectory(dirname(out))
  checkmate::assertDirectory(dirname(perm.prefix))
  checkmate::assertFile(paste0(perm.prefix, 1:cpus, ".txt"))

  options <- getOption("mbmdr")

  sysOut <- parallelMap::parallelMap(gammastep4, c = perm.prefix,
                                     more.args = list(file = file,
                                                      trait = trait,
                                                      q = cpus,
                                                      p = options$p,
                                                      o = out,
                                                      t = topfile,
                                                      options = options))

  return(sysOut)

}

gammastep4 <- function(file, trait, c, q, p, t, o, options) {

  check.options(options)

  args <- paste(paste0("--", trait),
               "--gammastep4",
               "-c", shQuote(c),
               "-q", sprintf("%d", q),
               "-p", sprintf("%d", p),
               "-m", sprintf("%d", options$m),
               "-at", sprintf("%d", options$at),
               "-ct", sprintf("%d", options$ct),
               "-ac", sprintf("%d", options$ac),
               "-x", sprintf("%f", options$x),
               "-t", shQuote(t),
               "-o", shQuote(o),
               "-r", options$r,
               "-a", options$a,
               "-rc", options$rc,
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
                      paste0("-rt", options$rt),
                      ""),
               "-pb", options$pb,
               shQuote(file),
               "&>", shQuote(paste0(o, '.log')))

  BBmisc::system3(command = options$exec,
                  args = args,
                  stdout = TRUE,
                  stderr = TRUE,
                  stop.on.exit.code = TRUE)

}
