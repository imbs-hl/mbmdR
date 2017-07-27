#' @title Create permutation files
#'
#' @description
#' Third step of parallel workflow of MB-MDR. Run permutations on multiple CPUs.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param cpus [\code{integer}]\cr
#'   Sets the total amount of CPUs to be used.
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param topfile [\code{string}]\cr
#'   Path of topfile. Defaults to <\code{work.dir}>/<\code{file}>.topfile.
#'
#' @param out.prefix [\code{string}]\cr
#'   Path for saving the permutation files. Defaults to <\code{work.dir}>/permutations/<\code{file}>_perm.
#'
#' @return System output of MB-MDR executable.
#'
#' @export
runPermutations <- function(file,
                            trait,
                            cpus,
                            work.dir = getwd(),
                            topfile = file.path(work.dir,
                                                paste(basename(file_path_sans_ext(file)),
                                                      "topfile", sep = ".")),
                            out.prefix = file.path(work.dir,
                                                   "permutations",
                                                   paste(basename(file_path_sans_ext(file)),
                                                         "perm", sep = "_"))) {

  checkmate::assertFile(file)
  checkmate::assertChoice(trait, c("binary", "continuous", "survival"))
  checkmate::assertNumber(cpus)
  checkmate::assertDirectory(work.dir)
  if(!checkmate::testDirectory(dirname(out.prefix))) {
    warning(paste(checkmate::checkDirectory(dirname(out.prefix)), "will be created!", sep = ", "))
    dir.create(dirname(out.prefix), recursive = TRUE)
  }
  checkmate::assertDirectory(dirname(out.prefix))
  checkmate::assertFile(topfile)

  options <- getOption("mbmdr")

  p <- sapply(BBmisc::chunk(1:options$p, n.chunks = cpus), length)

  sysOut <- parallelMap::parallelMap(gammastep3,
                                     p = p,
                                     i = 1:cpus, level = "mbmdR.parallelSteps",
                                     more.args = list(file = file,
                                                      trait = trait,
                                                      t = topfile,
                                                      out.prefix = out.prefix,
                                                      options = options))

  waitForFiles(fns = sprintf("%s%d.txt", out.prefix, 1:cpus), timeout = options$fs.latency)

  return(sysOut)

}

gammastep3 <- function(file, trait, p, i, t, out.prefix, options) {

  check.options(options)

  args <- paste(paste0("--", trait),
                "--gammastep3",
                "-p", sprintf("%d", p),
                "-t", shQuote(t),
                "-o", shQuote(paste0(out.prefix, sprintf("%d", i), '.txt')),
                ifelse(is.null(options$r),
                       "",
                       paste("-r", sprintf("%d", options$r+i))),
                "-m", sprintf("%d", options$m),
                "-at", sprintf("%d", options$at),
                "-ct", sprintf("%d", options$ct),
                "-ac", sprintf("%d", options$ac),
                "-x", sprintf("%f", options$x),
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
                "&>", shQuote(paste0(out.prefix, sprintf("%d", i), '.log')))

  BBmisc::system3(command = options$exec,
                  args = args,
                  stdout = TRUE,
                  stderr = TRUE,
                  stop.on.exit.code = TRUE)

}
