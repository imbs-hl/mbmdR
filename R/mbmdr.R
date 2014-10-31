#' @title MB-MDR Analysis
#'
#' @description
#' Run a complete MB-MDR analysis. Necessary files and directories will be created in given working directory.
#'
#' @param file [\code{string}]\cr
#'   File path of input MB-MDR file.
#'
#' @param trait [\code{string}]\cr
#'   Type of trait. "binary", "continuous" or "survival".
#'
#' @param cpus.topfiles [\code{integer}]\cr
#'   Total amount of CPUs to be used for partial topfiles.
#'
#' @param cpus.permutations [\code{integer}]\cr
#'   Sets the total amount of CPUs to be used for permutations.
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param prefix.topfiles [\code{string}]\cr
#'   Path for saving the partial topfiles. Defaults to <\code{\link{getwd}()}>/topfiles/<\code{file}>_top.
#'
#' @param topfile [\code{string}]\cr
#'   Path for saving the final topfile. Defaults to <\code{\link{getwd}()}>/<\code{file}>.topfile.
#'
#' @param prefix.permutations [\code{string}]\cr
#'   Path for saving the permutation files. Defaults to <\code{\link{getwd}()}>/permutations/<\code{file}>_perm.
#'
#' @param resultfile [\code{string}]\cr
#'   Sets the output file name. Defaults to <\code{\link{getwd}()}>/<\code{file}>.result.
#'
#' @param exec [\code{string}]\cr
#'   Default mbmdr executable. Default: "mbmdr"
#'
#' @param n.pvalues [\code{integer}]\cr
#'   Number of p-value to compute. Default: 1000
#'
#' @param permutations [\code{integer}]\cr
#'   Permutation amount for multiple-testing. Default: 999
#'
#' @param random.seed [\code{integer} or \code{NULL}]\cr
#'   Random seed parameter. Default: \code{NULL} (random value)
#'
#' @param group.size [\code{integer}]\cr
#'   Minimum group size to be statistically relevant. Default: 10
#'
#' @param alpha [\code{number}]\cr
#'   Cutoff value for the chi-square test. Default: 0.1
#'
#' @param multi.test.corr [\code{string}]\cr
#'   Multiple-testing correction algorithm. "NONE", "MAXT", "MINP", "RAWP",
#'   "STRAT1", "STRAT2" or "gammaMAXT" (default).
#'
#' @param adjustment [\code{string}]\cr
#'   Adjust method to be used. "CODOMINANT" (default), "ADDITIVE" or "NONE".
#'
#' @param dim [\code{string}]\cr
#'   Dimension of interactions. "1D", "2D" (defaut) or "3D".
#'
#' @param verbose [\code{string}]\cr
#'   Verbose level. "NONE" (default), "SHORT", "MEDIUM" or "LONG".
#'
#' @param progressbar [\code{string}]\cr
#'   Progress bar. "NONE" or "NORMAL" (default).
#'
#' @param erase [\code{character} or \code{NULL}]\cr
#'   Vector of markers to be excluded. Default: \code{NULL}
#'
#' @param erase.file [\code{string} or \code{NULL}]\cr
#'   File of markers to be excluded. One marker per line. Default: \code{NULL}
#'
#' @param filter [\code{character} or \code{NULL}]\cr
#'   Analyse only the pairs composed of exactly one marker
#'   (for instance an environment variable) from the vector of markers names.
#'   Default: \code{NULL}
#'
#' @param filter.file [\code{string} or \code{NULL}]\cr
#'   Analyse only the pairs composed of exactly one marker
#'   (for instance an environment variable) from the file of markers names.
#'   One marker per line. Default: \code{NULL}
#'
#' @param input.format [\code{string}]\cr
#'   Input file format. "MDR" or "MBMDR" (default).
#'
#' @param transform [\code{string}]\cr
#'   Rank transformation (continuous trait only).
#'   "RANK_TRANSFORM" or "NONE" (default)
#'
#' @return
#' Throws an error if any check fails and invisibly returns TRUE otherwise.
#'
#' @export
mbmdr <- function(file,
                  trait,
                  cpus.topfiles,
                  cpus.permutations,
                  work.dir = getwd(),
                  prefix.topfiles = file.path(work.dir,
                                              "topfiles",
                                              paste0(gsub('(.+)\\.(.*)',
                                                          '\\1',
                                                          basename(file)),
                                                     "_top")),
                  topfile = file.path(work.dir,
                                      paste0(gsub('(.+)\\.(.*)',
                                                  '\\1',
                                                  basename(file)),
                                             ".topfile")),
                  prefix.permutations = file.path(work.dir,
                                                  "permutations",
                                                  paste0(gsub('(.+)\\.(.*)',
                                                              '\\1',
                                                              basename(file)),
                                                         "_perm")),
                  resultfile = file.path(work.dir,
                                         paste0(gsub('(.+)\\.(.*)',
                                                     '\\1',
                                                     basename(file)),
                                                ".result")),
                  exec = "mbmdr",
                  n.pvalues = 1000,
                  permutations = 999,
                  random.seed = NULL,
                  group.size = 10,
                  alpha = 0.1,
                  multi.test.corr = "gammaMAXT",
                  adjustment = "CODOMINANT",
                  dim = "2D",
                  verbose = "NONE",
                  progressbar  = "NORMAL",
                  erase = NULL,
                  erase.file = NULL,
                  filter = NULL,
                  filter.file = NULL,
                  input.format = "MBMDR",
                  transform = "NONE") {

  assertFile(file)
  assertChoice(trait, c("binary", "continuous", "survival"))
  assertInt(cpus.topfiles)
  assertInt(cpus.permutations)
  assertString(prefix.topfiles)
  assertString(topfile)
  assertString(prefix.permutations)
  assertString(resultfile)

  configure(exec,
            n.pvalues,
            permutations,
            random.seed,
            group.size,
            alpha,
            multi.test.corr,
            adjustment,
            dim,
            verbose,
            progressbar,
            erase,
            erase.file,
            filter,
            filter.file,
            input.format,
            transform)

  waitForJobs(createPartialTopFiles(file = file, trait = trait, cpus = cpus.topfiles, out.prefix = prefix.topfiles, work.dir = work.dir))
  waitForJobs(combinePartialTopFiles(file = file, trait = trait, cpus = cpus.topfiles, topfiles.prefix = prefix.topfiles, out = topfile, work.dir = work.dir))
  waitForJobs(runPermutations(file = file, trait = trait, cpus = cpus.permutations, topfile = topfile, out.prefix = prefix.permutations, work.dir = work.dir))
  waitForJobs(createOutput(file = file, trait = trait, cpus = cpus.permutations, topfile = topfile, out = resultfile, perm.prefix = prefix.permutations, work.dir = work.dir))

}
