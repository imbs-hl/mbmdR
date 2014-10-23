#' @title Set options
#'
#' @description
#' Configure the options of MB-MDR.
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
#' @param out.suffix [\code{string}]\cr
#'   Suffix for output file name. Default: "_output.txt".
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
configure <- function(exec = "mbmdr",
                      n.pvalues = 1000,
                      permutations = 999,
                      random.seed = NULL,
                      group.size = 10,
                      alpha = 0.1,
                      multi.test.corr = "gammaMAXT",
                      out.suffix = "_output.txt",
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

  mbmdr <- list(exec = exec,                      # Set default mbmdr executable
                n = n.pvalues,               # Number of p-value to compute
                p = permutations,            # Permutation amount for multiple-testing
                r = random.seed,             # Random seed parameter
                m = group.size,              # Minimum group size to be statistically relevant
                x = alpha,                   # Cutoff value for the chi-square test
                mt = multi.test.corr,        # Multiple-testing correction algorithm
                o = out.suffix,              # Output file name
                a = adjustment,              # Adjust method to be used
                d = dim,                     # Dimension of interactions
                v = verbose,                 # Verbose
                pb = progressbar,            # Progress bar
                e = erase,                   # Erase markers (list)
                E = erase.file,              # Erase markers (file)
                filter = filter,             # Filter (list)
                filter.file = filter.file,   # Filter (file)
                input.format = input.format, # Input format
                rt = transform)              # Rank transformation

  check.options(mbmdr)

  options(mbmdr = mbmdr)

  invisible(TRUE)

}
