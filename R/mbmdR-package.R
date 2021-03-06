#' mbmdR
#'
#' @name mbmdR
#' @docType package
#'
#' @import checkmate BBmisc tools data.table backports
NULL

.onLoad <- function(libname, pkgname, ...) {

  options(mbmdr = list(exec = "mbmdr",         # Set default mbmdr executable
                       n = 1000,               # Number of p-value to compute
                       p = 999,                # Permutation amount for multiple-testing
                       r = NULL,               # Random seed parameter
                       m = 10,                 # Minimum group size to be statistically relevant
                       x = 0.1,                # Cutoff value for the chi-square test
                       mt = "gammaMAXT",       # Multiple-testing correction algorithm
                       o = "_output.txt",      # Output file name
                       a = "CODOMINANT",       # Adjust method to be used
                       d = "2D",               # Dimension of interactions
                       v = "NONE",             # Verbose
                       pb = "NORMAL",          # Progress bar
                       e = NULL,               # Erase markers (list)
                       E = NULL,               # Erase markers (file)
                       filter = NULL,          # Filter (list)
                       filter.file = NULL,     # Filter (file)
                       input.format = "MBMDR", # Input format
                       rt = "NONE")            # Rank transformation
  )

}

.onAttach = function(libname, pkgname) {
  parallelMap::parallelRegisterLevels(package = pkgname, levels = c("parallelSteps"))
}
