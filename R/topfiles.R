#' @title Maps a function over lists or vectors, adding jobs to a registry.
#'
#' @description
#' You can then submit these jobs to the batch system.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @examples
#'  x <- 1+1
#'  x
#'
#' @export
createPartialTopFiles <- function() {

}

gammastep1 <- function(file, trait, id, cpus) {

  check.options()

  options <- getOption("mbmdr")

  cmd <- paste(options$exec,
               " --", trait,
               " --gammastep1",
               " -i ", id,
               " -N ", cpus,
               " -n ", options$n,
               " -m ", options$m,
               " -a ", options$a,
               ifelse(testCharacter(options$e),
                      paste(" -e ", paste(options$e, collapse = ",")), ""),
               ifelse(testString(options$E),
                      paste(" -E ", options$E), ""),
               ifelse(testCharacter(options$filter),
                      paste(" -f ", paste(options$filter, collapse = ",")), ""),
               ifelse(testString(options$filter.file),
                      paste(" -F ", options$filter.file), ""),
               " -if ", options$input.format,
               " -rt ", options$rt,
               " -pb ", options$pb,
               " ", file,
               sep = "")

}
