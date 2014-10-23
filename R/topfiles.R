#' @title Create partial top files
#'
#' @description
#' First step of parallel workflow of MB-MDR. Creates partial topfiles on multiple CPUs.
#'
#' @param id [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}. Defaults to "partialTopFiles".
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param reg.dir [\code{string}]\cr
#'   Path for saving the \link{BatchJobs} \link{Registry}. Defaults to <\code{work.dir}>/registries/<\code{id}>.
#'
#' @param skip [\code{logical}]\cr
#'   Skip creation of a new registry if a registry is found in file.dir. Defaults to TRUE.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
createPartialTopFiles <- function(id = "partialTopFiles",
                                  work.dir = getwd(),
                                  reg.dir = file.path(work.dir, "registries", id),
                                  skip = TRUE) {

  makeRegistry("partialTopFiles",
               file.dir = reg.dir,
               work.dir = work.dir,
               skip = skip)

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
