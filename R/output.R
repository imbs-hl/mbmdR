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
#' @param reg.id [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}. Defaults to "output".
#'
#' @param work.dir [\code{string}]\cr
#'   Working directory for MB-MDR. Defaults to current working directory.
#'
#' @param reg.dir [\code{string}]\cr
#'   Path for saving the \link{BatchJobs} \link{Registry}. Defaults to <\code{work.dir}>/registries/<\code{reg.id}>.
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
#' @param skip [\code{logical}]\cr
#'   Skip creation of a new registry if a registry is found in file.dir. Defaults to TRUE.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
createOutput <- function(file,
                         trait,
                         cpus,
                         reg.id = "output",
                         work.dir = getwd(),
                         reg.dir = file.path(work.dir, "registries", reg.id),
                         topfile = file.path(work.dir,
                                             paste0(gsub('(.+)\\.(.*)',
                                                         '\\1',
                                                         basename(file)),
                                                    ".topfile")),
                         perm.prefix = file.path(work.dir,
                                                 "permutations",
                                                 paste0(gsub('(.+)\\.(.*)',
                                                             '\\1',
                                                             basename(file)),
                                                        "_perm")),
                         out = file.path(work.dir,
                                         paste0(gsub('(.+)\\.(.*)',
                                                     '\\1',
                                                     basename(file)),
                                                ".result")),
                         skip = TRUE) {

  assertFile(file)
  assertChoice(trait, c("binary", "continuous", "survival"))
  assertNumber(cpus)
  assertString(reg.id)
  assertDirectory(work.dir)
  if(!testDirectory(reg.dir)) {
    warning(paste(checkDirectory(reg.dir), "will be created!", sep = ", "))
    dir.create(reg.dir, recursive = TRUE)
  }
  assertDirectory(reg.dir)
  if(!testDirectory(dirname(out))) {
    warning(paste(checkDirectory(dirname(out)), "will be created!", sep = ", "))
    dir.create(dirname(out), recursive = TRUE)
  }
  assertDirectory(dirname(out))
  assertDirectory(dirname(perm.prefix))
  assertFile(paste0(perm.prefix, 1:cpus, ".txt"))
  assertLogical(skip)

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      packages = c('mbmdR'))

  options <- getOption("mbmdr")

  jobs <- batchMap(reg, gammastep4,
                   c = perm.prefix,
                   more.args = list(file = file,
                                    trait = trait,
                                    q = cpus,
                                    p = options$p,
                                    o = out,
                                    t = topfile,
                                    options = getOption("mbmdr")))

  submitJobs(reg, chunk(jobs, chunk.size = 1000),
             chunks.as.arrayjobs = getConfig()$ssh,
             job.delay = TRUE)

  return(reg)

}

gammastep4 <- function(file, trait, c, q, p, t, o, options) {

  check.options(options)

  cmd <- paste(options$exec,
               " --", trait,
               " --gammastep4",
               " -c ", c,
               " -q ", sprintf("%d", q),
               " -p ", sprintf("%d", p),
               " -x ", sprintf("%f", options$x),
               " -t ", t,
               " -o ", o,
               ifelse(testNull(options$r), "", paste0(" -r ", options$r)),
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
               ifelse(trait == "continuous", paste0(" -rt ", options$rt), ""),
               " -pb ", options$pb,
               " ", file,
               sep = "")

  print(cmd)

  system(cmd)

}
