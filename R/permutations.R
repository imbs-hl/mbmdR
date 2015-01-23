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
#' @param reg.id [\code{string}]\cr
#'   Name for the \link{BatchJobs} \link{Registry}. Defaults to "permutations".
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
#' @param out.prefix [\code{string}]\cr
#'   Path for saving the permutation files. Defaults to <\code{work.dir}>/permutations/<\code{file}>_perm.
#'
#' @param skip [\code{logical}]\cr
#'   Skip creation of a new registry if a registry is found in file.dir. Defaults to TRUE.
#'
#' @return BatchJobs registry object.
#'
#' @export
runPermutations <- function(file,
                            trait,
                            cpus,
                            reg.id = "permutations",
                            work.dir = getwd(),
                            reg.dir = file.path(work.dir, "registries", reg.id),
                            topfile = file.path(work.dir,
                                                paste0(gsub('(.+)\\.(.*)',
                                                            '\\1',
                                                            basename(file)),
                                                       ".topfile")),
                            out.prefix = file.path(work.dir,
                                                   "permutations",
                                                   paste0(gsub('(.+)\\.(.*)',
                                                               '\\1',
                                                               basename(file)),
                                                          "_perm")),
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
  if(!testDirectory(dirname(out.prefix))) {
    warning(paste(checkDirectory(dirname(out.prefix)), "will be created!", sep = ", "))
    dir.create(dirname(out.prefix), recursive = TRUE)
  }
  assertDirectory(dirname(out.prefix))
  assertLogical(skip)

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      packages = c('mbmdR'))

  options <- getOption("mbmdr")

  p <- rep(options$p %/% cpus, cpus)
  rem <- options$p %% cpus

  if (rem > 0) {
    p[1:(options$p %% cpus)] <- p[1:(options$p %% cpus)]+1
  }

  jobs <- batchMap(reg, gammastep3,
                   p = p, i = 1:cpus,
                   more.args = list(file = file,
                                    trait = trait,
                                    t = topfile,
                                    out.prefix = out.prefix,
                                    options = getOption("mbmdr")))

  submitJobs(reg, chunk(jobs, chunk.size = 1000),
             chunks.as.arrayjobs = getConfig()$ssh,
             job.delay = TRUE)

  return(reg)

}

gammastep3 <- function(file, trait, p, i, t, out.prefix, options) {

  check.options(options)

  cmd <- paste(options$exec,
               " --", trait,
               " --gammastep3",
               " -p ", sprintf("%d", p),
               " -t ", t,
               " -o ", paste0(out.prefix, sprintf("%d", i), '.txt'),
               ifelse(testNull(options$r), "", paste0(" -r ", sprintf("%d", options$r))),
               " -m ", sprintf("%d", options$m),
               " -x ", sprintf("%f", options$x),
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
