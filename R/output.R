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
#' @return BatchJobs registry object.
#'
#' @export
createOutput <- function(file,
                         trait,
                         cpus,
                         reg.id = "output",
                         work.dir = getwd(),
                         reg.dir = file.path(work.dir, "registries", reg.id),
                         topfile = file.path(work.dir,
                                             paste(basename(file_path_sans_ext(file)),
                                                   "topfile", sep = ".")),
                         perm.prefix = file.path(work.dir,
                                                 "permutations",
                                                 paste(basename(file_path_sans_ext(file)),
                                                       "perm", sep = "_")),
                         out = file.path(work.dir,
                                         paste(basename(file_path_sans_ext(file)),
                                               "result", sep = ".")),
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

  options <- getOption("mbmdr")

  reg <- makeRegistry(reg.id,
                      file.dir = reg.dir,
                      work.dir = work.dir,
                      skip = skip,
                      seed = options$r,
                      packages = c('mbmdR'))

  jobs <- batchMap(reg, gammastep4,
                   c = perm.prefix,
                   more.args = list(file = file,
                                    trait = trait,
                                    q = cpus,
                                    p = options$p,
                                    o = out,
                                    t = topfile,
                                    options = getOption("mbmdr")))

  submitJobs(reg, chunk(jobs, chunk.size = 1),
             chunks.as.arrayjobs = getConfig()$ssh,
             resources = list(nodes = 1,
                              ppn = 1,
                              mem = paste0(ceiling(2*file.size(file)/1024^3), "g")),
             job.delay = TRUE)

  return(reg)

}

gammastep4 <- function(file, trait, c, q, p, t, o, options) {

  check.options(options)

  args <- paste(paste0("--", trait),
               "--gammastep4",
               "-c", c,
               "-q", sprintf("%d", q),
               "-p", sprintf("%d", p),
               "-m", sprintf("%d", options$m),
               "-at", sprintf("%d", options$at),
               "-ct", sprintf("%d", options$ct),
               "-ac", sprintf("%d", options$ac),
               "-x", sprintf("%f", options$x),
               "-t", t,
               "-o", o,
               "-r", options$r,
               "-a", options$a,
               "-rc", options$rc,
               ifelse(testCharacter(options$e),
                      paste("-e", paste(options$e, collapse = ",")),
                      ""),
               ifelse(testString(options$E),
                      paste("-E", options$E),
                      ""),
               ifelse(testCharacter(options$filter),
                      paste("-f", paste(options$filter, collapse = ",")),
                      ""),
               ifelse(testString(options$filter.file),
                      paste("-F", options$filter.file),
                      ""),
               ifelse(testCharacter(options$k),
                      paste("-k", paste(options$k, collapse = ",")),
                      ""),
               ifelse(testString(options$K),
                      paste("-K", options$K),
                      ""),
               "-if", options$input.format,
               ifelse(trait == "continuous",
                      paste0("-rt", options$rt),
                      ""),
               "-pb", options$pb,
               file)

  print(paste(options$exec, paste(args, collapse = " ")))

  BBmisc::system3(command = options$exec,
                  args = args,
                  stdout = TRUE,
                  stderr = TRUE,
                  stop.on.exit.code = TRUE)

}
