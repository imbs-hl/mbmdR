#' @title MB-MDR Analysis
#'
#' @description
#' Run a complete MB-MDR analysis. Necessary files and directories will be created in given working directory.
#'
#' @param formula [\code{formula}]\cr
#'   Object of class \code{formula} describing the model to fit.
#'
#' @param data [\code{data.frame}]\cr
#'   Object containing all the data.
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
#'   Path for saving the partial topfiles. Defaults to <\code{work.dir}>/topfiles/<\code{file}>_top.
#'
#' @param topfile [\code{string}]\cr
#'   Path for saving the final topfile. Defaults to <\code{work.dir}>/<\code{file}>.topfile.
#'
#' @param prefix.permutations [\code{string}]\cr
#'   Path for saving the permutation files. Defaults to <\code{work.dir}>/permutations/<\code{file}>_perm.
#'
#' @param resultfile [\code{string}]\cr
#'   Sets the output file name. Defaults to <\code{work.dir}>/<\code{file}>.result.
#'
#' @param logfile [\code{string}]\cr
#'   Sets the output file name. Defaults to <\code{work.dir}>/<\code{file}>.log.
#'
#' @param modelsfile [\code{string}]\cr
#'   Sets the models file name. Defaults to <\code{work.dir}>/<\code{file}>.models.
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
#' @param num.traits [\code{integer}]\cr
#'   Amount of traits.
#'   Default: 1
#'
#' @param current.trait [\code{integer}]\cr
#'   The selected trait.
#'   Default: 1
#'
#' @param num.covariates [\code{integer}]\cr
#'   Amount of covariates.
#'   Default: 0
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
#' @param cov.adjustment [\code{string}]\cr
#'   Covariable adjust method to be used. "RESIDUALS" (default) or "ONTHEFLY.
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
#' @param keep [\code{character} or \code{NULL}]\cr
#'   Keep only the markers from this vector of marker names.
#'   Default: \code{NULL}
#'
#' @param keep.file [\code{string} or \code{NULL}]\cr
#'   Keep only the markers from the given file. One marker per line.
#'   Default: \code{NULL}
#'
#' @param replicate.file [\code{string} or \code{NULL}]\cr
#'   Used as the second stage of a discovery-replication analysis: keep only the
#'   pairs from the given output of the first stage.
#'   Default: \code{NULL}
#'
#' @param input.format [\code{string}]\cr
#'   Input file format. "MDR" or "MBMDR" (default).
#'
#' @param transform [\code{string}]\cr
#'   Rank transformation (continuous trait only).
#'   "RANK_TRANSFORM" or "NONE" (default)
#'
#' @param bj.config [\code{string}]\cr
#'   Location of the configuration file to load.
#'   Default is ".BatchJobs.conf" in the current working directory.
#'
#' @param cutting_value = "-a" [\code{double"-a", "-b" or numeric}]\cr
#'   Defines the rule, which HLO-tables should be read in at the end of mbmdr.
#'   "-a" means: all with at least one significant value
#'   "-b" means: all with at least one "H" and one "L" and p < .05
#'   numeric: all with p-value below the given number ("=" excluded)
#'
#' @param ... [\code{any}]\cr
#'   Additional parameter passed to and from other methods.
#'
#' @return
#' Throws an error if any check fails and returns TRUE otherwise.
#'
#' @export
mbmdr <- function(formula = NULL,
                  data = NULL,
                  file = NULL,
                  trait,
                  cpus.topfiles,
                  cpus.permutations,
                  work.dir = getwd(),
                  prefix.topfiles = file.path(work.dir,
                                              "topfiles",
                                              paste(ifelse(is.null(file), "input", basename(file_path_sans_ext(file))),
                                                    "top", sep = "_")),
                  topfile = file.path(work.dir,
                                      paste(ifelse(is.null(file), "input", basename(file_path_sans_ext(file))),
                                            "topfile", sep = ".")),
                  prefix.permutations = file.path(work.dir,
                                                  "permutations",
                                                  paste(ifelse(is.null(file), "input", basename(file_path_sans_ext(file))),
                                                        "perm", sep = "_")),
                  resultfile = file.path(work.dir,
                                         paste(ifelse(is.null(file), "input", basename(file_path_sans_ext(file))),
                                               "result", sep = ".")),
                  logfile = file.path(work.dir,
                                      paste(ifelse(is.null(file), "input", basename(file_path_sans_ext(file))),
                                            "log", sep = ".")),
                  modelsfile = file.path(work.dir,
                                        paste(ifelse(is.null(file), "input", basename(file_path_sans_ext(file))),
                                              "models", sep = ".")),
                  exec = "mbmdr",
                  n.pvalues = 1000,
                  permutations = 999,
                  random.seed = as.integer(Sys.Date()),
                  group.size = 10,
                  num.traits = 1,
                  current.trait = 1,
                  num.covariates = 0,
                  alpha = 0.1,
                  multi.test.corr = "gammaMAXT",
                  adjustment = "CODOMINANT",
                  cov.adjustment = "RESIDUALS",
                  dim = "2D",
                  verbose = "MEDIUM",
                  progressbar  = "NORMAL",
                  erase = NULL,
                  erase.file = NULL,
                  filter = NULL,
                  filter.file = NULL,
                  keep = NULL,
                  keep.file = NULL,
                  replicate.file = NULL,
                  input.format = "MBMDR",
                  transform = "NONE",
                  bj.config = NULL,
                  cutting_value = "-a", ...) {

  tryCatch(suppressAll(system(exec, intern = TRUE)))

  if(!testNull(file)) {
    assertFile(file)
  }
  if(testNull(file) & testNull(formula)) {
    stop("Either a file or formula and data must be given!")
  }
  if(!testNull(formula) & testNull(data)) {
    stop("Formula without data object given!")
  }
  if(!testNull(formula)) {
    assertClass(formula, "formula")
  }
  if(!testNull(data)) {
    assertDataFrame(data)
  }
  assertChoice(trait, c("binary", "continuous", "survival"))
  assertInt(cpus.topfiles)
  assertInt(cpus.permutations)
  assertInt(random.seed)
  assertString(prefix.topfiles)
  assertString(topfile)
  assertString(prefix.permutations)
  assertString(resultfile)
  if(!testNull(bj.config)) {
    assertFile(bj.config)
    loadConfig(conffile = bj.config)
  }
  dir.create(work.dir, recursive = TRUE)

  configure(exec,
            n.pvalues,
            permutations,
            random.seed,
            group.size,
            num.traits,
            current.trait,
            num.covariates,
            alpha,
            multi.test.corr,
            adjustment,
            cov.adjustment,
            dim,
            verbose,
            progressbar,
            erase,
            erase.file,
            filter,
            filter.file,
            keep,
            keep.file,
            replicate.file,
            input.format,
            transform)

  clean(work.dir = work.dir)

  if(!testNull(formula)) {
    file <- file.path(work.dir, "input.mbmdr")
    out <- sapply(X = model.frame(formula, data),
                  FUN = function(x){x <- as.numeric(as.character(x))
                                    if(min(x)<0){
                                      x+abs(min(x))
                                    } else {
                                      x
                                    }
                  })
    write.table(x = out,
                file = file,
                quote = FALSE,
                na = "-9",
                row.names = FALSE,
                col.names = TRUE)
  } else {
    data <- read.table(file, header = TRUE, nrows = 1)
  }


  if(ncol(data)<1000 | (cpus.topfiles==1 & cpus.permutations==1) | multi.test.corr != "gammaMAXT") {

    message("Running the analysis as a single thread...\n")

    invisible(waitForJobs(runSingleThread(file = file,
                                          trait = trait,
                                          out = resultfile,
                                          log = logfile,
                                          mod = modelsfile,
                                          work.dir = work.dir, ...)))

  } else {
    message("Starting parallel workflow..\n")

    message("Creating partial topfiles on ", cpus.topfiles, " CPUs...\n")
    invisible(waitForJobs(createPartialTopFiles(file = file,
                                                trait = trait,
                                                cpus = cpus.topfiles,
                                                out.prefix = prefix.topfiles,
                                                work.dir = work.dir, ...)))

    message("Combining partial topfiles...\n")
    invisible(waitForJobs(combinePartialTopFiles(file = file,
                                                 trait = trait,
                                                 cpus = cpus.topfiles,
                                                 topfiles.prefix = prefix.topfiles,
                                                 out = topfile,
                                                 work.dir = work.dir, ...)))

    message("Running permutation test on ", cpus.permutations, " CPUs...\n")
    invisible(waitForJobs(runPermutations(file = file,
                                          trait = trait,
                                          cpus = cpus.permutations,
                                          topfile = topfile,
                                          out.prefix = prefix.permutations,
                                          work.dir = work.dir, ...)))

    message("Creating output...\n")
    invisible(waitForJobs(createOutput(file = file, trait = trait,
                                       cpus = cpus.permutations,
                                       topfile = topfile,
                                       out = resultfile,
                                       perm.prefix = prefix.permutations,
                                       work.dir = work.dir, ...)))

  }



  return(reading(cutting_value = cutting_value,
                 correction = adjustment,
                 inputFile_tables = modelsfile,
                 inputFile_list = resultfile,
                 daten = data
                 )
         )
}
