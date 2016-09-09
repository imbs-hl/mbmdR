#' @title MB-MDR Analysis
#'
#' @description
#' Run a complete MB-MDR analysis. Necessary files and directories will be created in given working directory.
#'
#' @param formula [\code{formula}]\cr
#'   Object of class \code{formula} describing the model to fit. See details.
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
#'   Verbose level. "SHORT", "MEDIUM" (default) or "LONG".
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
#' @param ... [\code{any}]\cr
#'   Additional parameter passed to and from other methods.
#'
#' @details  The \code{formula} must have the following form:\cr
#'   \code{~trait1+trait2+...+traitnt+cov1+cov2+...+covnc+marker1+marker2+...+markernm}\cr
#'   or\cr
#'   \code{~trait1+trait2+...+cov1+cov2+.}, if there are many markers.\cr
#'   Here \code{trait1, trait2,..., traitnt} are the \code{nt} traits, \code{cov1, cov2, ..., covnc} are the \code{nc} covariates and \code{marker1, marker2, ..., markernm} are the \code{nm} markers, i.e. SNPs and/or environment variables to be exported to the MB-MDR file format. The first form let's you explicitly choose which variables are exported whereas the latter form exports all variables in data, but let's you re-arrange the ordering of variables. If you know that the variables in your data are correctly ordered, i.e. first the \code{nt} traits, then the \code{nc} variables and then the \code{nm} markers, you can simply provide the formula \code{~.}.\cr\cr
#'   WARNING: The formula interface is generally slow for big datasets. Consider to provide a correctly formatted file on the filesystem via \code{file} instead.
#'
#' @return
#' Throws an error if any check fails and returns a \code{\link{data.table}} otherwise.
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
                                              paste(ifelse(is.null(file),
                                                           "input",
                                                           basename(tools::file_path_sans_ext(file))),
                                                    "top", sep = "_")),
                  topfile = file.path(work.dir,
                                      paste(ifelse(is.null(file),
                                                   "input",
                                                   basename(tools::file_path_sans_ext(file))),
                                            "topfile", sep = ".")),
                  prefix.permutations = file.path(work.dir,
                                                  "permutations",
                                                  paste(ifelse(is.null(file),
                                                               "input",
                                                               basename(tools::file_path_sans_ext(file))),
                                                        "perm", sep = "_")),
                  resultfile = file.path(work.dir,
                                         paste(ifelse(is.null(file),
                                                      "input",
                                                      basename(tools::file_path_sans_ext(file))),
                                               "result", sep = ".")),
                  logfile = file.path(work.dir,
                                      paste(ifelse(is.null(file),
                                                   "input",
                                                   basename(tools::file_path_sans_ext(file))),
                                            "log", sep = ".")),
                  modelsfile = file.path(work.dir,
                                         paste(ifelse(is.null(file),
                                                      "input",
                                                      basename(tools::file_path_sans_ext(file))),
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
                  transform = "NONE", ...) {

  tryCatch(BBmisc::suppressAll(system(exec, intern = TRUE)))

  if(!checkmate::testNull(file)) {
    checkmate::assertFile(file)
  }
  if(checkmate::testNull(file) & checkmate::testNull(formula)) {
    stop("Either a file or formula and data must be given!")
  }
  if(!checkmate::testNull(formula) & checkmate::testNull(data)) {
    stop("Formula without data object given!")
  }
  if(!checkmate::testNull(formula)) {
    checkmate::assertClass(formula, "formula")
  }
  if(!checkmate::testNull(data)) {
    checkmate::assertDataFrame(data)
  }
  checkmate::assertChoice(trait, c("binary", "continuous", "survival"))
  checkmate::assertChoice(verbose, c("SHORT", "MEDIUM", "LONG"))
  checkmate::assertInt(cpus.topfiles)
  checkmate::assertInt(cpus.permutations)
  checkmate::assertInt(random.seed)
  checkmate::assertString(prefix.topfiles)
  checkmate::assertString(topfile)
  checkmate::assertString(prefix.permutations)
  checkmate::assertString(resultfile)
  if(!checkmate::testNull(replicate.file)) {
    checkmate::assertFile(replicate.file)
    replicate <- TRUE
  } else {
    replicate <- FALSE
  }
  if(!checkmate::testNull(bj.config)) {
    checkmate::assertFile(bj.config)
    BatchJobs::loadConfig(conffile = bj.config)
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

  if(!checkmate::testNull(formula)) {
    file <- file.path(work.dir, "input.mbmdr")

    if(file.exists(file.path(work.dir, "input.mbmdr"))) {
      message("Input file already exists. Using the existing one...")
      writeOut <- FALSE
    } else {
      # write out the R object to disk
      message("Writing out the R object to MB-MDR format on disk...")
      file <- file.path(work.dir, "input.mbmdr")
      out <- stats::model.frame(formula, data)
      out <- as.data.frame(lapply(out, FUN = function(x) {
        if(is.factor(x)) {
          levels(x) <- 0:(length(levels(x))-1)
          return(as.character(x))
        } else {
          return(x)
        }
      }))
      readr::write_delim(x = out,
                         path = file,
                         delim = " ",
                         na = "-9",
                         append = FALSE)
      writeOut <- TRUE
    }
  }

  # Check the number of columns
  awk <- tryCatch({
    BBmisc::suppressAll(system("awk -V", intern = TRUE))
    TRUE},
    error = function(e) {
      FALSE
    })
  if(awk) {
    # awk is available on the OS
    ncols <- as.numeric(BBmisc::system3(command = "awk",
                                        args = c("-F' '", "'{print NF; exit}'", file),
                                        stdout = TRUE,
                                        stderr = TRUE)$output)
  } else {
    # awk is not availabe on the OS, fall back to data.table's fread
    ncols <- ncol(data.table::fread(input = file, nrows = 1, header = FALSE))
  }

  if(ncols < 1000 | (cpus.topfiles==1 & cpus.permutations==1) | multi.test.corr != "gammaMAXT" | replicate) {

    message("Running the analysis as a single thread...\n")

    invisible(waitForJobs(runSingleThread(file = file,
                                          trait = trait,
                                          out = resultfile,
                                          log = logfile,
                                          mod = modelsfile,
                                          work.dir = work.dir, ...)))

  } else {
    message("Starting parallel workflow..\n")

    if(all(file.exists(paste0(prefix.topfiles, 1:cpus.topfiles, ".txt")))) {
      message("Found all topfiles. Skipping this step...")
      step1new <- FALSE
    } else {
      message("Creating partial topfiles on ", cpus.topfiles, " CPUs...\n")
      invisible(createPartialTopFiles(file = file,
                                      trait = trait,
                                      cpus = cpus.topfiles,
                                      out.prefix = prefix.topfiles,
                                      work.dir = work.dir, ...))
      step1new <- TRUE
    }

    if(file.exists(topfile)) {
      message("Found the combined topfile. Skipping this step...")
      step2new <- FALSE
    } else {
      message("Combining partial topfiles...\n")
      invisible(combinePartialTopFiles(file = file,
                                       trait = trait,
                                       cpus = cpus.topfiles,
                                       topfiles.prefix = prefix.topfiles,
                                       mod = modelsfile,
                                       out = topfile,
                                       work.dir = work.dir, ...))
      step2new <- TRUE
    }

    if(all(file.exists(paste0(prefix.permutations, 1:cpus.permutations, ".txt")))) {
      message("Found all permutation files. Skipping this step...")
      step3new <- FALSE
    } else {
      message("Running permutation test on ", cpus.permutations, " CPUs...\n")
      invisible(runPermutations(file = file,
                                trait = trait,
                                cpus = cpus.permutations,
                                topfile = topfile,
                                out.prefix = prefix.permutations,
                                work.dir = work.dir, ...))
      step3new <- TRUE
    }

    if(file.exists(resultfile)) {
      message("Found the result file. Skipping this step...")
      step4new <- FALSE
    } else {
      message("Creating output...\n")
      invisible(createOutput(file = file, trait = trait,
                             cpus = cpus.permutations,
                             topfile = topfile,
                             out = resultfile,
                             perm.prefix = prefix.permutations,
                             work.dir = work.dir, ...))
      step4new <- TRUE
    }

  }

  res <- data.table::fread(resultfile)
  data.table::setnames(res, c("Marker1", "Marker2", "TestStat", "pValue"))

  return(res)
}
