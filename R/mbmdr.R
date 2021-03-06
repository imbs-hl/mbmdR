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
#'   Sets the log file name. Defaults to <\code{work.dir}>/<\code{file}>.log.
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
#' @param result.only [\code{bool}]\cr
#'   Return only a data.table object with the results or a list with an element
#'   for each combination with more detailed informations about HLO tables etc. (default).
#'
#' @param fs.latency [\code{number}]\cr
#'   File system latency in seconds to use to wait for files. Default is \code{65} seconds.
#'
#' @param clean [\code{logical}]\cr
#'   Shall intermediate files be deleted? Default is \code{FALSE}.
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
                  random.seed = as.integer(stats::runif(1, min = 1, max = .Machine$integer.max)),
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
                  result.only = FALSE,
                  fs.latency = 65,
                  clean = FALSE, ...) {

  old_warn_level <- getOption("warn")
  options("warn" = 1)
  on.exit(options("warn" = old_warn_level))

  tryCatch(BBmisc::suppressAll(system(exec, intern = TRUE)),
           error = function(e) {
             stop("MB-MDR executable not found!")
           })

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
  dir.create(work.dir, recursive = TRUE)
  checkmate::assertFlag(result.only)
  checkmate::assertFlag(clean)

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
            transform,
            fs.latency)

  utils::flush.console()

  if(!checkmate::testNull(formula)) {
    file <- file.path(work.dir, "input.mbmdr")

    if(file.exists(file.path(work.dir, "input.mbmdr"))) {
      message("Input file already exists. Using the existing one...")
      utils::flush.console()
      writeOut <- FALSE
    } else {
      # write out the R object to disk
      message("Writing out the R object to MB-MDR format on disk...")
      utils::flush.console()
      file <- file.path(work.dir, "input.mbmdr")
      mf <- stats::model.frame(formula, data)
      mt <- attr(mf, "terms")
      out <- mf[, attr(mt, "term.labels")]
      out <- as.data.frame(lapply(out, FUN = function(x) {
        x <- factor(x)
        if(is.factor(x)) {
          levels(x) <- 0:(length(levels(x))-1)
          return(as.character(x))
        } else {
          return(x)
        }
      }))
      data.table::fwrite(cbind("y" = mf[, 1], out),
                         file = file,
                         sep = " ",
                         append = FALSE,
                         na = "-9")
      writeOut <- TRUE
    }
  }

  # Check the number of columns
  ncols <- tryCatch({
    sysOut <- (system2(command = "awk",
                       args = c("-F' '", "'{print NF; exit}'", shQuote(file)),
                       stdout = TRUE,
                       stderr = TRUE))

    if(!is.null(attr(sysOut, "status"))) {
      stop(sysOut)
    } else {
      as.integer(sysOut)
    }

  }, error = function(e) {
    message("Command line program 'awk' not found or failed. Using R code as fallback.")
    utils::flush.console()
    ncol(data.table::fread(input = file, nrows = 1, header = FALSE))
  })

  options <- getOption("mbmdr")

  if(ncols < 1000 |
     (cpus.topfiles==1 & cpus.permutations==1) |
     options$mt != "gammaMAXT" |
     replicate) {

    message("Running the analysis as a single thread...\n")
    utils::flush.console()


    if(any(file.exists(c(resultfile, logfile, modelsfile)))) {
      message("Found output files. Skipping new analysis...")
      utils::flush.console()
      single_new <- FALSE
    } else {
      invisible(runSingleThread(file = file,
                                trait = trait,
                                out = resultfile,
                                log = logfile,
                                mod = modelsfile,
                                work.dir = work.dir, ...))
    }

  } else {
    message("Starting parallel workflow..\n")
    utils::flush.console()

    if(all(file.exists(paste0(prefix.topfiles, 1:cpus.topfiles, ".txt")))) {
      message("Found all topfiles. Skipping this step...")
      utils::flush.console()
      step1new <- FALSE
    } else {
      message("Creating partial topfiles on ", cpus.topfiles, " CPUs...\n")
      utils::flush.console()
      invisible(createPartialTopFiles(file = file,
                                      trait = trait,
                                      cpus = cpus.topfiles,
                                      out.prefix = prefix.topfiles,
                                      work.dir = work.dir, ...))
      step1new <- TRUE
    }

    if(file.exists(topfile)) {
      message("Found the combined topfile. Skipping this step...")
      utils::flush.console()
      step2new <- FALSE
    } else {
      message("Combining partial topfiles...\n")
      utils::flush.console()
      invisible(combinePartialTopFiles(file = file,
                                       trait = trait,
                                       cpus = cpus.topfiles,
                                       topfiles.prefix = prefix.topfiles,
                                       mod = modelsfile,
                                       out = topfile,
                                       work.dir = work.dir,
                                       logfile = logfile, ...))
      step2new <- TRUE
    }

    if(all(file.exists(paste0(prefix.permutations, 1:cpus.permutations, ".txt")))) {
      message("Found all permutation files. Skipping this step...")
      utils::flush.console()
      step3new <- FALSE
    } else {
      message("Running permutation test on ", cpus.permutations, " CPUs...\n")
      utils::flush.console()
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
      utils::flush.console()
      step4new <- FALSE
    } else {
      message("Creating output...\n")
      utils::flush.console()
      invisible(createOutput(file = file, trait = trait,
                             cpus = cpus.permutations,
                             topfile = topfile,
                             out = resultfile,
                             perm.prefix = prefix.permutations,
                             work.dir = work.dir,
                             logfile = logfile, ...))
      step4new <- TRUE
    }

  }

  if(!result.only) {
    output <- list()
    output$mdr_models <- read(resultfile, logfile, modelsfile, trait, options, result.only)
    output$options <- options
    output$call <- sys.call()
    class(output$mdr_models) <- "mdr_models"
    class(output) <- "mbmdr"
  } else {
    output <- read(resultfile, logfile, modelsfile, trait, options, result.only)
  }

  # Clean up intermediate file
  if(clean) {
    unlink(topfile, recursive = TRUE, force = TRUE)
    unlink(dirname(prefix.topfiles), recursive = TRUE, force = TRUE)
    unlink(dirname(prefix.permutations), recursive = TRUE, force = TRUE)
    unlink(modelsfile, recursive = TRUE, force = TRUE)
    unlink(resultfile, recursive = TRUE, force = TRUE)
    unlink(logfile, recursive = TRUE, force = TRUE)
  }

  return(output)

}

#' Combine MB-MDR objects
#'
#' @param ...         [\code{mdr_models}]\cr
#'                    \code{mdr_models} objects to be concatenated.
#' @param recursive   [\code{logical}]\cr
#'                    Not used.
#'
#' @details The concatenated results are sorted according to the test statistics
#' of the models.
#'
#' @return A \code{mdr_models} object of concatenated \code{mdr_models} objects.
#' @export
c.mdr_models <- function(..., recursive = FALSE) {
  dots <- list(...)
  res <- unlist(dots, recursive = FALSE)
  res <- res[order(sapply(res, function(model) model$statistic), decreasing = TRUE)]
  class(res) <- "mdr_models"
  return(res)
}

#' Extract a subset of MDR models
#' @param mdr_models [\code{mdr_models}]\cr
#'                   A list of \code{mdr_model}s of class \code{mdr_models}.
#'
#' @param i [\code{integer}]\cr
#'          A vector of integers, indicating which MDR models to extract.
#'
#' @return A list of \code{mdr_model}s of class \code{mdr_models}.
#'
#' @export
`[.mdr_models` <- function(mdr_models, i) {
  checkmate::assertClass(mdr_models, c("mdr_models"))
  checkmate::assertInteger(i, lower = 1, any.missing = FALSE, min.len = 1)

  class(mdr_models) <- NULL
  res <- mdr_models[i]
  class(res) <- "mdr_models"
  return(res)
}

#' Extract a subset of MDR models from a MB-MDR result object
#' @param mbmdr [\code{mbmdr}]\cr
#'                   A \code{mbmdr} object.
#'
#' @param i [\code{integer}]\cr
#'          A vector of integers, indicating which MDR models to extract.
#'
#' @return A list of \code{mdr_model}s of class \code{mdr_models}.
#'
#' @export
`[.mbmdr` <- function(mbmdr, i) {
  checkmate::assertClass(mbmdr, c("mbmdr"))
  checkmate::assertInteger(i, lower = 1, any.missing = FALSE, min.len = 1)

  mbmdr$mdr_models[i]

}

#' @export
print.mdr_model <- function(mdr_model) {
  num_rows <- attr(mdr_model$cell_labels, "num_rows")
  predictions <- matrix(mdr_model$cell_predictions, nrow = num_rows)
  hlo_table <- matrix(mdr_model$cell_labels, nrow = num_rows)
  num_cols <- ncol(hlo_table)
  cat(sprintf("    MDR model of features %s\n\n", paste(mdr_model$features, collapse = ", ")))
  cat(sprintf(sprintf("      %% %ds\t\t%% %ds\n",
                      num_cols*6 + (num_cols - 1),
                      num_cols*4 + (num_cols - 1)),
              "Average trait", "HLO matrix"))
  for (r in seq_len(num_rows)) {
    cat(sprintf("        "))
    cat(sprintf("% 6.4f", predictions[r, ]))
    cat(sprintf("\t\t"))
    cat(sprintf("% 4s", hlo_table[r, ]))
    cat("\n")
  }
  cat("\n")
  cat(sprintf("      Test statistic: %.4f\n", mdr_model$statistic))
  cat(sprintf("      P value: %.4f\n", mdr_model$pvalue))
  cat("\n")
}

#' @export
print.mdr_models <- function(mdr_models, n = 5) {

  num_models <- length(mdr_models)

  cat(sprintf("List of %d MDR models:\n\n", num_models))
  for (i in seq_len(min(n, num_models))) {
    cat("  ", rep("-", 30), "\n")
    print(mdr_models[[i]])
  }
  if (n < num_models) {
    cat("   ...\n")
    cat(sprintf("Output truncated after %d MDR models!", n))
  }
}

#' @export
print.mbmdr <- function(mbmdr, n = 1) {
  cat(deparse(mbmdr$call, width.cutoff = getOption("width")), sep = "\n")
  cat("\n")
  print(mbmdr$mdr_models, n = n)
}

#' @export
as.data.frame.mdr_model <- function(mdr_model) {
  num_features <- length(mdr_model$features)
  df <- data.frame(as.list(mdr_model$features),
                   STATISTIC = mdr_model$statistic,
                   PVALUE = mdr_model$pvalue,
                   stringsAsFactors = FALSE)
  names(df) <- c(sprintf("FEATURE%d", 1:num_features), "STATISTIC", "PVALUE")
  return(df)
}

#' @export
as.data.frame.mdr_models <- function(mdr_models) {
  df_list <- lapply(mdr_models, as.data.frame)
  df <- do.call(dplyr::bind_rows, args = df_list)
  return(df)
}

#' @export
as.data.frame.mbmdr <- function(mbmdr) {
  as.data.frame(mbmdr$mdr_models)
}
