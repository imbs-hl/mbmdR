
read <- function(resultfile, logfile, modelsfile, trait, options) {

  # Prepare output
  out <- list()

  # Read result file ----
  result <- data.table::fread(resultfile, skip = 3, head = FALSE)

  # Set column names of result file
  colnames <- switch(options$d,
                     "1D" = c("Marker1", "TestStat"),
                     "2D" = c("Marker1", "Marker2", "TestStat"),
                     "3D" = c("Marker1", "Marker2", "Marker3", "TestStat"))
  if(ncol(result) > length(colnames)) {
    colnames <- c(colnames, "pValue")
  }
  data.table::setnames(result, colnames)

  out$result <- result

  # Read log file ----
  log <- readLines(logfile)

  out$log <- log

  # Read models file ----
  models <- readLines(modelsfile)
  models <- models[models != ""]

  models <- if(trait == "binary" & options$ac == 0) {
    if(options$v == "MEDIUM") {
      read_medium_binary_models(models)
    } else if(options$v == "LONG") {
      read_long_binary_models(models, options$a)
    }
  } else if(trait == "continuous" | (trait == "binary" & options$ac > 0)) {
    if(options$v == "MEDIUM") {
      read_medium_contiuous_models(models)
    } else if(options$v == "LONG") {
      read_long_contiuous_models(models, options$a)
    }
  } else {
    NULL
  }

  out$models <- models

  models <- list()
  for(i in 1:nrow(result)) {
    model <- list()
    model$features <- out$models$modelnames[[i]]
    model$statistic <- out$result[i]$TestStat
    model$pvalue <- out$result[i]$pValue
    model$cell_predictions <- out$models$cell_predictions[[i]]
    model$cell_labels <- out$models$cell_labels[[i]]
    models[[i]] <- model
  }

  return(models)

}

read_long_contiuous_models <- function(models, adjustment) {
  subject_begin <- grep("Amount subjects:", models)
  trait_begin <- grep("Average continuous trait:", models)
  matrices_begin <- grep(switch(adjustment,
                                "ADDITIVE" = "HLO matrix: with ADDITIVE correction",
                                "CODOMINANT" = "HLO matrix: with CODOMINANT correction",
                                "HLO matrix: WITHOUT correction"), models)

  process_continuous(subject_begin, trait_begin, matrices_begin, models)
}

read_long_binary_models <- function(models, adjustment) {
  affected_begin <- grep("Affected subjects:", models)
  unaffected_begin <- grep("Unaffected subjects:", models)
  matrices_begin <- grep(switch(adjustment,
                                "ADDITIVE" = "HLO matrix: with ADDITIVE correction",
                                "CODOMINANT" = "HLO matrix: with CODOMINANT correction",
                                "HLO matrix: WITHOUT correction"), models)

  process_binary(affected_begin, unaffected_begin, matrices_begin, models)
}

read_medium_contiuous_models <- function(models) {
  subject_begin <- grep("Amount subjects:", models)
  trait_begin <- grep("Average continuous trait:", models)
  matrices_begin <- grep("HLO matrix:", models)

  process_continuous(subject_begin, trait_begin, matrices_begin, models)
}

read_medium_binary_models <- function(models) {
  affected_begin <- grep("Affected subjects:", models)
  unaffected_begin <- grep("Unaffected subjects:", models)
  matrices_begin <- grep("HLO matrix:", models)

  process_binary(affected_begin, unaffected_begin, matrices_begin, models)
}

process_continuous <- function(subject_begin, trait_begin, matrices_begin, models) {
  modelnames <- strsplit(models[subject_begin-1], split = " ")

  num_models <- length(modelnames)

  num_subjects <- list()
  average_trait <- list()
  matrices <- list()

  for(i in 1:num_models) {

    num_rows <- trait_begin[i] - subject_begin[i] - 1

    num_subjects[[i]] <- as.integer(matrix(scan(text = models[(subject_begin[i]+1):(subject_begin[i]+num_rows)],
                                                quiet = TRUE),
                                           nrow = num_rows, byrow = TRUE))
    attr(num_subjects[[i]], "num_rows") <- num_rows

    average_trait[[i]] <- as.numeric(matrix(scan(text = models[(trait_begin[i]+1):(trait_begin[i]+num_rows)],
                                                 quiet = TRUE),
                                            nrow = num_rows, byrow = TRUE))
    attr(num_subjects[[i]], "num_rows") <- num_rows

    matrices[[i]] <- as.character(matrix(scan(text = models[(matrices_begin[i]+1):(matrices_begin[i]+num_rows)],
                                              what = "character",
                                              quiet = TRUE),
                                         nrow = num_rows, byrow = TRUE))
    attr(matrices[[i]], "num_rows") <- num_rows

  }

  models <- list(modelnames = modelnames,
                 cell_subjects = num_subjects,
                 cell_predictions = average_trait,
                 cell_labels = matrices)

  return(models)
}

process_binary <- function(affected_begin, unaffected_begin, matrices_begin, models) {
  modelnames <- strsplit(models[affected_begin-1], split = " ")

  num_models <- length(modelnames)

  num_affected <- list()
  num_unaffected <- list()
  prop_affected <- list()
  matrices <- list()

  for(i in 1:num_models) {

    num_rows <- unaffected_begin[i] - affected_begin[i] - 1

    num_affected[[i]] <- as.numeric(matrix(scan(text = models[(affected_begin[i]+1):(affected_begin[i]+num_rows)],
                                                quiet = TRUE),
                                           nrow = num_rows, byrow = TRUE))
    attr(num_affected[[i]], "num_rows") <- num_rows

    num_unaffected[[i]] <- as.numeric(matrix(scan(text = models[(unaffected_begin[i]+1):(unaffected_begin[i]+num_rows)],
                                                  quiet = TRUE),
                                             nrow = num_rows, byrow = TRUE))
    attr(num_unaffected[[i]], "num_rows") <- num_rows

    prop_affected[[i]] <- num_affected[[i]]/(num_affected[[i]]+num_unaffected[[i]])

    matrices[[i]] <- as.character(matrix(scan(text = models[(matrices_begin[i]+1):(matrices_begin[i]+num_rows)],
                                              what = "character",
                                              quiet = TRUE),
                                         nrow = num_rows, byrow = TRUE))
    attr(matrices[[i]], "num_rows") <- num_rows

  }

  models <- list(modelnames = modelnames,
                 cell_affected = num_affected,
                 cell_unaffected = num_unaffected,
                 cell_predictions = prop_affected,
                 cell_labels = matrices)

  return(models)
}
