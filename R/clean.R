#' @title Delete old MB-MDR files
#'
#' @description
#' Searches for old MB-MDR analyses file in the given directory.
#'
#' @param work.dir [\code{string}]\cr
#'   The directory to look in.
#'
#' @return
#' Returns nothing.
#'
#' @export
clean <- function(work.dir = getwd()) {

  logs <- list.files(path = work.dir, pattern = "*.log", full.names = TRUE)
  models <- list.files(path = work.dir, pattern = "*.m_models.txt", full.names = TRUE)
  files <- list.files(path = work.dir, pattern = "*.mbmdr", full.names = TRUE)
  results <- list.files(path = work.dir, pattern = "*.result", full.names = TRUE)
  topfile <- list.files(path = work.dir, pattern = "*.topfile", full.names = TRUE)
  modelsfile <- list.files(path = work.dir, pattern = "*.models", full.names = TRUE)
  registries <- grep("registries", list.dirs(path = work.dir, recursive = FALSE), value = TRUE)
  topfiles <- grep("topfiles", list.dirs(path = work.dir, recursive = FALSE), value = TRUE)
  permutations <- grep("permutation", list.dirs(path = work.dir, recursive = FALSE), value = TRUE)

  oldstuff <- c(logs, models, files, results, topfile, modelsfile, registries, topfiles, permutations)

  if(length(oldstuff) > 0) {
    message("Found the following files and directories from previous MB-MDR analyses:")
    message(paste(oldstuff, collapse = "\n"))

    if(interactive()) {
      answer <- readline("Delete [a]ll/[n]one/[s]ingle? ")

      deleteSingle <- function(os) {
        sapply(os, function(x) {
          ans <- readline(paste("Delete", x, "? [y]es/[n]o/[c]ancel: "))
          switch (ans,
                  y = invisible(unlink(x, recursive = TRUE, force = TRUE)),
                  n = return(),
                  c = stop("Cancelled!"))
        })
      }
    } else {
      message("You are not in interactive mode. For safy reasons you have to delete the files manually!")
      answer <- "c"
    }

    switch (answer,
            a = invisible(unlink(oldstuff, recursive = TRUE, force = TRUE)),
            n = stop("Nothing done!"),
            s = deleteSingle(oldstuff),
            c = stop("Cancelled!"))
  } else {
    message("No files and directories from previous MB-MDR analyses found.")
  }

}
