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
  registries <- grep("registries", list.dirs(path = work.dir, recursive = FALSE), value = TRUE)

  message("Found the following files and directories from previous MB-MDR analyses:")
  message(paste(oldstuff <- c(logs, models, files, results, registries), collapse = "\n"))

  answer <- readline("Delete [a]ll/[n]one/[s]ingle? ")

  deleteSingle <- function(os) {
    sapply(os, function(x) {
      ans <- readline(paste("Delete", x, "? [y]es/[n]o/[c]ancel: "))
      switch (ans,
              y = invisible(unlink(x, recursive = TRUE, force = TRUE)),
              n = return(),
              c = stop("Cancelled!")
      )
    })
  }

  switch (answer,
    a = invisible(unlink(oldstuff, recursive = TRUE, force = TRUE)),
    n = stop("Nothing done!"),
    s = deleteSingle(oldstuff)
  )

}
