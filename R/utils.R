
#' Wait for files on NFS mounts
#'
#' @param fns      [\code{character}]\cr
#'                 Vector of file paths to wait for.
#' @param timeout  [\code{number}]\cr
#'                 Time in seconds to wait for files to appear.#'
#'
#' @details Use \code{\link{list.files}} here as this seems to trick the nfs cache; see https://github.com/mllg/batchtools/issues/85.
#'
#' @return \code{TRUE} if all files are available. Otherwise throws an error
#'
waitForFiles = function(fns, timeout = NA_real_) {

  if (is.na(timeout))
    return(TRUE)

  fns <- fns[!file.exists(fns)]
  if (length(fns) == 0L) {
    return(TRUE)
  }

  paths <- dirname(fns)

  fns = setdiff(fns, list.files(paths, full.names = TRUE))
  if (length(fns) == 0L) {
    return(TRUE)
  }

  timeout = timeout + Sys.time()
  repeat {
    Sys.sleep(0.5)
    fns = setdiff(fns, list.files(paths, full.names = TRUE))
    if (length(fns) == 0L)
      return(TRUE)
    if (Sys.time() > timeout)
      stop(sprintf("Timeout while waiting for %i files, e.g. '%s'", length(fns), fns[1L]))
  }
}
