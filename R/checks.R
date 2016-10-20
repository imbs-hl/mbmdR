#' @title Check options
#'
#' @description
#' Check options of mbmdR for correct type.
#'
#' @param options [\code{list}]\cr
#'   List of MB-MDR options.
#'
#' @return
#' Throws an error if any check fails and invisibly returns TRUE otherwise.
check.options <- function(options = getOption("mbmdr")) {

  assertions <- checkmate::makeAssertCollection()

  checkmate::assertInt(options$n,
                       add = assertions)
  checkmate::assertInt(options$p,
                       add = assertions)
  checkmate::assert(checkInt(options$r), checkNull(options$r),
                    add = assertions)
  checkmate::assertInt(options$m,
                       add = assertions)
  checkmate::assertInt(options$at,
                       add = assertions)
  checkmate::assertInt(options$ct,
                       add = assertions)
  checkmate::assertInt(options$ac,
                       add = assertions)
  checkmate::assertNumber(options$x,
                          add = assertions)
  checkmate::assertChoice(options$mt,
                          choices = c("NONE", "MAXT", "MINP", "RAWP", "STRAT1",
                                      "STRAT2", "gammaMAXT"),
                          add = assertions)
  checkmate::assertChoice(options$a, c("CODOMINANT", "ADDITIVE", "NONE"),
                          add = assertions)
  checkmate::assertChoice(options$rc, c("RESIDUALS", "ONTHEFLY"),
                          add = assertions)
  checkmate::assertChoice(options$d, c("1D", "2D", "3D"),
                          add = assertions)
  if(options$d == "1D" & options$mt == "gammaMAXT") {
    warning("Requested 1D analysis with 'gamamMAXT' correction algorithm. This is not possible. Falling back to 'MAXT' correction algorithm.")
    options$mt <- "MAXT"
  }
  if(options$a != "NONE" & options$d == "1D") {
    warning("Requested 1D analysis with '", options$a, "' adjustment. This is not possible. Setting adjustment to 'NONE'.")
    options$a <- "NONE"
  }
  checkmate::assertChoice(options$v, c("SHORT", "MEDIUM", "LONG"),
                          add = assertions)
  checkmate::assertChoice(options$pb, c("NONE", "NORMAL"),
                          add = assertions)
  checkmate::assert(checkmate::checkCharacter(options$e),
                    checkmate::checkNull(options$e),
                    add = assertions)
  checkmate::assert(checkmate::checkFile(options$E),
                    checkmate::checkNull(options$E),
                    add = assertions)
  checkmate::assert(checkmate::checkCharacter(options$filter),
                    checkmate::checkNull(options$filter),
                    add = assertions)
  checkmate::assert(checkmate::checkFile(options$filter.file),
                    checkmate::checkNull(options$filter.file),
                    add = assertions)
  checkmate::assert(checkmate::checkCharacter(options$k),
                    checkmate::checkNull(options$k),
                    add = assertions)
  checkmate::assert(checkmate::checkFile(options$K),
                    checkmate::checkNull(options$K),
                    add = assertions)
  checkmate::assert(checkmate::checkFile(options$s),
                    checkmate::checkNull(options$s),
                    add = assertions)
  checkmate::assertChoice(options$input.format,
                          choices = c("MDR", "MBMDR"),
                          add = assertions)
  checkmate::assertChoice(options$rt,
                          choices = c("NONE", "RANK_TRANSFORM"),
                          add = assertions)

  checkmate::reportAssertions(assertions)

  options(mbmdr = options)

  invisible(TRUE)

}
