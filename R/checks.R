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

  assertInt(options$n)
  assertInt(options$p)
  assert(checkInt(options$r), checkNull(options$r))
  assertInt(options$m)
  assertNumber(options$x)
  assertChoice(options$mt, c("NONE", "MAXT", "MINP", "RAWP", "STRAT1", "STRAT2", "gammaMAXT"))
  assertString(options$o)
  assertChoice(options$a, c("CODOMINANT", "ADDITIVE", "NONE"))
  assertChoice(options$d, c("1D", "2D", "3D"))
  assertChoice(options$v, c("NONE", "SHORT", "MEDIUM", "LONG"))
  assertChoice(options$pb, c("NONE", "NORMAL"))
  assert(checkCharacter(options$e), checkNull(options$e))
  assert(checkFile(options$E), checkNull(options$E))
  assert(checkCharacter(options$filter), checkNull(options$filter))
  assert(checkFile(options$filter.file), checkNull(options$filter.file))
  assertChoice(options$input.format, c("MDR", "MBMDR"))
  assertChoice(options$rt, c("NONE", "RANK_TRANSFORM"))

}
