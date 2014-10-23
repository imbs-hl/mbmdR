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
  assertString(options$mt)
  assertString(options$o)
  assertString(options$a)
  assertString(options$d)
  assertString(options$v)
  assertString(options$pb)
  assert(checkCharacter(options$e), checkNull(options$e))
  assert(checkString(options$E), checkNull(options$E))
  assert(checkCharacter(options$filter), checkNull(options$filter))
  assert(checkString(options$filter.file), checkNull(options$filter.file))
  assertString(options$input.format)
  assertString(options$rt)

}
