#' Default Numeric Object Coercer
#'
#' @description by default, as.numeric works fine for coercing objects into numeric objects.
#'
#' @param x an object
#'
#' @details just a wrapper for as.numeric really
#'
#' @return x as a numeric object
#' @export
#'
#' @examples
#' as_numeric("4")
#' as_numeric(4)
#' as_numeric(TRUE)
as_numeric.default <- function(x) {
  print(as.numeric(x))
}
