#' Numeric Coercion Function
#'
#' @description a generic function to coerce different classes of objects into numbers, like as.numeric but generic
#'
#' @param x an object
#'
#' @details This is just a wrapper function for as.numeric and as_numeric.pqnumber.
#'
#' @return the result of the UseMethod function, which depends on the object class.
#' @export
#'
#' @examples
#' as_numeric("3")
#' as_numeric(as_pqnumber(3))
#' as_numeric(0.5)
as_numeric <- function(x) {
  UseMethod("as_numeric")
}


