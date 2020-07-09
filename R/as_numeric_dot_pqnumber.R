#' Make PQ Number regular number
#'
#' @description Turns PQ numbers into the real number that they otherwise represent.
#'
#' @param x a pq number
#'
#' @details uses print.pqnumber to do most of the heavy lifting.
#'
#' @return x as a number
#' @export
#'
#' @examples
#' as_numeric(as_pqnumber(2.3))
#' as_numeric(as_pqnumber(23))
#' as_numeric(as_pqnumber(0.3))
as_numeric.pqnumber <- function(x){
  print(x)
}
