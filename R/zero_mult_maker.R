#' PQ Number Multiplication Zero-Adder
#'
#' @description adds zeroes to PQ numbers to make multiplying them smoother
#'
#' @param x a pq num vector
#' @param y the number of zeroes to be added
#' @param side whether the zeroes should be added at the start or end
#'
#' @details a helper function for *.pqnumber, which adds the needed number of zeroes at the end or beginning
#'  of nums.
#'
#' @return a pq num vector with the right number of zeroes for the task given
#' @export
#'
#' @examples
#' zero_mult_maker(as_pqnumber(3.4)$nums, 3, "not start")
#' zero_mult_maker(as_pqnumber(-3.4)$nums, 2, "start")
#' zero_mult_maker(as_pqnumber(0.4)$nums, 1, "not start")
zero_mult_maker <- function(x,y, side="start"){
  if (side == "start"){
    x <- c(numeric(y), x)
  }
  else {
    x <- c(x, numeric(y))
  }
  x
}
