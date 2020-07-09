#' Sample Moment Finder
#'
#' @description finds the first four sample moments of a given argument, meaning x to the power of (1,2,3,4), divided by
#' the number of observations
#'
#' @param x a vector
#'
#' @return the four first moments of x
#' @export
#'
#' @examples
#' sample_fn(c(3,2,6))
#' sample_fn(c(3:7))
#' sample_fn(c(3,3,3))
sample_fn <- function(x){
  if (is.numeric(x) != TRUE) {
   stop("X isn't numeric!")
  }
  n <- length(x)
  xbar <- sum(x) / n
  mo_1 <- 0
  mo_2 <- 0
  mo_3 <- 0
  mo_4 <- 0
  for (i in 1:length(x)){
    mo_1 <- mo_1 + ((x[i]^1)/n)
  }
  for (j in 1:length(x)){
    mo_2 <- mo_2 + ((x[j]^2)/n)
  }
  for (k in 1:length(x)){
    mo_3 <- mo_3 + ((x[k]^3)/n)
  }
  for (l in 1:length(x)){
    mo_4 <- mo_4 + ((x[l]^4)/n)
  }
  list(mo_1, mo_2, mo_3, mo_4)
}
