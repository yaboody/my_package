#' Central Moment Finder
#'
#' @description Finds the first four central moments of X, meaning (x - mean of x) to the power of (1,2,3,4), all divided
#' by the number of observations.
#'
#' @param x a vector
#'
#' @return the four first central moments of vector x, in a list
#' @export
#'
#' @examples
#' sample_fn(c(3,2,6))
#' sample_fn(c(3:7))
#' sample_fn(c(3,3,3))
sample_central_fn <- function(x){
  if (is.numeric(x) != TRUE) {
    stop("X isn't numeric!")
  }
  n <- length(x)
  xbar <- sum(x) / n
  mo_1 <- 0
  mo_2 <- 0
  mo_3 <- 0
  mo_4 <- 0
  for (i in 1:n){
    mo_1 <- mo_1 + (((x[i] - xbar)^1)/n)
  }
  for (j in 1:n){
    mo_2 <- mo_2 + (((x[j] - xbar)^2)/n)
  }
  for (k in 1:n){
    mo_3 <- mo_3 + (((x[k] - xbar)^3)/n)
  }
  for (l in 1:n){
    mo_4 <- mo_4 + (((x[l] - xbar)^4)/n)
  }
  list(mo_1, mo_2, mo_3, mo_4)
}

