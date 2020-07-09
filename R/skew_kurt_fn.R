#' Skew-Kurtosis Calculator
#'
#' @description Finds the skew of X and the kurtosis of X using the appropriate mathematical formulas.
#'
#' @param x a vector
#'
#' @details uses sample_central_fn as a helper function.
#'
#' @return The skew and kurtosis of x
#' @export
#'
#' @examples
#' skew_kurt_fn(c(3,4,6))
#' skew_kurt_fn(1:7)
#' skew_kurt_fn(c(3,3,3,3,3))
skew_kurt_fn <- function(x){
  if (is.numeric(x) == FALSE){
   stop("X isn't numeric!")
  }
  seed <- sample_central_fn(x)
  skew <- seed[[3]] / seed[[2]]^(3/2)
  kurt_num <- seed[[4]]
  kurt_denom <- seed[[2]]^2
  kurt <- (kurt_num / kurt_denom) - 3
  list(skew, kurt)
}
