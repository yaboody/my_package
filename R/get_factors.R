#' Prime Factors Finder
#'
#' @description gets the non-unique (i.e. potentially repeating) prime factors of a number and returns them, sorted
#'
#' @param x a number whose prime factors we want to find
#'
#' @details This finds x's unique factors, checks to see if they're prime, and if they are divides x by them, then keeps
#' dividing x by them to see if that yields an integer, and if it does then that becomes part of the return. This process
#' continues until x is reduced down to 1, at which point the factors are returned. Also, can't handle
#' zero or negative numbers.
#'
#' @return the non-unique prime factors of a number, sorted from small to large
#' @export
#'
#' @examples
#' get_factors(7)
#' get_factors(8)
#' get_factors(-3)
get_factors <- function(x) {
  if (x != as.numeric(x)){
    stop("Argument should be an integer")
  }
  if (x <= 0){
    stop("Zero and negative numbers? Those are things this function can't, and shouldn't, handle.")
  }
  factor_index <- 1
  factor_vector <- numeric(2)
  length_index <- 1
  length_vector <- numeric(0)
  prime_index <- 1
  prime_vector <- numeric(0)
  x_reduced <- x
  for (i in 2:x) {
    if (x %% i == 0) {
      factor_vector[factor_index] <- i
      factor_index <- factor_index + 1
    }
  }
  for (j in 1:length(factor_vector)) {
    prime <- ifelse(factor_vector[j] %% seq_len(x) == 0, 1, 0)
    if (sum(prime) == 2) {
      prime_vector[prime_index] <- factor_vector[j]
      prime_index <- prime_index + 1
    }
  }

  for (k in 1:length(prime_vector)){
    x_reduced <- x_reduced / prime_vector[k]
  }

  while (x_reduced != 1){
    for (l in 1:length(prime_vector)){
      x_reduced_more <- x_reduced / prime_vector[l]
      if (x_reduced_more  == as.integer(x_reduced_more)) {
        x_reduced <- x_reduced_more
        prime_vector[prime_index] <- prime_vector[l]
        prime_index <- prime_index + 1
      }
      if (x_reduced == 1){
        break
      }
    }
  }
  sorted_prime_vector <- sort(prime_vector)
  sorted_prime_vector
}
