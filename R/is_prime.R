#' Prime Determinator
#'
#' @description by calling the get_factors function, we can see how many prime factors a number has; prime numbers have
#' two factors and only one of them is prime, so if a number has one prime factor it's prime, and if not it isn't
#'
#' @param x a number
#'
#' @return TRUE for prime numbers, FALSE for non-prime numbers
#'
#' @details just uses get_factors for most of the heavy lifting.
#'
#' @export
#'
#' @examples
#' is_prime(31)
#' is_prime(30)
#' is_prime(0)
is_prime <- function(x) {
  if (x != as.numeric(x) | is.character(x) == TRUE){
    stop("This function only takes numeric arguments.")
  }
  if (length(get_factors(x)) == 1) {
    return(TRUE)
  }
  else {
    FALSE
  }
}
