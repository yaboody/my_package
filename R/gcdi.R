#' Iterative Greatest Common Denominator Finder.
#'
#' @description gcdi divides two numbers by every number until the smaller of the two,
#' then takes the larger of the two that divides cleanly into both as the greatest common denominator.
#'
#' @param a a number
#' @param b another number
#'
#' @details If one of the numbers is zero, you get zero early. And if one of the arguments isn't a number, you get an error.
#'
#' @return their greatest common denominator
#' @export
#'
#' @examples
#' gcdi(10,20)
#' gcdi(0,15)
#' gcdi(2,2)
gcdi <- function(a, b) {
  if (a != as.numeric(a) | b != as.numeric(b)){
    stop("This only takes two numeric arguments please")
  }
  gcd <- 0
  if (min(a,b) == 0){
    gcd <- 0
  }
  else {
    for (i in seq_len(min(a, b))) {
      if (a %% i == 0 & b %% i == 0) {
        gcd <- i
      }
    }
  }
  gcd
}
