#' Recursive Greatest Common Denominator Finder
#'
#' @description gcdr continually calls itself with the minimum of the two arguments given and the difference between them,
#' until the difference between them is negative, at which point the minimum should be the GCD of the original two numbers.
#'
#' @param a a number
#' @param b another number
#'
#' @details If one of the numbers is zero, you get zero early. And if one of the arguments isn't a number, you get an error.
#' Since this is recursive, it can run pretty long. Use gcdi instead.
#'
#' @return their greatest common denominator
#' @export
#'
#' @examples
#' gcdr(1,2)
#' gcdr(0,1)
#' gcdr(20,200)
gcdr <- function(a, b) {
  if (a != as.numeric(a) | b != as.numeric(b)){
    stop("This only takes two numeric arguments please")
  }
  if (min(a,b) == 0){
    return(0)
  }
  else {
    if ((max(a,b) - min(a,b)) <= 0) {
      return(min(a,b))
    }
  }
  gcdr(min(a,b), max(a,b) - min(a,b))
}
