#' Multi-Method Greatest Common Denominator finder
#'
#' @description gcd calls either gcdi or gcdr, depending on argument c
#'
#' @param a a number
#' @param b another number
#' @param c either "recursive", "iterative", or any other quoted string
#'
#' @details In addition to regular error handling, if C isn't "recursive" or "iterative", the
#' function defaults to iterative and throws a warning message.
#'
#' @return the greatest common denominator according to the desired method
#' @export
#'
#' @examples
#' Put in two numbers, see what their greatest common denominator is
#' gcd(1,2, "recursive")
#' gcd(1,2, "anything else")
#' gcd(1,2, "iterative")
gcd <- function(a, b, c = "iterative") {
  if (a != as.numeric(a) | b != as.numeric(b) | as.character(c) == FALSE){
    stop("This only takes two numeric arguments and a character argument please")
  }
  if (c == "iterative") {
    return(gcdi(a, b))
  }
  else if (c == "recursive") {
    return(gcdr(a, b))
  }
  else {
    warning("That wasn't one of the intended choices, so we'll default it to iterative for now.")
    gcdi(a, b)
  }
}
