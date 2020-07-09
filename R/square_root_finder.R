#' Iterative-Guessing Square Root Finder
#'
#' @description Using an offshoot of the Newton-Raphson root finding formula, this function finds square roots of numbers
#' within a certain tolerance level.
#'
#' @param x a number whose root needs finding
#' @param tol the distance we will accept from 0 for the function before we call the guess a root
#' @param iter_max the max number of iterations before the function stops
#' @param verbose whether you want each guess printed or not
#' @param initial_guess the starting point of root_finder's iterative process
#'
#' @return the root
#' @export
#'
#' @examples
#' root_finder(4, 6, 120, FALSE, x / 2)
square_root_finder <- function(x, tol = .00001, iter_max = 100, verbose = FALSE, initial_guess = x / (x*(x/2))){
  if (is.numeric(c(x,tol,iter_max,initial_guess)) == FALSE | is.logical(verbose) != TRUE | x < 0 | tol < 0 | iter_max < 0){
    stop("Incorrect arguments!")
  }
  ans <- "Sorry, this function could not find the root in under the given number of iterations."
  guess <- numeric(iter_max + 1)
  guess[1] <- initial_guess
  for (i in 1:iter_max){
    guess[i + 1] <- (guess[i] + (x / guess[i])) / 2
    if (abs((guess[i+1]^2) - x) <= tol){
      if (verbose == TRUE){
        ans <- guess[1:i+1]
      }
      else {
        ans <- guess[i + 1]
      }
      break
    }
  }
  ans
}
