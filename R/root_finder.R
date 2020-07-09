#' Iterative-Guessing Root Finder
#'
#' @description Using a general offshoot of the Newton-Raphson root-findng formula, this function finds the general positive
#' root of just about any number, including negative ones (for odd roots).
#'
#' @param x a number whose root needs finding
#' @param tol the distance from zero for our function we will accept as "equivalency"
#' @param iter_max the max number of iterations before the function stops
#' @param verbose whether you want each guess printed or not
#' @param initial_guess the starting point of root_finder's iterative process
#' @param root the degree of root (square, cube, etc.)
#'
#' @return the root
#' @export
#'
#' @examples
#' root_finder(4, 6, 120, FALSE, x / 2, 3)
root_finder <- function(x, tol = .00001, iter_max = 100, verbose = FALSE, initial_guess = x / (x*(x/2)), root = 2){
  if (is.numeric(c(x,tol,iter_max,initial_guess, root)) == FALSE | is.logical(verbose) != TRUE | root < 0 | tol < 0 | iter_max < 0 | (x < 0 & root %% 2 == 0)){
    stop("Incorrect arguments!")
  }
  ans <- "Sorry, this function could not find the root in under the given number of iterations."
  guess <- numeric(iter_max + 1)
  guess[1] <- initial_guess
  for (i in 1:iter_max){
    guess[i + 1] <- guess[i] - (guess[i] / root) + (x / (root*(guess[i]^(root-1))))
    if (abs((guess[i+1]^root) - x) <= tol){
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

