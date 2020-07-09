#' Newton-Raphson optimization function!
#'
#' @description Using the Newton-Raphson optimization formula, this function finds the minimum of a given function,
#' using its derivatives.
#'
#' @param x an initial guess for the variable
#' @param f a function of one variable
#' @param var_name the name of the variable f uses
#' @param tol the minimum difference between two guesses for which we will accept a guess as correct
#' @param iter_max the maximum number of iterations before the function shuts down
#'
#' @return every guess made by the function
#' @export
#'
#' @examples
#' opt(3, f_x, .002, 140)
opt <- function(x = x/2, f = expr, var_name = "z", tol = .00001, iter_max = 100){
  if (is.numeric(c(x,tol,iter_max)) == FALSE | sum(c(x,tol,iter_max) > 0) < 1){
    stop("Incorrect arguments!")
  }
  ans <- "Sorry, this function could not find the root in under the given number of iterations."
  guess <- numeric(iter_max + 1)
  guess[1] <- x
  for (i in 1:iter_max){
    f_prime <- D(f, name=var_name)
    f_dbl_prime <- D(f_prime, name=var_name)
    z <- guess[i]
    guess[i + 1] <- guess[i] - (eval(f_prime) / eval(f_dbl_prime))
    z <- guess[i+1]
    if (abs(eval(f_prime)) <= tol){
      ans <- guess[1:i+1]
      break
    }
  }
  ans
}


