#' Properly conform PQ numbers post-operation
#'
#' @description Ensures that PQ numbers post-operation have the correct digits by shifting over tens;
#' this also applies to subtraction, but with negative digits instead of digits over 10.
#'
#' @param x a pq number's num vector
#' @param y a pq number's sign
#'
#' @details This is a helper function used by -.pqnumber, +.pqnumber, and *.pqnumber. In fact, it's
#' basically what makes all of them work.
#'
#' @return a properly conformed PQ num vector which has only 0-9 digits
#' @export
#'
#' @examples
#' pq_operator(as_pqnumber(5.6)$nums, as_pqnumber(5.6)$sign))
#' pq_operator(as_pqnumber(-56)$nums, as_pqnumber(-56)$sign))
#' pq_operator(as_pqnumber(0.56)$nums, as_pqnumber(0.56)$sign))
pq_operator <- function(x, y){
  for (i in 1:length(x)){
    if (y > 0){
      x_div <- x[i] %/% (10)
      if (x_div != 0){
        x[i] <- x[i] - (10*x_div)
        x[i + 1] <- x[i+1] + x_div
      }
    }
    else {
      x_div <- x[i] %/% - 10
      if (x_div != 0) {
        x[i] <- x[i] + (10*x_div)
        x[i + 1] <- x[i+1] - x_div
      }
    }
  }
  x
}
