#' Check if Object is PQ Number
#'
#' @description returns TRUE for pq numbers and FALSE for non-pq numbers.
#'
#' @param x a number
#'
#' @return TRUE if x is a pq number, FALSE if not
#' @export
#'
#' @examples
#' is_pqnumber(as_pqnumber(3.45))
#' is_pqnumber(as_pqnumber(3))
#' is_pqnumber(as_pqnumber(0.45))
is_pqnumber <- function(x){
  if (is.list(x) == TRUE & length(x) == 4){
    if ((sum(class(x) == "pqnumber") == 1 & (x[[1]] == 1 | x[[1]] == -1) & length(x[[2]]) == 1 & length(x[[3]]) == 1 & length(x[[4]]) == x[[2]] + x[[3]] + 1)){
      return(TRUE)
    }
  }
  else {
    return(FALSE)
  }
}
