#' PQ Number Zero Adder
#'
#' @description Adds zeroes in the right places to make adding or subtracting PQ numbers a smooth and easy process.
#'
#' @param x a pq number
#' @param y a pq number
#'
#' @details sort of messes with the p and q, so other functions have to help fix that issue.
#'
#' @return a list of two properly-zeroed num vectors
#' @export
#'
#' @examples
#' zero_maker(as_pqnumber(2.1), as_pqnumber(4/5))
#' zero_maker(as_pqnumber(23), as_pqnumber(.45))
#' zero_maker(as_pqnumber(0.21), as_pqnumber(-45))
zero_maker <- function(x,y){
  if (class(x) != "pqnumber" | class(y) != "pqnumber"){
    stop("Only PQ numbers please!")
  }
  xp <- x$p
  xq <- x$q
  xnum <- x$nums
  yp <- y$p
  yq <- y$q
  ynum <- y$nums
  num_holder <- numeric(0)
  if (xq != yq){
    if (xq > yq){
      num_holder[1:(xq-yq)] <- numeric(xq - yq)
      num_holder[(xq-yq+1):(length(ynum) + (xq - yq))] <- ynum
      ynum <- num_holder
    }
    else {
      num_holder[1:(yq-xq)] <- numeric(yq - xq)
      num_holder[(yq-xq+1):(length(xnum) + (yq - xq))] <- xnum
      xnum <- num_holder
    }
  }
  num_holder <- numeric(0)
  if (xp != yp){
    if (xp > yp){
      num_holder[1:length(ynum)] <- ynum
      num_holder[(length(ynum) + 1):(length(ynum) + (xp - yp))] <- numeric(xp - yp)
      ynum <- num_holder
    }
    else {
      num_holder[1:length(xnum)] <- xnum
      num_holder[(length(xnum)+ 1):(length(xnum) + (yp - xp))] <- numeric(yp - xp)
      xnum <- num_holder
    }
  }
  list((xnum), (ynum))
}
