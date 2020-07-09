#' PQ number constructor
#'
#' @description converts a number into a pq notation which can be used to reconstruct it - think scientific notation.
#'
#' @param sign 1 or -1
#' @param p length of pre-decimal part of number + 1
#' @param q length of post-decimal part of number
#' @param nums vector of every digit of the number
#'
#' @details PQ numbers let you get around R's inherent bit limitations and perform operations on numbers too big or small
#' for R to normally allow you to deal with. Basically a type of scientific notation; numbers in nums are multiplied by 10^x, x
#' being determined by their pq position, then summed, returning the original number (multiplied by sign).
#'
#' @return a list of sign, p, q, nums with class pqnumber
#' @export
#'
#' @examples
#' pq_const(1,3,4,3:2)
#' pq_const(2,4,5,c(3,2,4))
pq_const <- function(sign = 1, p = 5, q = 1, nums = 1:7){
  if (p == as.integer(p) & q == as.integer(q) & sum(nums == as.integer(nums)) == length(nums) & (sign == 1 | sign == -1) & length(nums) == (p + q + 1)){
    p_q <- structure(list(sign = sign, p = p, q = q, nums = nums), class="pqnumber")
  }
  else {
    stop("That doesn't fit the description of a PQ number, cut it out.")
  }
  p_q
}
