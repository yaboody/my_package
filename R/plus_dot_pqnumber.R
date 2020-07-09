#' PQ Number Adder
#'
#' @description Adds 2 PQ numbers.
#'
#' @param x a pq number
#' @param y a pq number
#'
#' @detatils uses pq_operator, zero_maker, and zero_cutter
#'
#' @return the sum of these two pq numbers, with appropriate p, q, sign, and nums
#' @export
#'
#' @examples
#' as_pqnumber(3.4) + as_pqnumber(3.45)
#' as_pqnumber(3.2) + as_pqnumber(3/2)
`+.pqnumber` <- function(x,y){
  sum_p <- max(x$p, y$p)
  sum_q <-  max(x$q, y$q)
  prepped_sum_nums <- zero_maker(x,y)
  zeroed_sum_nums <- prepped_sum_nums[[1]] + prepped_sum_nums[[2]]
  zeroed_sum_nums[length(zeroed_sum_nums) + 1] <- 0
  sum_sign <- zeroed_sum_nums[length(zeroed_sum_nums) - 1] / abs(zeroed_sum_nums[length(zeroed_sum_nums) - 1])
  messy_sum_nums <- pq_operator(zeroed_sum_nums, sum_sign)
  messy_sum <- structure(list(sign = sum_sign, p = sum_p, q = sum_q, nums = messy_sum_nums), class="pqnumber")
  tidy_sum <- zero_cutter(messy_sum)
  sum <- tidy_sum
  sum
}


