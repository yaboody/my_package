#' PQ Number Subtracter
#'
#' @description Subtracts two PQ numbers.
#'
#' @param x a pq number
#' @param y a pq number
#'
#' @details If the difference between the two is zero, you'll get a message telling you so. Otherwise, you'll get
#' the appropriate PQ number, courtesy of zero_maker, pq_operator, and zero_cutter.
#'
#' @return the difference between these two numbers with appropriate sign, p, q, and nums
#' @export
#'
#' @examples
#' as_pqnumber(3.2) - as_pqnumber(6.75)
#' as_pqnumber(32) - as_pqnumber(-61.75)
#' as_pqnumber(3/2) - as_pqnumber(6075)
`-.pqnumber` <- function(x,y){
  diff_p <- max(x$p, y$p)
  diff_q <-  max(x$q, y$q)
  prepped_diff_nums <- zero_maker(x,y)
  zeroed_diff_nums <- prepped_diff_nums[[1]] - prepped_diff_nums[[2]]
  if (sum(zeroed_diff_nums == 0) == length(zeroed_diff_nums)){
    return("That's just zero.")
  }
  zeroed_diff_nums[length(zeroed_diff_nums) + 1] <- 0
  sign_switch <- 0
  for (i in 1:(length(zeroed_diff_nums) - 1)){
    if (zeroed_diff_nums[length(zeroed_diff_nums) - i] != 0 & sign_switch == 0){
      diff_sign <- zeroed_diff_nums[length(zeroed_diff_nums) - i] / abs(zeroed_diff_nums[length(zeroed_diff_nums) - i])
      sign_switch <- 1
    }
  }
  messy_diff_nums <- pq_operator(zeroed_diff_nums, diff_sign)
  messy_diff <- structure(list(sign = diff_sign, p = diff_p, q = diff_q, nums = messy_diff_nums), class="pqnumber")
  tidy_diff <- zero_cutter(messy_diff)
  diff <- tidy_diff
  return(diff)
}
