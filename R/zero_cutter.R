#' PQ Number Zero Cutter
#'
#' @description Cuts zeroes from PQ numbers where they are not needed, updates P and Q accordingly.
#' @param x a pq number
#'
#' @details due to zero_maker, zero_mult_maker, and so on, nums vectors may have meaningless zeroes, and p and q
#' need updating in any case. This function handles all that.
#'
#' @return a trimmed and corrected pq number
#' @export
#'
#' @examples
#' zero_cutter(as_pqnumber(3.4) + as_pqnumber(3.45))
#' zero_cutter(as_pqnumber(3.4) - as_pqnumber(3.45))
#' zero_cutter(as_pqnumber(-3.4) - as_pqnumber(3.4544))
zero_cutter <- function(x = w, mult=FALSE) {
  diff_index <- 0
  mult_index <- as.numeric(mult)
  if (x$nums[1] != 0){
    diff_index <- 1
  }
  num_true <- numeric(0)
  for (i in 1:length(x$nums)) {
    num_true <- paste0(num_true, x$nums[i])
  }
  num_true <- str_remove_all(num_true, pattern = "^[0]*")
  str_holder <- str_extract_all(num_true, pattern = "\\w")
  diff_length <- length(str_extract_all(x$nums, pattern = "\\w")) - length(str_extract_all(str_holder[[1]], pattern = "\\w"))
  if (diff_length > 0){
    x$q <- x$q - diff_length + diff_index + mult_index
  }
  num_true <- str_remove_all(num_true, pattern = "[0]*$")
  str_holder <- str_extract_all(num_true, pattern = "\\w")
  if (length(str_extract_all(x$nums, pattern = "\\w")) > (length(str_extract_all(str_holder[[1]], pattern = "\\w")) + diff_length)){
    x$p <- x$p + 1 - (length(str_extract_all(x$nums, pattern = "\\w")) - (length(str_extract_all(str_holder[[1]], pattern = "\\w")) + diff_length))
  }
  num_true <- str_extract_all(num_true, pattern = "\\w")
  num_true <- as.numeric(num_true[[1]])
  true_sum <- structure(list(sign = x$sign, p = x$p, q = x$q, nums = num_true))
  true_sum
}
