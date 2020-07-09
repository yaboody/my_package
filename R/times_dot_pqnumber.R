#' PQ Number Multiplier
#'
#' @description Multiplies two PQ numbers and outputs a PQ number with the correct sign, P, Q, and nums.
#'
#' @param x a pq number
#' @param y a pq number
#'
#' @details uses zero_mult_maker, as well as pq_operator and zero_cutter
#'
#' @return the product of two numbers, with appropriate sign, p, q, and nums
#' @export
#'
#' @examples
#' as_pqnumber(3.4) * as_pqnumber(3.45)
#' as_pqnumber(-3.4) * as_pqnumber(3.45)
#' as_pqnumber(-3.4) * as_pqnumber(-3.45)
`*.pqnumber` <- function(x,y){
  mult_row_list <- list()
  mult_row_list_full <- list()
  mult_p <- x$p + y$p + 1
  mult_q <-  x$q + y$q + 1
  mult_sign <- x$sign * y$sign
  prepped_mult_nums <- zero_maker(x,y)
  prep_x <- prepped_mult_nums[[1]]
  prep_y <- prepped_mult_nums[[2]]
  max_x_y <- max(length(prep_x), max(prep_y))
  for (j in 1:length(prep_x)){
    mult_row_list[[j]] <- zero_mult_maker(prep_x[j] * prep_y, j-1, "start")
  }
  mult_max_length <- length(mult_row_list[[j]])
  for (k in 1:length(mult_row_list)){
    mult_row_list_full[[k]] <- zero_mult_maker(mult_row_list[[k]], (mult_max_length - length(mult_row_list[[k]]) + 1), "end")
  }
  mult_max_length <- mult_max_length + 1
  mult_sum <- numeric(mult_max_length)
  for (i in 1:length(mult_row_list_full)){
    mult_sum <- mult_sum + mult_row_list_full[[i]]
  }
  messy_mult_nums <- pq_operator(mult_sum, mult_sign)
  messy_mult <- structure(list(sign = mult_sign, p = mult_p, q = mult_q, nums = messy_mult_nums), class="pqnumber")
  tidy_mult <- zero_cutter(messy_mult, TRUE)
  mult <- tidy_mult
  mult
}
