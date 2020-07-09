#' PQ number converter
#'
#' @description converts regular numbers to PQ numbers
#'
#' @param x a number
#'
#' @details PQ numbers let you get around R's inherent bit limitations and perform operations on numbers too big or small
#' for R to normally allow you to deal with. Basically a type of scientific notation; numbers in nums are multiplied by 10^x, x
#' being determined by their pq position, then summed, returning the original number (multiplied by sign).
#'
#' @return x as a pqnumber with appropriate sign, p, q, and nums
#' @export
#'
#' @examples
#' as_pqnumber(344.4)
#' as_pqnumber(30)
#' as_pqnumber(0.30)
as_pqnumber <- function(x = 3465){
  if (is.numeric(x) == FALSE | length(x) > 1){
    stop("X should be a single number, so stop that")
  }
  if (as.integer(x) == x){
    nums <- str_extract_all(x, pattern = "\\w")
    p <- length(nums) - 1
    q <- 0
  }
  else {
    x_true <- character(2)
    x_char <- as.character(x)
    x_true <- strsplit(x_char, split="\\.")
    nums <- str_extract_all(x_char, pattern = "\\w")
    p_holder <- str_extract_all(x_true[[1]][1], pattern = "\\w")
    q_holder <- str_extract_all(x_true[[1]][2], pattern = "\\w")
    p <- length(p_holder[[1]]) - 1
    q <- length(q_holder[[1]])
  }
  if (x > 0){
    sign <- 1
  }
  else {
    sign <- -1
  }
  nums[[1]][1:length(nums[[1]])] <- nums[[1]][length(nums[[1]]):1]
  as_pq_x <- structure(list(sign = sign, p = p , q = q, nums = as.numeric(nums[[1]])), class="pqnumber")
  as_pq_x
}
