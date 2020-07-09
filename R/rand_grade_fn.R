#' Random NA inserter function
#'
#' @description This overwrites some number of values in a data frame or tibble with NAs.
#'
#' @param x number of NAs to insert
#' @param frame a frame, some of whose values are about to become NAs
#'
#' @details This function assumes there are no NAs in the data frame already, since it stops at a number of NAs, not
#' at a number of NAs inserted. Act accordingly.
#'
#' @return the same frame, with a few more NAs hopefully
#' @export
#'
#' @examples
#' rand_grade_fn(12, diamonds)
#' rand_grade_fn(10, diamonds)
#' rand_grade_fn(0, diamonds)
rand_grade_fn <- function(x, frame) {
  if ((is_tibble(frame) != TRUE & is.data.frame(frame) != TRUE) & x != as.numeric(x)) {
   stop("x should be numeric, and frame should be a tibble or data frame")
  }
  while(sum(is.na(frame)) < x) {
    rand_row <- sample(1:100, 1)
    frame[rand_row, ncol(frame)] <- NA
  }
  frame
}
