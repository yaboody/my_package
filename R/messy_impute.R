#' Messy Imputation Function
#'
#' @description If you've got a messy data frame or tibble with NA values, this function can replace them with the mean
#' or median of their column or row.
#'
#' @param frame a data frame or tibble (hopefully with NAs)
#' @param center mean or median, a measure of centrality
#' @param margin row or column, where centrality should be measured
#'
#' @details If you've got NAs outside of the last column, this function may have issues.
#'
#' @return the frame with its NAs as means / medians of rows / columns
#' @export
#'
#' @examples
#' messy_impute(rand_grade_fn(15, diamonds), mean, 1)
#' messy_impute(rand_grade_fn(12, diamonds), median, 2)
#' messy_impute(rand_grade_fn(25, diamonds), mean, 2)
messy_impute <- function(frame, center, margin) {
  if ((margin != 1 & margin != 2) | (is.data.frame(frame) != TRUE | is_tibble(frame) != TRUE)){
    stop("frame needs to be a data frame or tibble, center should be mean or median, and margin should be 1 or 2")
  }
  frame <- frame[,-1]
  frame <- apply(frame, margin, FUN = function(x) {
    ifelse(is.na(x),
           center(x, na.rm = TRUE),
           x) })
  frame
}
