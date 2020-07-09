#' Tidy Imputation Function
#'
#' @description If you've got a tidy data frame or tibble with NA values, this function can replace them with the mean
#' or median of their column or row.
#'
#' @param frame a data frame or tibble
#' @param center mean or median
#' @param margin 1 or 2 (student name or type of assignment)
#' @param impute_cols The name of the columns to be imputed
#'
#' @details Since this function was meant to be used on gradebooks specifically, it pretty much only works on specifically
#' named columns and observations. This can't be helped; if you want to use it more generally, type tidy_impute(),
#' which prints the code, and then copy-paste some stuff around.
#'
#' @return the frame with its NAs as means or medians of what would be considered the columns or rows of those
#' observations in a messy data frame or tibble
#' @export
#'
#' @examples
#' tidy_impute(frame_name, mean, 1)
#' tidy_impute(frame_name, median, 2)
tidy_impute <- function(frame, center, margin, impute_cols){
  if ((margin != 1 & margin != 2) | (is.data.frame(frame) != TRUE | is_tibble(frame) != TRUE)){
    stop("frame needs to be a data frame or tibble, center should be mean or median, and margin should be 1 or 2")
  }
  ungroup(mutate_at(group_by_at(frame, margin), .vars = impute_cols, .funs = function(y) { ifelse(is.na(y), center(y, na.rm=TRUE), y) }))
}
