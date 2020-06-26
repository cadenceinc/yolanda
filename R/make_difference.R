#' Make Data - Difference
#'
#' This will produce a dataframe with additional columns showing the
#' difference between each numeric column, subsequent to the first, and the
#' first numeric column.
#'
#' @param df dataframe
#' @param include_growth logical - Do you want the growth between columns
#'  along with the difference?
#'
#' @return dataframe
#' @export
#'
make_difference <- function(df, include_growth = FALSE) {
  nc <- numeric_cols(df)
  for (i in 2:length(nc)) {
    df[[paste0(nc[i], "_diff")]] <- calc_diff(df[[nc[1]]], df[[nc[i]]])
    if (include_growth) {
      df[[paste0(nc[i], "_growth")]] <- calc_growth(df[[nc[1]]], df[[nc[i]]])
    }
  }
  df
}
