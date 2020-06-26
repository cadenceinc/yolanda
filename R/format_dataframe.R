#' Format Dataframe Values (Row and Column)
#'
#' @param df dataframe
#' @param cols numeric
#' @param rows numeric
#'
#' @return dataframe
#' @export
#'
fmt_value_by_row_and_col <- function(df, rows, cols = numeric_cols(df)) {
  if (is.character(cols)) {cols <- which(names(df) %in% cols)}
  nc <- numeric_cols(df)
  all_numeric <- all(names(df)[cols] %in% nc)
  if (isFALSE(all_numeric)) stop("The chosen columns are not numeric.")
  df[rows, cols] <- round(df[rows, cols] / 1000)
  df
}

#' Format Dataframe Values (Row)
#'
#' @param df dataframe
#' @param rows numeric
#'
#' @return dataframe
#' @export
#'
fmt_value_by_row <- function(df, rows) {
  nc <- numeric_cols(df)
  df[rows, nc] <- round(df[rows, nc] / 1000)
  df
}

#' Format Dataframe Values (Column)
#'
#' @param df dataframe
#' @param cols numeric
#'
#' @return dataframe
#' @export
#'
fmt_value_by_col <- function(df, cols = numeric_cols(df)) {
  if (is.character(cols)) {cols <- which(names(df) %in% cols)}
  nc <- numeric_cols(df)
  all_numeric <- all(names(df)[cols] %in% nc)
  if (isFALSE(all_numeric)) stop("The chosen columns are not numeric.")
  df[, cols] <- round(df[, cols] / 1000)
  df
}

#' Format Dataframe Percent (Row and Column)
#'
#' @param df dataframe
#' @param cols numeric
#' @param rows numeric
#'
#' @return dataframe
#' @export
#'
#'
fmt_percent_by_row_and_col <- function(df, rows, cols = numeric_cols(df)) {
  if (is.character(cols)) {cols <- which(names(df) %in% cols)}
  nc <- numeric_cols(df)
  all_numeric <- all(names(df)[cols] %in% nc)
  if (isFALSE(all_numeric)) stop("The chosen columns are not numeric.")
  df[rows, cols] <- round(df[rows, cols] * 100, 2)
  df
}

#' Format Dataframe Percent (Row)
#'
#' @param df dataframe
#' @param rows numeric
#'
#' @return dataframe
#' @export
#'
#'
fmt_percent_by_row <- function(df, rows) {
  nc <- numeric_cols(df)
  df[rows, nc] <- round(df[rows, nc] * 100, 2)
  df
}

#' Format Dataframe Percent (Column)
#'
#' @param df dataframe
#' @param cols numeric
#'
#' @return dataframe
#' @export
#'
#'
fmt_percent_by_col <- function(df, cols = numeric_cols(df)) {
  if (is.character(cols)) {cols <- which(names(df) %in% cols)}
  nc <- numeric_cols(df)
  all_numeric <- all(names(df)[cols] %in% nc)
  if (isFALSE(all_numeric)) stop("The chosen columns are not numeric.")
  df[, cols] <- round(df[, cols] * 100, 2)
  df
}
