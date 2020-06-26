#' Numeric Columns
#'
#' @param df dataframe
#'
#' @return a character vector of columns that correspond to all numeric columns
#' @export
#'
numeric_cols <- function(df) { names(df)[unlist(lapply(df, is.numeric))] }

#' Non-Numeric Columns
#'
#' @param df dataframe
#'
#' @return a character vector of columns that correspond to all
#'  non-numeric columns
#' @export
#'
non_numeric_cols <- function(df) { names(df)[!unlist(lapply(df, is.numeric))] }

#' Date Columns
#'
#' @param df dataframe
#'
#' @return a character vector of columns that correspond to all date columns
#' @export
#'
date_cols <- function(df) {
  ret <- unlist(lapply(df, function(x) inherits(x, c("Date", "POSIXt"))))
  names(df)[ret]
}
