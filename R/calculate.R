#' Calculate Difference
#'
#' This function calculates the difference between two values while ensuring
#' there are no adverse results such as NA, NAN, or Inf.
#'
#' @param x numeric value, or vector
#' @param y numeric value, or vector
#'
#' @return numeric value, or vector
#' @export
#'
calc_diff <- function(x, y) {
  x <- repl_na(x)
  y <- repl_na(y)
  repl_na(x - y)
}

#' Calculate Growth
#'
#' This function calculates the growth of one value over another while ensuring
#' there are no adverse results such as NA, NAN, or Inf.  It also attempts to
#' avoid the issue of returning infinitely small, non-zero, values by rounding
#' each value to two decimals first.
#'
#' @param x numeric value, or vector
#' @param y numeric value, or vector
#'
#' @return numeric value, or vector
#' @export
#'
calc_growth <- function(x, y) {
  x <- round(repl_na(x), 2)
  y <- round(repl_na(y), 2)
  repl_na((x - y) / abs(y))
}

#' Calculate Margin
#'
#' This function calculates the margin, or relative size, of a value compared to
#' another while ensuring there are no adverse results such as NA, NAN, or Inf.
#' It also attempts to avoid the issue of returning infinitely small, non-zero,
#' values by rounding each value to two decimals first.
#'
#' @param x numeric value, or vector
#' @param y numeric value, or vector
#'
#' @return numeric value, or vector
#' @export
#'
calc_margin <- function(x, y) {
  x <- round(repl_na(x), 2)
  y <- round(repl_na(y), 2)
  repl_na(x / y)
}
