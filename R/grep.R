#' Grep Zero
#'
#' A wrapper function for 'grep' that ignores case and returns values (default).
#'
#' @param pattern character
#' @param x character
#' @param value logical
#' @param invert logical
#'
#' @return character
#' @export
#'
grep_0 <- function(pattern, x, value = TRUE, invert = FALSE) {
  grep(pattern, x, ignore.case = TRUE, value = value, invert = invert)
}

#' Grep Zero Vectorized
#'
#' @param pattern character
#' @param x character
#'
#' @return vector
#' @export
#'
grep_v <- function(pattern, x) {
  unlist(lapply(pattern, function(p) grep_0(p, x)))
}
