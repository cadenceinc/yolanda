#' Paste - sep ('new_line')
#'
#' @param ... elements passed to 'paste'
#'
#' @return A character vector of the concatenated values
#' @export
#'
paste_n <- function(...) {paste(..., sep = "\n")}

#' Paste - collapse ('new_line')
#'
#' @param ... elements passed to 'paste'
#'
#' @return A character vector of the concatenated values
#' @export
#'
paste0_n <- function(...) {paste0(..., collapse = "\n")}

#' Replace NA's
#'
#' This will replace all na, nan, inf values in a vector
#'
#' @param x vector, usually numeric.
#' @param replacement value, usually character or numeric, that is used as the
#'  replacement for all NA, NAN, or Inf values found.  default = 0.
#'
#' @return vector
#' @export
#'
repl_na <- function(x, replacement = 0) {
  ifelse(is.nan(x) | is.na(x) | is.infinite(x), replacement, x)
}

#' Sum - NA.RM = TRUE
#'
#' @param x numeric vector
#'
#' @return numeric value
#' @export
#'
sum_na <- function(x) {sum(x, na.rm = TRUE)}

#' Intersecting Columns
#'
#' @param df1 dataframe1
#' @param df2 dataframe2
#'
#' @return This returns a character vector listing the names of the intersecting
#'   columns of the two dataframes listed above.
#' @export
#'
inter_cols <- function(df1, df2) {
  intersect(names(df1), names(df2))
}

#' Evaluate Symbol
#'
#' This functions purpose is to eliminate problems with global variable bindings
#' by using characters instead of symbols.  This function takes the character,
#' converts it to a symbol and evaluates.
#'
#' @param x characters - usually representing columns in a dataframe
#'
#' @return NULL
#' @export
#'
eval_sym <- function(x) {
  eval.parent(as.symbol(x))
}

#' Capitalize First Letter
#'
#' @param x character or vector
#'
#' @return character or vector
#' @export
#'
cap_first <- function(x) {
  stopifnot(is.character(x))
  unlist(lapply(strsplit(x, split = " "), function(s) {
    if (is.na(s[1])) return(NA)
    paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ")
  }))
}
