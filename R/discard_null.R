#' Discard Null
#'
#' This is a recursive function that removes all NULL values from a list.
#'
#' @param x list
#'
#' @return a list without NULL values
#' @export
#'
discard_null <- function(x) {
  x <- x[!unlist(lapply(x, is.null))]
  if(is.list(x)){ x <- lapply( x, discard_null) }
  x
}
