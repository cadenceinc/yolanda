#' Shear
#'
#' This allows for filtering dataframes and then eliminating the filtered
#' column.  It's a quick way to simplify data.  It's also a good way to
#' simplify a range of datasets that may not have similar features because if
#' the columns to be filtered do no exist in the dataframe the original
#' dataframe is returned.
#'
#' @param df dataframe to be filtered
#' @param ... a named set of values where the names are the columns to be
#'  filtered and the values are the filtering criteria.
#'
#' @return dataframe
#' @export
#'
shear <- function(df, ...) {
  xx <- list(...)
  if ("" %in% names(xx)) stop("each element of `...` must be named.")

  for (i in 1:length(xx)) {
    col   <- names(xx)[i]
    value <- xx[[i]]
    if (!col %in% names(df)) return(df)
    nms <- names(df)[!names(df) %in% col]
    df  <- df[df[[col]] %in% value, ]
    df  <- df[, nms]
  }

  df
}
