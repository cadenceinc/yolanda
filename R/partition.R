#' Partition
#'
#' This creates a recursive list by splitting a dataframe sequencially by
#' the provided columns.
#'
#' @param df data
#' @param cols character vector of columns to split the dataframe.
#' @param drop logical.  When splitting dataframes should the factors not in
#'  the dataset be dropped?  default = TRUE.
#'
#' @return recursive list
#' @export
#'
partition <- function(df, cols, drop = TRUE) {
  UseMethod("partition")
}

#' @export
partition.data.frame <- function(df, cols, drop = TRUE) {
  if (length(cols) == 1) {

    df <- split(df[names(df) != cols], df[cols], drop = drop)
  } else {

    use <- cols[1]
    rest <- cols[-1]
    df.list <- split(df[names(df) != use], df[use], drop = drop)
    df <- partition(df.list, rest)

  }
  attr(df, "partitions") <- cols
  df
}

#' @export
partition.list <- function(df, cols) {
  lapply(df, partition, cols)
}
