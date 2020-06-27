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
partition <- function(df, cols, drop = TRUE) {
  if (!is.data.frame(df)) stop("partition: df must be a dataframe")
  if (any(!cols %in% names(df))) {
    message(paste0("Columns: ", cols[!cols %in% names(df)], " ignored."))
  }
  cols <- cols[cols %in% names(df)]

  partitions <- lapply(cols, function(x) unique(df[[x]]))
  partitions <- stats::setNames(partitions, cols)

  partition_lengths <- unlist(lapply(partitions, length))
  partition_lengths <- stats::setNames(partition_lengths, cols)

  if (length(cols) == 1) {

    df <- split(df[names(df) != cols], df[cols], drop = drop)

  } else {

    use <- cols[1]
    rest <- cols[-1]
    df.list <- split(df[names(df) != use], df[use], drop = drop)
    df <- lapply(df.list, partition, rest)

  }

  attr(df, "partitioned_columns") <- cols
  attr(df, "partition_lengths")   <- partition_lengths
  attr(df, "partition_levels")    <- length(cols)
  attr(df, "partitions")          <- partitions
  df
}
