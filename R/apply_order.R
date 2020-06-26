#' Apply Column Order
#'
#' @param df dataframe
#' @param col_order character vector
#'
#' @return dataframe ordered by column
#' @export
#'
apply_col_order <- function(df, col_order = sort(names(df))) {

  existing_nnc  <- non_numeric_cols(df)
  existing_nc   <- numeric_cols(df)
  ordered_nnc   <- col_order[col_order %in% existing_nnc]
  ordered_nc    <- col_order[col_order %in% existing_nc]
  unordered_nnc <- sort(existing_nnc[!existing_nnc %in% ordered_nnc])
  unordered_nc  <- sort(existing_nc[!existing_nc %in% ordered_nc])

  ## output ordered dataframe
  df[, c(ordered_nnc, unordered_nnc, ordered_nc, unordered_nc)]

}

#' Apply Row Order
#'
#' @param df dataframe
#' @param cols character vector
#' @param reverse_cols character vector or NULL (default)
#'
#' @return dataframe
#' @export
#'
apply_row_order <- function(df, cols = names(df), reverse_cols = NULL) {
  cols <- lapply(cols, function(x) {
    if (x %in% reverse_cols) {
      x <- dplyr::desc(df[[x]])
    } else {
      x <- df[[x]]
    }
    x
  })
  dplyr::arrange(df, !!!cols)
}
