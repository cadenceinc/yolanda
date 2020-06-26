#' Make Data - Commonsize
#'
#' This is a recursive function.
#' If the split columns are present the function will use them to split the
#' the dataframe into a list of dataframes, then cycle through each element
#' and apply the function.
#'
#' @param df dataframe
#' @param pos numeric
#' @param split_cols character vector specifying the columns to split by.
#'  defaults to NULL, which results in no split.
#'
#' @return dataframe
#' @export
#'
make_common <- function(df, pos, split_cols = NULL) {
  if (is.null(split_cols)) {

    ## if there are incredibly small non-zero floats there is the possibility
    ## of creating impossible long digits.  So we round to 3 digits before
    ## making the common size statement
    df <- dplyr::mutate_if(df, is.numeric, ~ round(., 3))
    nnc    <- non_numeric_cols(df)
    nc     <- numeric_cols(df)
    common <- lapply(df[, nc], function(x) {calc_margin(x, x[pos])})
    common <- data.frame(common, stringsAsFactors = FALSE)
    common <- stats::setNames(common, paste0(nc, "_percent"))
    common <- dplyr::mutate_if(common, is.numeric, ~ round(. * 100, 2))
    df <- cbind(df, common)
    df <- df[, c(nnc, as.vector(rbind(nc, paste0(nc, "_percent"))))]

  } else {

    df <- split(df, lapply(split_cols, function(x) df[[x]]))
    df <- lapply(df, make_common, pos = pos)
    df <- Reduce(rbind, df)

  }
  df
}
