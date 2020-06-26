#' Add Counter
#'
#' This function identifies all NAs in a column and converts it to a character
#' with a counter, the default value being 'other_{counter}'.  The counter is
#' based on a reference column of choice, the default being the original
#' column.  It is not intended to use the original column as the reference but
#' if that's chosen all NA's will show '_1'.
#'
#' @param df dataframe
#' @param col character specifying the column selection
#' @param reference_col character showing the reference column.  The default is
#'  'col'
#' @param prefix character specifying the prefix that belongs to counter.  The
#'  default is 'other'
#'
#' @return dataframe with 'col' modified.
#' @export
#'
add_counter <- function(df, col, reference_col = col, prefix = "other") {
  fct <- factor(df[[reference_col]], exclude = NULL)
  fct <- paste(prefix, as.numeric(fct), sep = "_")
  df[[col]] <- ifelse(is.na(df[[col]]), fct, df[[col]])
  df
}

