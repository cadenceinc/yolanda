#' Group and Summarize
#'
#' @param df data.frame
#' @param groups character
#' @param summary_cols character vector of columns to summarize.  This
#'  defaults to all numeric columns, but could be switched to other columns
#'  if the desire, for instance, is to concatenate all characters.
#' @param negate logical
#' @param .f function to be applied.
#'  Default is 'sum_na', or, sum(x, na.rm = TRUE).
#'
#' @return data.frame
#' @export
#'
group_and_summarize <- function(df,
                                groups       = non_numeric_cols(df),
                                summary_cols = numeric_cols(df),
                                negate       = FALSE,
                                .f           = sum_na) {
  stopifnot(is.character(groups))
  stopifnot(is.character(summary_cols))

  ## if negate is specified the groups need to be inversed
  ## this is accomplished by identifying all nonnumeric columns
  ## selecting those NOT in the group provided
  if (negate) {
    nnc    <- non_numeric_cols(df)
    groups <- nnc[!nnc %in% groups]
  }

  if (length(intersect(groups, summary_cols)) > 0) {
    stop("You cannot have groups and summary fields that overlap.")
  }

  df %>%
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarize_at(dplyr::vars(summary_cols), .f) %>%
    dplyr::ungroup()
}
