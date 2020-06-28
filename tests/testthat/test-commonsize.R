context("building commonsize datasets")


# no-split ----------------------------------------------------------------


test_that("make_common works without split cols", {
  df <-
    storms %>%
    dplyr::select("name", "status", "pressure", "category") %>%
    dplyr::group_by_at(dplyr::vars("name", "status")) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::spread(., "status", "pressure", fill = 0)

  cmn <- make_common(df, 1)

  ## make sure row and column count are correct
  expect_identical(nrow(df), nrow(cmn))

  ## resulting dataframe names
  names_are <- numeric_cols(cmn)
  should_be <- sort(c(numeric_cols(df), paste0(numeric_cols(df), "_percent")))
  expect_identical(names_are, should_be)

  ## make sure there are no NAs
  common_cols <- grep("_percent", names(cmn))
  expect_identical(nrow(cmn[is.na(cmn), common_cols]), 0L)

  ## check values
  expect_identical(
    cmn$`tropical depression_percent`,
    round(df$`tropical depression` / df$`tropical depression`[1], 3)
  )
})


# split -------------------------------------------------------------------


test_that("make_common works with split cols", {
  df <-
    storms %>%
    dplyr::select("name", "status", "pressure", "category") %>%
    dplyr::group_by_at(dplyr::vars("name", "status")) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::spread(., "status", "pressure", fill = 0)

  cmn <- make_common(df, 1, split_cols = "category")

  ## make sure row and column count are correct
  expect_identical(nrow(df), nrow(cmn))

  ## resulting dataframe names
  names_are <- numeric_cols(cmn)
  should_be <- sort(c(numeric_cols(df), paste0(numeric_cols(df), "_percent")))
  expect_identical(names_are, should_be)

  ## make sure there are no NAs
  common_cols <- grep("_percent", names(cmn))
  expect_identical(nrow(cmn[is.na(cmn), common_cols]), 0L)

  ## check values
  expect_identical(
    cmn$`tropical depression_percent`,
    split(df, df$category, drop = TRUE) %>%
      lapply(function(x) {
        xx <- repl_na(round(x$`tropical depression`, 3))
        x$`tropical depression_percent` <- repl_na(round(xx / xx[1], 3))
        x
      }) %>%
      Reduce(rbind, .) %>%
      dplyr::pull("tropical depression_percent")
  )
})
