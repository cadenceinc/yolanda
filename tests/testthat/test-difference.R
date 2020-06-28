context("building differences into datasets")


# without growth ----------------------------------------------------------


test_that("make_difference works without growth", {
  df <-
    storms %>%
    dplyr::select("name", "status", "wind", "pressure", "ts_diameter")

  diff <- make_difference(df)

  ## check names
  names_are <- names(diff)
  names_should_be <- c(
    non_numeric_cols(df),
    numeric_cols(df),
    paste0(numeric_cols(df)[-1], "_diff")
    )
  expect_identical(names_are, names_should_be)

  ## check values
  expect_identical(diff$pressure_diff, df$wind - df$pressure)
  expect_identical(
    diff$ts_diameter_diff,
    repl_na(df$wind) - repl_na(df$ts_diameter)
    )
})


# with growth -------------------------------------------------------------


test_that("make_difference works withgrowth", {
  df <-
    storms %>%
    dplyr::select("name", "status", "wind", "pressure", "ts_diameter")

  diff <- make_difference(df, include_growth = TRUE)

  ## check names
  names_are <- names(diff)
  names_should_be <- c(
    non_numeric_cols(df),
    numeric_cols(df),
    paste0(numeric_cols(df)[-1], "_diff"),
    paste0(numeric_cols(df)[-1], "_growth")
  )
  expect_identical(sort(names_are), sort(names_should_be))

  ## check values
  expect_identical(diff$pressure_diff, df$wind - df$pressure)
  expect_identical(
    diff$ts_diameter_diff,
    repl_na(df$wind) - repl_na(df$ts_diameter)
  )

  ## check growth
  growth_is <- diff$pressure_growth
  growth_should_be <- repl_na(
    (round(repl_na(df$wind), 2) - round(repl_na(diff$pressure), 2)) /
      abs(round(repl_na(diff$pressure), 2))
  )
  expect_identical(growth_is, growth_should_be)

  growth_is <- diff$ts_diameter_growth
  growth_should_be <- repl_na(
    (round(repl_na(df$wind), 2) - round(repl_na(diff$ts_diameter), 2)) /
      abs(round(repl_na(diff$ts_diameter), 2))
  )
  expect_identical(growth_is, growth_should_be)
})

