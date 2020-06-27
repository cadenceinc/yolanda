context("selecting columns based on class")


# numeric cols ------------------------------------------------------------


test_that("numeric_cols works", {
  nc <-
    c(
      "hour",
      "lat",
      "long",
      "wind",
      "pressure",
      "ts_diameter",
      "hu_diameter"
    )
  expect_identical(numeric_cols(storms), nc)
})


# non-numeric cols --------------------------------------------------------


test_that("non_numeric_cols works", {
  nnc <- c("name", "status", "category", "period_ending")
  expect_identical(non_numeric_cols(storms), nnc)
})


# date cols ---------------------------------------------------------------


test_that("date_cols works", {
  expect_identical(date_cols(storms), c("period_ending"))
})
