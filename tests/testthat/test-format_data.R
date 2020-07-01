context("formatting numerical data")


# format values -----------------------------------------------------------


test_that("fmt_value_by_col works", {
  expect_error(fmt_value_by_col(storms, 1))

  expect_identical(
    fmt_value_by_col(storms),
    dplyr::mutate_if(storms, is.numeric, ~ round(./1000))
  )

  expect_identical(
    fmt_value_by_col(storms, 2),
    dplyr::mutate_at(storms, dplyr::vars(2), ~ round(./1000))
  )
})

test_that("fmt_value_by_row works", {
  expect_identical(
    fmt_value_by_row(storms),
    dplyr::mutate_if(storms, is.numeric, ~ round(./1000))
  )

  expect_identical(
    fmt_value_by_row(storms, 2),
    {
      nc <- numeric_cols(storms)
      storms[2, nc] <- round(storms[2, nc] / 1000)
      storms
    }
  )
})

test_that("fmt_value_by_row_and_col works", {
  expect_identical(
    fmt_value_by_row_and_col(storms),
    dplyr::mutate_if(storms, is.numeric, ~ round(./1000))
  )

  ## row only
  expect_identical(
    fmt_value_by_row_and_col(storms, 2),
    {
      nc <- numeric_cols(storms)
      storms[2, nc] <- round(storms[2, nc] / 1000)
      storms
    }
  )

  ## column only
  expect_identical(
    fmt_value_by_row_and_col(storms, cols = 2),
    dplyr::mutate_at(storms, dplyr::vars(2), ~ round(./1000))
  )

  ## row and column
  expect_identical(
    fmt_value_by_row_and_col(storms, 2, 2),
    {
      storms[2, 2] <- round(storms[2, 2] / 1000)
      storms
    })
})


# format percent ----------------------------------------------------------


test_that("fmt_percent_by_col works", {
  expect_error(fmt_percent_by_col(storms, 1))

  expect_identical(
    fmt_percent_by_col(storms),
    dplyr::mutate_if(storms, is.numeric, ~ round(. * 100, 2))
  )

  expect_identical(
    fmt_percent_by_col(storms, 2),
    dplyr::mutate_at(storms, dplyr::vars(2), ~ round(.* 100, 2))
  )
})

test_that("fmt_percent_by_row works", {
  expect_identical(
    fmt_percent_by_row(storms),
    dplyr::mutate_if(storms, is.numeric, ~ round(.* 100, 2))
  )

  expect_identical(
    fmt_percent_by_row(storms, 2),
    {
      nc <- numeric_cols(storms)
      storms[2, nc] <- round(storms[2, nc] * 100, 2)
      storms
    }
  )
})

test_that("fmt_percent_by_row_and_col works", {
  expect_identical(
    fmt_percent_by_row_and_col(storms),
    dplyr::mutate_if(storms, is.numeric, ~ round(.* 100, 2))
  )

  ## row only
  expect_identical(
    fmt_percent_by_row_and_col(storms, 2),
    {
      nc <- numeric_cols(storms)
      storms[2, nc] <- round(storms[2, nc] * 100, 2)
      storms
    }
  )

  ## column only
  expect_identical(
    fmt_percent_by_row_and_col(storms, cols = 2),
    dplyr::mutate_at(storms, dplyr::vars(2), ~ round(.* 100, 2))
  )

  ## row and column
  expect_identical(
    fmt_percent_by_row_and_col(storms, 2, 2),
    {
      storms[2, 2] <- round(storms[2, 2] * 100, 2)
      storms
    })
})
