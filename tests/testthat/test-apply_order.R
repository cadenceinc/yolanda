context("ordering rows and columns")


# column ordering ---------------------------------------------------------


test_that("apply_col_order works", {
  new_order <- c("lat", "status")
  df <- storms %>% dplyr::select("status", "category", "lat", "wind")

  expect_identical(
    names(apply_col_order(df, new_order)),
    c("status", "category", "lat", "wind")
    )
})

test_that("apply_col_order works with unused column choices", {
  new_order <- c("lat", "status", "test")
  df <- storms %>% dplyr::select("status", "category", "lat", "wind")

  expect_identical(
    names(apply_col_order(df, new_order)),
    c("status", "category", "lat", "wind")
  )
})

test_that("apply_col_order works with no column choices", {
  df <- storms %>% dplyr::select("status", "category", "lat", "wind")

  expect_identical(
    names(apply_col_order(df)),
    c("category", "status", "lat", "wind")
  )
})


# row ordering ------------------------------------------------------------


test_that("apply_row_order works", {
  df <- storms %>% dplyr::select("status", "category", "lat", "wind")

  expect_identical(
    apply_row_order(df),
    df[do.call(order, lapply(df, function(x) x)), ]
  )
})

test_that("apply_row_order works with reversal", {
  df <- storms %>% dplyr::select("status", "category", "lat", "wind")

  expect_identical(
    apply_row_order(df, reverse_cols = "category"),
    df %>% dplyr::arrange(.$status, desc(.$category), .$lat,  .$wind)
  )

  expect_identical(
    apply_row_order(df, reverse_cols = c("status", "category")),
    df %>% dplyr::arrange(desc(.$status), desc(.$category), .$lat,  .$wind)
  )
})
