context("Add Counter: can I add a counter to NAs in a variable?")

test_that("add_counter works", {
  df <- head(storms, 100)

  ## reference col == na col
  expect_identical(
    unique(add_counter(df, "ts_diameter")$ts_diameter),
    "other_1"
  )

  ## reference col == different col
  expect_identical(
    unique(add_counter(df, "ts_diameter", "status")$ts_diameter),
    c("other_2", "other_3", "other_1")
  )

  ## check prefix
  expect_identical(
    unique(add_counter(df, "ts_diameter", "status", "test")$ts_diameter),
    c("test_2", "test_3", "test_1")
  )
})

