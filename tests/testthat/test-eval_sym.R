context("evaluating symbols")

test_that("eval_sym works", {

  ## does not work with select
  ## because select takes character as input
  expect_error(dplyr::select(storms, eval_sym("name")))

  ## does work with mutate
  expect_identical(
    storms,
    dplyr::mutate(storms, "name" = eval_sym("name"))
    )

  ## allows for creating new variables
  expect_identical(
    storms %>% dplyr::mutate("test" = .$hour + .$wind),
    storms %>% dplyr::mutate("test" = eval_sym("hour") + eval_sym("wind"))
    )
})
