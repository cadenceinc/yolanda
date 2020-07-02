context("find the depth of a list")

test_that("depth works", {
  expect_identical(depth("test"), 0)
  expect_identical(depth(data.frame("test" = 1)), 0)
  expect_identical(depth(list()), 0)
  expect_identical(depth(list(list())), 1)
  expect_identical(depth(list(list(list(1)))), 3)
  expect_identical(depth(list("test" = list("test" = 1))), 2)
})
