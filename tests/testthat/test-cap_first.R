context("capitalize first letter of a word")

test_that("cap_first works", {
  expect_identical(
    cap_first(c("one", "i don't know", NA)),
    c("One", "I Don't Know", NA)
  )

  expect_error(cap_first(1:10))
  expect_identical(cap_first(paste0(1:10)), paste0(1:10))
})
