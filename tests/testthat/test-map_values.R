context("mapping values")


# map value ---------------------------------------------------------------


test_that("map_values works", {
  test_vect   <- c("one", "two", "three")
  test_map    <- stats::setNames(test_vect, seq_along(test_vect))
  test_result <- map_values(test_vect, test_map)
  expect_identical(test_result, paste0(1:3))
})

test_that("map_values works with NAs", {
  test_vect   <- c("one", "two", "three", NA)
  test_map    <- stats::setNames(test_vect, seq_along(test_vect))
  test_result <- map_values(test_vect, test_map)
  expect_identical(test_result, paste0(1:4))
})

test_that("map_values works with non-matches", {
  ## values that have no match to the map should return their original value
  test_vect   <- c("one", "two", "three", "four")
  test_map    <- stats::setNames(test_vect[1:3], c(1:3))
  test_result <- map_values(test_vect, test_map)
  expect_identical(test_result, c(paste0(1:3), "four"))

  ## if a replacement is provided, then the replacement is used instead of
  ## the original value
  test_result <- map_values(test_vect, test_map, replacement = "test")
  expect_identical(test_result, c(paste0(1:3), "test"))
})


# map funct ---------------------------------------------------------------


test_that("map_funct works", {
  test_vect   <- c("one", "two", "three")
  test_result <- map_funct(test_vect, toupper)
  expect_identical(test_result,toupper(test_vect))
})

test_that("map_funct works with NAs", {
  ## the function will attempt to be applied to the vector, and return
  ## the result if possible, or the original.  In this vase the original is
  ## NA.
  test_vect   <- c("one", "two", "three", NA)
  test_result <- map_funct(test_vect, toupper)
  expect_identical(test_result, toupper(test_vect))

  test_result <- map_funct(test_vect, function(x) gsub("o", "1", x))
  expect_identical(test_result, c("1ne", "tw1", "three", NA))
})


# map date ----------------------------------------------------------------


test_that("map_date works", {
  test_vect <- as.Date(c("2020-05-31", "2020-04-30", "2020-03-31"))
  test_result <- map_date(test_vect)
  expect_identical(test_result, c("May 2020", "Apr 2020", "Mar 2020"))
})

test_that("map_date works for fiscal week", {
  test_vect <- as.Date(c("2020-05-31", "2020-04-30", "2020-03-31"))
  test_result <- map_date(test_vect, style = "epiweek")
  expect_identical(test_result, c("23 2020", "18 2020", "14 2020"))
})


# partial map values ------------------------------------------------------


test_that("pmap_values works", {
  test_vect <- c("one", "two", "three")
  test_map <- stats::setNames(c("on|ten", "tw|eleven", "thre|twenty"), 1:3)
  test_result <- pmap_values(test_vect, test_map)
  expect_identical(test_result, paste0(1:3))
})
