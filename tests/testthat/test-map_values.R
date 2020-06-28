context("mapping values")

test_that("map_values works ", {
  df       <- storms
  xx       <- unique(df$status)
  test_map <- stats::setNames(xx, 1:length(xx))

  values_are <- map_values(df$status, test_map)
  should_be  <- df$status
  for (z in seq_along(xx)) {
    should_be <- gsub(xx[z], as.character(z), should_be)
    }

  expect_identical(values_are, should_be)
})


test_that("map_values works with NAs", {
  df <- storms
  df[1, "status"] <- NA

  xx       <- unique(df$status)
  test_map <- stats::setNames(xx, 1:length(xx))
#
#   values_are <- map_values(df$status, test_map)
#   should_be  <- df$status
#   for (z in seq_along(xx)) {
#     should_be <- gsub(xx[z], as.character(z), should_be)
#   }
#
#   expect_identical(values_are, should_be)
})
