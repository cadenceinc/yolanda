context("grouping and summarization is functional")


# no arguments ------------------------------------------------------------


test_that("group and summarize works as-is", {
  df <-
    storms %>%
    dplyr::select("status", "category", "pressure") %>%
    group_and_summarize()

  should_be <-
    storms %>%
    dplyr::group_by_at(dplyr::vars("status", "category")) %>%
    dplyr::summarize("pressure" = sum(pressure, na.rm = TRUE)) %>%
    dplyr::ungroup()

  expect_identical(df, should_be)
  })


# specifying groups -------------------------------------------------------


test_that("group and summarize works with specified groups", {
  df <-
    storms %>%
    dplyr::select("status", "category", "pressure") %>%
    group_and_summarize("status")

  should_be <-
    storms %>%
    dplyr::group_by_at(dplyr::vars("status")) %>%
    dplyr::summarize("pressure" = sum(pressure, na.rm = TRUE)) %>%
    dplyr::ungroup()

  expect_identical(df, should_be)
})


# spefifying numeric columns ----------------------------------------------


test_that("group and summarize works with specified summary columns", {
  df <-
    storms %>%
    dplyr::select("status", "category", "pressure", "lat", "long") %>%
    group_and_summarize(., summary_cols = "pressure")

  should_be <-
    storms %>%
    dplyr::group_by_at(dplyr::vars("status", "category")) %>%
    dplyr::summarize("pressure" = sum(pressure, na.rm = TRUE)) %>%
    dplyr::ungroup()

  expect_identical(df, should_be)
})


# negation ----------------------------------------------------------------


test_that("group and summarize works with applied negation", {
  df <-
    storms %>%
    dplyr::select("status", "category", "pressure", "lat", "long") %>%
    group_and_summarize(., "status", negate = TRUE)

  should_be <-
    storms %>%
    dplyr::group_by_at(dplyr::vars("category")) %>%
    dplyr::summarize(
      "pressure" = sum(pressure, na.rm = TRUE),
      "lat"      = sum(lat, na.rm = TRUE),
      "long"     = sum(long, na.rm = TRUE)
      ) %>%
    dplyr::ungroup()

  expect_identical(df, should_be)
})


# alternate function ------------------------------------------------------


test_that("group and summarize works with different functions", {
  df <-
    storms %>%
    dplyr::select("status", "category", "pressure", "lat", "long") %>%
    group_and_summarize(., .f = function(x) mean(x, na.rm = TRUE))

  should_be <-
    storms %>%
    dplyr::group_by_at(dplyr::vars("status", "category")) %>%
    dplyr::summarize(
      "pressure" = mean(pressure, na.rm = TRUE),
      "lat"      = mean(lat, na.rm = TRUE),
      "long"     = mean(long, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  expect_identical(df, should_be)
})
