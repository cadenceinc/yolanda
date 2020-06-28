context("shearing data")

test_that("shear works", {
  ## one selection
  df1 <- shear(storms, "name" = "Amy")
  df2 <- storms %>%
    dplyr::filter(.$name == "Amy") %>%
    dplyr::select(-"name")
  expect_identical(df1, df2)

  ## two selections
  df1 <- shear(storms, "name" = "Amy", "status" = "tropical depression")
  df2 <-
    storms %>%
    dplyr::filter(.$name == "Amy", .$status == "tropical depression") %>%
    dplyr::select(-"name", -"status")
  expect_identical(df1, df2)

  ## selection does not exist
  ## should return original data
  expect_identical(shear(storms, "test" = "test"), storms)

  ## one selection does not exist
  ## another selection does exist
  df1 <- shear(storms, "name" = "Amy", "test" = "test")
  df2 <- storms %>%
    dplyr::filter(.$name == "Amy") %>%
    dplyr::select(-"name")
  expect_identical(df1, df2)
})

