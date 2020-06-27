context("Partition: partitioning dataframes is functional")


test_that("partition works", {
  ## one partition
  pl <- partition(head(storms, 100), "name")

  nms <- c("Amy", "Belle", "Caroline", "Doris")
  expect_identical(length(pl), 4L)
  expect_identical(sort(names(pl)), nms)
  expect_identical(attributes(pl)$partitioned_columns, "name")
  expect_equivalent(attributes(pl)$partition_lengths, 4)
  expect_identical(attributes(pl)$partition_levels, 1L)
  expect_identical(sort(attributes(pl)$partitions$name), nms)

  ## two partitions
  pl <- partition(head(storms, 100), c("name", "status"))

  nms <- c("Amy", "Belle", "Caroline", "Doris")
  sts <- c("hurricane", "tropical depression", "tropical storm")
  expect_identical(length(pl), 4L)
  expect_identical(sort(names(pl)), nms)
  expect_identical(attributes(pl)$partitioned_columns, c("name", "status"))
  expect_equivalent(attributes(pl)$partition_lengths, c(4, 3))
  expect_identical(attributes(pl)$partition_levels, 2L)
  expect_identical(sort(attributes(pl)$partitions$name), nms)
  expect_identical(sort(attributes(pl)$partitions$status), sts)

  ## three partitions
  pl <- partition(head(storms, 100), c("name", "status", "category"))

  pts <- c("name", "status", "category")
  nms <- c("Amy", "Belle", "Caroline", "Doris")
  sts <- c("hurricane", "tropical depression", "tropical storm")
  cts <- c("-1", "0", "1", "2", "3")
  expect_identical(length(pl), 4L)
  expect_identical(sort(names(pl)), nms)
  expect_identical(attributes(pl)$partitioned_columns, pts)
  expect_equivalent(attributes(pl)$partition_lengths, c(4, 3, 5))
  expect_identical(attributes(pl)$partition_levels, 3L)
  expect_identical(sort(attributes(pl)$partitions$name), nms)
  expect_identical(sort(attributes(pl)$partitions$status), sts)
  expect_identical(paste(sort(attributes(pl)$partitions$category)), cts)

  ## columns that don't exist
  expect_message(partition(head(storms, 100), c("name", "test")))
})


