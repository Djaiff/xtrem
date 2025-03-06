context("remove_xtrem_duplicates")
library(tidyverse)

test_that("remove_xtrem_duplicates() works 2 same-values duplicates", {
  # min with same values
  data <- tibble(Status = c("max", "min", "max", "min", "min"),
                 Idx = c(3, 5, 10, 15, 20),
                 Values = c(13, 8, 15, 10, 10))
  res <- tibble(Status = c("max", "min", "max", "min"),
                Idx = c(3, 5, 10, 20),
                Values = c(13, 8, 15, 10))
  expect_equivalent(remove_xtrem_duplicates(data, TRUE), res)

  data <- tibble(Status = c("max", "min", "max", "min", "min"),
                 Idx = c(3, 5, 10, 15, 20),
                 Values = c(13, 8, 15, 10, 10))
  res <- tibble(Status = c("max", "min", "max", "min"),
                Idx = c(3, 5, 10, 15),
                Values = c(13, 8, 15, 10))
  expect_equivalent(remove_xtrem_duplicates(data, FALSE), res)
})

test_that("remove_xtrem_duplicates() works 2 different-values duplicates", {
  # min with different values
  data <- tibble(Status = c("max", "min", "max", "min", "min"),
                 Idx = c(3, 5, 10, 15, 20),
                 Values = c(13, 8, 15, 10, 8))
  res <- tibble(Status = c("max", "min", "max", "min"),
                Idx = c(3, 5, 10, 20),
                Values = c(13, 8, 15, 8))
  expect_equivalent(remove_xtrem_duplicates(data, TRUE), res)
})

test_that("remove_xtrem_duplicates() works several duplicates", {
  # multiple min and max duplicates
  data <- tibble(Status = c("max", "max", "min", "max", "min", "min"),
                 Idx = c(3, 5, 10, 15, 20, 25),
                 Values = c(13, 14, 8, 15, 10, 8))
  res <- tibble(Status = c("max", "min", "max", "min"),
                Idx = c(5, 10, 15, 25),
                Values = c(14, 8, 15, 8))
  expect_equivalent(remove_xtrem_duplicates(data, TRUE), res)
})
