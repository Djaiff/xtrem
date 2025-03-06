context("remove_noise")
library(tidyverse)

test_that("remove_noise() works as expected in simple case", {
  xtrem_data <- tibble(Idx = c(1, 3, 5, 6, 7),
                       Values = c(11, 13, 10, 10.04, 10),
                       Status = c("min","max","min","max","min"))

  last <- FALSE
  res <- tibble(Idx = c(1,3,5), Values = c(11,13,10),
                Status = c("min","max","min"))
  expect_equivalent(remove_noise(xtrem_data, last), res)

  last <- TRUE
  res <- tibble(Idx = c(1,3,7), Values = c(11,13,10),
                Status = c("min","max","min"))
  expect_equivalent(remove_noise(xtrem_data, last), res)

})

test_that("remove_noise() works as expected complex case", {
  xtrem_data <- tibble(Idx = c(1, 3, 5, 6, 7),
                       Values = c(11, 11.03, 10, 10.04, 10),
                       Status = c("min","max","min","max","min"))

  last <- FALSE
  res <- tibble(Idx = 5, Values = 10, Status = "min")
  expect_equivalent(remove_noise(xtrem_data, last), res)

  last <- TRUE
  res <- tibble(Idx = c(7), Values = c(10),
                Status = c("min"))
  expect_equivalent(remove_noise(xtrem_data, last), res)
})

test_that("remove_noise() works as expected with real cases", {
  xtrem_data <- readRDS("data/xtrem_noisy_1.RDS")
  res <- readRDS("data/xtrem_noisy_1_expected.RDS")
  expect_equivalent(remove_noise(xtrem_data), res)
})
