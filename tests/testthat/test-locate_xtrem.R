context("locate_xtrem")
library(tidyverse)

test_that("locate_xtrem() works with duplicates", {
  x <- c(10,10,9,8,7,7,7,8,9,9)

  last <- FALSE
  res <- tibble(Idx = c(1, 5, 9), Values = c(10, 7, 9),
                Status = c("max", "min", "max")) %>%
    mutate_at(vars(Idx), as.integer)
  expect_equivalent(locate_xtrem(x, last) %>% select(-NB, -Last), res)

  last <- TRUE
  res <- tibble(Idx = c(2, 7, 10), Values = c(10, 7, 9),
                Status = c("max", "min", "max")) %>%
    mutate_at(vars(Idx), as.integer)
  expect_equivalent(locate_xtrem(x, last) %>% select(-NB, -Last), res)
})


