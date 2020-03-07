#
# Tests for variance_threshold_select()
#
library(tibble)

testthat::test_that("relevant features remain", {
  data <- tibble(
    a = c(1,2,3),
    b = c(2,2,2),
    c = c('123', '123', '123'),
    d = c('123', '124', '1')
  )

  feature_indexes <- variance_threshold_select(data)
  testthat::expect_identical(feature_indexes, c(1, 4))
})

testthat::test_that("appropriate features are dropped with higher threshold", {
  data <- tibble(
    a = c(1,2,3),
    b = c(2,2,2),
    c = c('123', '123', '123'),
    d = c('123', '14', '1')
  )

  feature_indexes <- variance_threshold_select(data, threshold = 2)
  testthat::expect_identical(feature_indexes, c(4))
})

testthat::test_that("inputs checking", {
  data <- tibble(
    a = c(1,2,3),
    d = c('123', '124', '1')
  )

  testthat::expect_error(
    variance_threshold_select(data, threshold=-1),
    "Threshold must be a positive number."
  )
  testthat::expect_error(
    variance_threshold_select(data, threshold='123'),
    "Threshold must be a positive number."
  )
  testthat::expect_error(
    variance_threshold_select(123),
    "Expected a `data.frame` object for `data`."
  )
})
