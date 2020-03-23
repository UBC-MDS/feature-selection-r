#
# Tests for variance_thresholding()
#

#'
#' Test for some general use cases with default thresholding
#'
#' @NoRd
testthat::test_that("relevant features remain", {
  data <- dplyr::tibble(
    a = c(1,2,3),
    b = c(2,2,2),
    c = c('123', '123', '123'),
    d = c('123', '124', '1')
  )

  feature_indexes <- variance_thresholding(data)
  testthat::expect_equal(feature_indexes, c(1, 4))
})

#'
#' Make sure the thresholding apart from the default is working as intended
#'
#' @NoRd
testthat::test_that("appropriate features are dropped with higher threshold", {
  data_a <- dplyr::tibble(
    a = c(1,2,3),
    b = c(2,2,2),
    c = c('123', '123', '123'),
    d = c('123', '14', '1')
  )

  feature_indexes_a <- variance_thresholding(data_a, threshold = 2)
  testthat::expect_equal(feature_indexes_a, c(4))

  data_b <- dplyr::tibble(
    a = c(1,2,3),
    b = c(2,2,2)
  )

  feature_indexes_b <- variance_thresholding(data_b, threshold = 100)
  testthat::expect_equal(feature_indexes_b, NULL)
})

#'
#' Make sure the function blows up when invalid inputs are past in for
#' both the threshold and X
#'
#' @NoRd
testthat::test_that("inputs checking", {
  data <- dplyr::tibble(
    a = c(1,2,3),
    d = c('123', '124', '1')
  )

  testthat::expect_error(
    variance_thresholding(data, threshold=-1),
    "Threshold must be a positive number."
  )
  testthat::expect_error(
    variance_thresholding(data, threshold='123'),
    "Threshold must be a positive number."
  )
  testthat::expect_error(
    variance_thresholding(123),
    "Expected a `data.frame` object for `data`."
  )
})
