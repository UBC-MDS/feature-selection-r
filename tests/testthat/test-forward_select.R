#
# Tests for forward_selection()
#

#' Sample custom scorer that fits a model and returns
#' an appropriate score for the feature selection problem.
#'
#' @param data A data.frame with features and response columns.
#'
#' @return The mean squared error (MSE) of the model
scorer <- function(data) {
   model <- lm(Y ~ ., data)
   return(mean(model$residuals^2))
}

#'
#' This test creates a dataset that has 5 features that
#' are used to compute `Y`.
#' The remaining features are independent of `Y`.
#' This test should select the 5 feature columns used to compute `Y`.
#'
#' @NoRd
testthat::test_that("relevant features remain", {
  # Create dataframe and remove Y true column; keep Y.
  set.seed(0)
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- data[1:(length(data)-1)]
  y <- data[length(data)]

  # Run Test compare scores with expected ones
  set.seed(1230)
  results <- forward_selection(scorer, X, y, 4, 4)
  testthat::expect_setequal(results, c(1, 2, 4, 5))
})

#'
#' Make sure that the inputs are valid.
#' Namely, X and Y need to be data frames and min feature number > 0.
#'
#' @NoRd
testthat::test_that("X and y tests", {
  set.seed(0)
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- data[1:(length(data)-1)]
  y <- data[length(data)]

  # X and y are Dataframes
  testthat::test_that("X param is a data.frame (or tibble)", {
    testthat::expect_error(forward_selection(scorer, 0, y), "data.frame")
    testthat::expect_error(forward_selection(scorer, "nonsense", y), "data.frame")
    testthat::expect_error(forward_selection(scorer, c(), y), "data.frame")
    testthat::expect_error(forward_selection(scorer, list(), y), "data.frame")
  })
  testthat::test_that("y param is a data.frame (or tibble)", {
    testthat::expect_error(forward_selection(scorer, X, 0), "data.frame")
    testthat::expect_error(forward_selection(scorer, X, "nonsense"), "data.frame")
    testthat::expect_error(forward_selection(scorer, X, c()), "data.frame")
    testthat::expect_error(forward_selection(scorer, X, list()), "data.frame")
  })

  array_3d <- data.frame(array(rnorm(100), c(100)))
  # X is a 1-d array
  testthat::expect_error(forward_selection(scorer, array_3d, y), "X must be a 2-d array")

  # y is a 1-d array
  testthat::expect_error(forward_selection(scorer, X, X), "y must be a 1-d array")

  # check that min number of features is greater than or equal to one
  testthat::expect_error(forward_selection(scorer, X, y, 0, 6))

  # check min_features smaller or equal to number or features of X.
  testthat::expect_error(forward_selection(scorer, X, y, 20, 21))

  # check X and y have the same number of samples
  testthat::expect_error(forward_selection(scorer, X, data.frame(y$Y[1:10])))
})

#'
#' Check that the number of features are between min and max number of features
#'
#' @NoRd
testthat::test_that("min_features and max_features works properly", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- data[1:(length(data)-1)]
  y <- data[length(data)]
  testthat::expect_gte(length(forward_selection(scorer, X, y, 5, 6)), 4)
  testthat::expect_lte(length(forward_selection(scorer, X, y, 5, 6)), 6)
  testthat::expect_error(forward_selection(scorer, X, y, 6, 5))
})

#'
#' Check that 'scorer' is a function
#'
#' @NoRd
testthat::test_that("`scorer param is a function", {
  testthat::expect_error(forward_selection(0, X, y, 4, 4), "scorer")
  testthat::expect_error(forward_selection("a", X, y, 4, 4), "scorer")
})
