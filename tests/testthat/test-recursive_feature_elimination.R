#
# Tests for recursive_feature_elimination()
#

#' Sample custom scorer that fits a model and returns
#' an appropriate score for the feature selection problem.
#'
#' @param data A data.frame with features and response columns.
#'
#' @return The column name to be eliminated
scorer <- function(data) {
  model <- lm(Y ~ ., data)
  names(which.min(model$coefficients[-1]))[[1]]
}

# This test creates a dataset that has 5 features that are are used to compute `Y`.
# The remaining features are independent of `Y`.
# This test should select the 5 feature columns used to compute `Y`.
testthat::test_that("relevant features remain", {
  # create dataframe and remove Ytrue column; keep Y.
  set.seed(0)
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)

  # test
  features <- recursive_feature_elimination(scorer, data, n_features_to_select = 4)
  testthat::expect_identical(features, c("X1", "X2", "X4", "X5", "Y"))
})

# Test early stop logic and conversion from list of eliminated features
# to list of features to keep.
# NOTE: Number of features returned is always one more than specified
# because it never eliminates target output column.
testthat::test_that("correct number of features are returned", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, data, 1)), 2)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, data, 2)), 3)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, data, 9)), 10)
})

testthat::test_that("`scorer param is a function", {
  testthat::expect_error(recursive_feature_elimination(0, data.frame(), 1), "scorer")
})

testthat::test_that("data param is a data.frame (or tibble)", {
  testthat::expect_error(recursive_feature_elimination(scorer, 0, 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, "nonsense", 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, c(), 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, list(), 1), "data.frame")
})

testthat::test_that("tibbles work too!", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  data <- dplyr::as_tibble(data)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, data, 1)), 2)
})

testthat::test_that("n_features_to_select param is a number within valid range", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  testthat::expect_warning(recursive_feature_elimination(scorer, data, 1.1), "truncated")
  testthat::expect_error(recursive_feature_elimination(scorer, data, "not a number"), "Expected a number")
  testthat::expect_error(recursive_feature_elimination(scorer, data, -1), "value between")
  testthat::expect_error(recursive_feature_elimination(scorer, data, 0), "value between")
  testthat::expect_error(recursive_feature_elimination(scorer, data, 10), "value between")
  testthat::expect_error(recursive_feature_elimination(scorer, data, 11), "value between")
})
