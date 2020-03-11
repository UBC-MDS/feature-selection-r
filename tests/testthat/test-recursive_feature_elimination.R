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
  X <- dplyr::select(data, -Y)
  y <- dplyr::select(data, Y)

  # test
  features <- recursive_feature_elimination(scorer, X, y, n_features_to_select = 4)
  testthat::expect_identical(features, c("X1", "X2", "X4", "X5", "Y"))
})

# Test early stop logic and conversion from list of eliminated features
# to list of features to keep.
# NOTE: Number of features returned is always one more than specified
# because it never eliminates target output column.
testthat::test_that("correct number of features are returned", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- dplyr::select(data, -Y)
  y <- dplyr::select(data, Y)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, X, y, 1)), 2)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, X, y, 2)), 3)
  testthat::expect_equal(length(recursive_feature_elimination(scorer, X, y, 9)), 10)
})

#
# Test that the user-defined customer scorer is verified to be a function
#
testthat::test_that("`scorer param is a function", {
  testthat::expect_error(recursive_feature_elimination(0, data.frame(), 1), "scorer")
})

#
# Test that X is a data.frame
#
testthat::test_that("data param X is a data.frame (or tibble)", {
  testthat::expect_error(recursive_feature_elimination(scorer, 0, data.frame(), 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, "nonsense", data.frame(), 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, c(), data.frame(), 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, list(), data.frame(), 1), "data.frame")
})

#
# Test that y is a data.frame
#
testthat::test_that("data param y is a data.frame (or tibble)", {
  testthat::expect_error(recursive_feature_elimination(scorer, data.frame(), 0, 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, data.frame(), "nonsense", 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, data.frame(), c(), 1), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, data.frame(), list(), 1), "data.frame")
})

#
# Test that X and y are the same length
#
testthat::test_that("X and y have the same number of examples ", {
  testthat::expect_error(recursive_feature_elimination(scorer,
                                                       X = data.frame(f1 = c(1, 2)),
                                                       y = data.frame(f2 = c(1)),
                                                       n_features_to_select = 1),
                         regexp = "examples")
})

#
# Test with dplyr::tibble
#
testthat::test_that("tibbles work too!", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- dplyr::as_tibble(dplyr::select(data, -Y))
  y <- dplyr::as_tibble(dplyr::select(data, Y))
  testthat::expect_equal(length(recursive_feature_elimination(scorer, X, y, 1)), 2)
})

#
# Test n_features_to_select input
#
testthat::test_that("n_features_to_select param is a number within valid range", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- dplyr::select(data, -Y)
  y <- dplyr::select(data, Y)
  testthat::expect_warning(recursive_feature_elimination(scorer, X, y, 1.1), "truncated")
  testthat::expect_error(recursive_feature_elimination(scorer, X, y, "not a number"), "Expected a number")
  testthat::expect_error(recursive_feature_elimination(scorer, X, y, -1), "value between")
  testthat::expect_error(recursive_feature_elimination(scorer, X, y, 0), "value between")
  testthat::expect_error(recursive_feature_elimination(scorer, X, y, 10), "value between")
  testthat::expect_error(recursive_feature_elimination(scorer, X, y, 11), "value between")
})
