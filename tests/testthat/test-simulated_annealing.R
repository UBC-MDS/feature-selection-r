#
# Tests for simulated_annealing()
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

# This test creates a dataset that has 5 features that are are used to compute `Y`.
# The remaining features are independent of `Y`.
# This test should select the 5 feature columns used to compute `Y`.
testthat::test_that("relevant features remain", {
  # Create dataframe and remove Ytrue column; keep Y.
  set.seed(0)
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- data[1:(length(data)-1)]
  y <- data[length(data)]

  # Run Test
  results <- replicate(simulated_annealing(scorer, X, y, bools = TRUE), n = 50)
  result_tally <- rowSums(results)
  testthat::expect_gte(result_tally[1], result_tally[6])
  testthat::expect_gte(result_tally[2], result_tally[7])
  testthat::expect_gte(result_tally[4], result_tally[9])
  testthat::expect_gte(result_tally[5], result_tally[10])
})

# Test output arrays are not empty
testthat::test_that("features returned is not empty", {
  # Create dataframe and remove Ytrue column; keep Y.  
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- data[1:(length(data)-1)]
  y <- data[length(data)]

  # Run Test
  testthat::expect_gt(length(simulated_annealing(scorer, X, y)), 0)
  testthat::expect_gt(sum(simulated_annealing(scorer, X, y, bools = TRUE)), 0)
})

testthat::test_that("`scorer param is a function", {
  testthat::expect_error(simulated_annealing(0, data.frame(), data.frame()), "scorer")
})

testthat::test_that("X param is a data.frame (or tibble)", {
  testthat::expect_error(recursive_feature_elimination(scorer, 0, data.frame()), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, "nonsense", data.frame()), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, c(), data.frame()), "data.frame")
  testthat::expect_error(recursive_feature_elimination(scorer, list(), data.frame()), "data.frame")
})

testthat::test_that("tibbles work too!", {
  data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
  X <- data[1:(length(data)-1)]
  y <- data[length(data)]
  X <- dplyr::as_tibble(X)

  testthat::expect_gt(length(simulated_annealing(scorer, X, y)), 0)
})