#' Select features by Recursive Feature Elimination
#'
#' @description
#' Feature selector that implements recursive feature elimination
#'
#' Implements a greedy algorithm that iteratively calls the user-supplied
#' scorer function and eliminates features based on its return value.
#'
#' @param scorer A custom user-supplied function that accepts a data.frame
#' as input and returns the column name of the column with the lowest weight.
#' @param data A data.frame or tibble
#' @param n_features_to_select The target number of features.
#'
#' @return Vector of column names of non-eliminated features.
#' @export
#'
#' @examples
#' > custom_scorer_fn <- function(data) {
#' >   model <- lm(Y ~ ., data)
#' >   names(which.min(model$coefficients[-1]))[[1]]
#' > }
#' > df <- tgp::friedman.1.data()
#' > data <- dplyr::select(df, -Ytrue)
#' > features <- featureselection::recursive_feature_elimination(custom_scorer_fn, data, 4)
#' [1] "X1" "X2" "X4" "X5" "Y"

recursive_feature_elimination <- function(scorer, data, n_features_to_select) {
  eliminated_features <- c()
  total_features <- ncol(data) - 1

  for (i in 1:(ncol(data) - 2)) {
    # Remove currently eliminated features
    features_to_try <- dplyr::select(data, -eliminated_features)

    # Get the next feature to remove
    feature_to_remove <- scorer(features_to_try)
    eliminated_features <- c(eliminated_features, feature_to_remove)

    # If we have our target number of features, stop.
    if (length(eliminated_features) + n_features_to_select >= total_features)
      break
  }

  # Return a vector of the features to keep
  purrr::reduce(names(data),
                function(acc, col) { if (any(eliminated_features == col)) acc else c(acc, col) })
}
