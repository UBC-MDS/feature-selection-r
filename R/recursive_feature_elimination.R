#' Select features by Recursive Feature Elimination
#'
#' @description
#' Iteratively fit and score an estimator and eliminate features
#' based on the score.
#'
#' The lower the score, the more favourable for the feature.
#' If your score is better the larger it is, then you should return the
#' negative of that score, for example negative MSE.
#'
#' @param scorer A user-defined function that fits an estimnator and returns a score.
#' @param data A data.frame or tibble
#' @param n_features_to_select The target number of features.
#'
#' @return A vector of feature indices representing selected features in ranked order.
#' @export
#'
#' @examples
#' # TBA
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
