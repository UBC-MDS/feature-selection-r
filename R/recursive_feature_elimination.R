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
#' @param X features
#' @param y response
#' @param n_features_to_select The target number of features.
#'
#' @return A vector of feature indices representing selected features in ranked order.
#' @export
#'
#' @examples
#' # TBA
recursive_feature_elimination <- function(scorer, X, y, n_features_to_select=None) {
  c()
}
