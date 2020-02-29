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
#' @param score_fn A user-defined function that fits an estimnator and returns a score.
#' @param num_features The target number of features.
#'
#' @return A vector of feature indices representing selected features in ranked order.
#' @export
#'
#' @examples
recursive_feature_elimination <- function(score_fn, num_features) {

}
