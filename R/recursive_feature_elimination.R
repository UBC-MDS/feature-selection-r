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
#' @param X A data.frame of shape (n_samples, n_features) with training samples.
#' @param y A data.frame of shape (n_samples, n_outputs)
#'          with true outputs used for training.
#' @param n_features_to_select The target number of features.
#'
#' @return Vector of column names of non-eliminated features.
#' @export
#'
#' @examples
#' custom_scorer_fn <- function(data) {
#'   model <- lm(Y ~ ., data)
#'   names(which.min(model$coefficients[-1]))[[1]]
#' }
#' df <- tgp::friedman.1.data()
#' data <- dplyr::select(df, -Ytrue)
#' X <- dplyr::select(data, -Y)
#' y <- dplyr::select(data, Y)
#' features <- featureselection::recursive_feature_elimination(custom_scorer_fn, X, y, 4)
#' # [1] "X1" "X2" "X4" "X5" "Y"
recursive_feature_elimination <- function(scorer, X, y, n_features_to_select) {
  # Is `scorer` a function?
  if (class(scorer) != "function") {
    stop("Expected a function for `scorer`." )
  }

  # Is X a data.frame or something compatible like tibble?
  if (!any(class(X) == "data.frame")) {
    stop("Expected a `data.frame` object for `X`.")
  }

  # Is y a data.frame or something compatible like tibble?
  if (!any(class(y) == "data.frame")) {
    stop("Expected a `data.frame` object for `y`.")
  }

  # Do both X and y have the same number of examples?
  if (nrow(X) != nrow(y)) {
    stop("Expected both X and y to have the same number of examples")
  }

  # Join data frames into single data frame
  data <- cbind(X, y)

  eliminated_features <- c()
  total_features <- ncol(X)

  # n_features_to_select must be a number
  if (!any(typeof(n_features_to_select) == c("integer", "double"))) {
    stop("Expected a number for `n_features_to_select`.")
  }

  # Warn if n_features_to_select is not a whole number
  if (n_features_to_select%%1 != 0) {
    warning("`n_features` is not a whole number. It was truncated.")
    n_features_to_select <- floor(n_features_to_select)
  }

  # n_features_to_select must be >= 1 and < total features
  if (n_features_to_select < 1 || n_features_to_select >= total_features) {
    stop(paste("Expected a value between 1 and", total_features - 1, "for `n_features_to_select`."))
  }

  for (i in 1:total_features) {
    # Remove currently eliminated features
    if (length(eliminated_features) == 0) {
      features_to_try <- data
    } else {
      features_to_try <- dplyr::select(data, -eliminated_features)
    }

    # Get the next feature to remove
    feature_to_remove <- scorer(features_to_try)
    eliminated_features <- c(eliminated_features, feature_to_remove)

    # If we have our target number of features, stop.
    if (length(eliminated_features) + n_features_to_select >= total_features)
      break
  }

  # Return a vector of the features to keep
  purrr::reduce(names(data),
                function(acc, col) { if (any(eliminated_features == col)) acc else c(acc, col) },
                .init = c())
}
