#' Select features based on feature selection
#'
#' @description Select features using forward selection algorithm.
#'   It starts as an empty model, and add the variable with the
#'   highest improvement in the accuracy of the model. The process
#'   is iteratively repeated and it stops when the remaining variables
#'   doesn't improve the accuracy of the model.
#'
#' @param scorer  A custom user-supplied function that accepts X and y
#'   (as defined below) as input and returns the error of the datasets.
#' @param X tibble. explanatory variables
#' @param y tibble. target
#' @param min_features double. number of minimum features to select
#' @param max_features double. number of maximum features to select
#'
#' @return vector. The indexes of selected features
#' @export
#' @examples
#' my_scorer <- function(data) {
#'   model <- lm(Y ~ ., data)
#'   return(mean(model$residuals^2))
#' }
#' data <- dplyr::select(tgp::friedman.1.data(), -Ytrue)
#' train_data <- data[1:(length(data)-1)]
#' test_data <- data[length(data)]
#' features <- featureselection::forward_selection(my_scorer, train_data, test_data, 3, 7)
#' # [1] 4 2 1 5

forward_selection <- function(scorer, X, y, min_features=1, max_features=10) {
  # Tests
  # 'scorer' must be a function
  if (class(scorer) != "function") {
    stop("Expected a function for `scorer`." )
  }

  # X must be a data.frame or something compatible
  if (!any(class(X) == "data.frame")) {
    stop("Expected a 'data.frame' object for `data`.")
  }

  if (length(dim(X)) != 2){
    stop("X must be a 2-d array")
  }

  # y must be a data.frame or something compatible
  if (!any(class(y) == "data.frame")) {
    stop("Expected a 'data.frame' object for `data`.")
  }

  if (length(dim(y)) != 1){
    stop("y must be a 1-d array")
  }

  # Looks for X and y have the same number of samples
  if (dim(X)[1] != dim(y)[1]){
    stop("X and y have inconsistent numbers of samples. X:", dim(X)[1], ", y:", dim(y)[1])
  }

  # max_features should be greather or equal to min_features
  if (min_features > max_features){
    stop("max_features should be greater or equal to min_features.")
  }

  # min_features should be positive, we want to select at least one feature
  if (min_features < 1){
    stop("min_features should be more than zero.")
  }

  # min_features should be no greater than number of features
  if (min_features > length(X)){
    stop("min_features should be smaller or equal to number of X's features.")
  }

  # Initialize parameters
  # Obtain initial array of randomly selected features
  ftr_no_selection <- c(1:length(X))
  ftr_selection <- c()
  ftr_running <- c()
  scores_fn <- c()
  X_new <- c()
  best_scores_iter <- Inf
  best_scores_all <- c()
  flag_keep_running <- TRUE
  flag_stop_running <- FALSE

  # Body of the function
  repeat{

    # get the scores for the features that haven't been added to the model
    for (i in ftr_no_selection){
      X_new = X[c(ftr_selection, i)]
      ftr_running[i] <- i
      scores_fn[i] <- scorer(cbind(X_new, y))
    }

    # selects the feature with the best performance of each itteration
    best_scores_iter <- min(scores_fn)
    selected_ftr <- match(best_scores_iter, scores_fn)

    # flag to stop the algorithm if the score doesn't decrease at least by 5%
    if (length(ftr_selection) >= 1){
      if (((min(best_scores_all) - best_scores_iter) / min(best_scores_all)) <= 0.05){
        flag_stop_running = TRUE
      }
    }

    # flag to keep running the algorithm until it reaches the min number of features
    if (length(ftr_selection) >= min_features){
      flag_keep_running = FALSE
    }

    # break if the the algorithm got more than min_features and
    # additional features doesn't improve the result
    if (flag_keep_running == FALSE & flag_stop_running == TRUE){
      break
    }

    # break if in reaches the max_features
    if (length(ftr_selection) >= max_features){
      break
    }

    ftr_selection <- c(ftr_selection, selected_ftr)
    ftr_no_selection <- ftr_no_selection[-selected_ftr]
    best_scores_all <- c(best_scores_all, best_scores_iter)

    best_scores_iter <- c()
    selected_ftr <- c()
    best_scores_iter <- Inf
  }

  return(ftr_selection)
}
