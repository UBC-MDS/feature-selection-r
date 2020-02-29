#' Select features based on feature selection
#'
#' @description Select features using forward selection algorithm. 
#'   It starts as an empty model, and add the variable with the  
#'   highest improvement in the accuracy of the model. The process
#'   is iteratively repeated and it stops when the remaining variables 
#'   doesn't improve the accuracy of the model.
#'
#' @param X tibble. training dataset
#' @param y tibble. test dataset
#' @param min_features double. number of minimum features to select
#' @param max_features double. number of maximum features to select
#'
#' @return vector. The indexes of selected features
#' @export
#' @examples
#' forward_select(
#'   train_data, test_data, max_features=5
#' )
forward_select <- function(train_data, test_data, min_features=2, max_features=10) {
}