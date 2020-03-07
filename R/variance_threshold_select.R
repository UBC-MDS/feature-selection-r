#' Select features based on variance threshold
#'
#' @description Find out about the variance of each feature and
#' filter out the ones below a certain threshold
#'
#' @param data tibble. The features to select from
#' @param threshold double. The variance threshold
#'
#' @return vector. The indexes of selected features
#' @export
#' @examples
#' variance_threshold_select(
#'   tibble(x1=c(1,2,3,4,5), x2=c(0,0,0,0,0), x3=c(1,1,1,1,1))
#' )
variance_threshold_select <- function(data, threshold = 0) {
  if (!any(class(data) == "data.frame")) {
    stop("Expected a `data.frame` object for `data`.")
  }

  if (threshold < 0 | !is.numeric(threshold)) {
    stop("Threshold must be a positive number.")
  }

  selected_feature_indexes <- c()
  i <- 1
  for (name in names(data)) {
    var_i <- var(data[,i])

    if (var_i > threshold) {
      selected_feature_indexes <- c(selected_feature_indexes, i)
    }

    i <- i + 1
  }

  selected_feature_indexes
}
