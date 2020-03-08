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

forward_select <- function(scorer, X, y, min_features=2, max_features=10) {
  # Is `scorer` a function?
  if (class(scorer) != "function") {
    stop("Expected a function for `scorer`." )
  }
  
  # Do we have a data.frame or something compatible like tibble?
  if (!any(class(X) == "data.frame")) {
    stop("Expected a `data.frame` object for `data`.")
  }
  
  # Obtain initial array of randomly selected features
  ftr_no_select <- c(1:length(X))
  ftr_select <- c()
  fn_score = c()#Inf
  best_ever = Inf

  for (j in max_features){
    for (i in ftr_no_select){
      X_new = X[c(ftr_select, i)]
      fn_score[i] <- scorer(cbind(X_new, y))
    }
    
    best_one = min(fn_score)
    best_ever = min(best_one, best_ever)
  }
  
  return(best_one)
}  


# # data
# X1 <- rnorm(100, 3, 41)
# X2 <- rnorm(100, 4, 13)
# X3 <- rnorm(100, 6, 1)
# X4 <- rnorm(100, 8, 11)
# X5 <- rnorm(100, 99, 15)
# X6 <- rnorm(100, 1, 14)
# X7 <- rnorm(100, 5, 11)
# Y <- rnorm(100, 3, 122)
# Z <- data.frame(X1, X2,X3, X4,X5, X6, X7)

# # scorer
# my_scorer <- function(data) {
#   model <- lm(Y ~ ., data)
#   return(mean(model$residuals^2))
# }

# #testing
# forward_select(my_scorer, Z, Y, 2, 5)

simulated_annealing <- function(scorer, X, y, c = 1, iterations = 100, bools = FALSE) {
    
    # Is `scorer` a function?
    if (class(scorer) != "function") {
    stop("Expected a function for `scorer`." )
    }

    # Do we have a data.frame or something compatible like tibble?
    if (!any(class(X) == "data.frame")) {
        stop("Expected a `data.frame` object for `data`.")
    }

    # Set mutate percentage
    mutate <- 0.05
    
    # Obtain initial array of randomly selected features
    ftr_all <- c(1:length(X))
    ftr_old <- c()
    while (length(ftr_old) == 0){
        ftr_old <- (rbinom(length(X), 1, 0.5) != 0)
    }
    score_old <- scorer(cbind(X[ftr_old], y))
    
    # Iterate through new versions of selected features
    for (i in 1:iterations){
        ftr_new <- ftr_old
        ftr_mutate <- sample(c(1:length(X)), size = ceiling(length(ftr_all)*mutate), replace = FALSE)
        for (f in ftr_mutate) {
            ftr_new[f] <- !ftr_new[f]
        }
        # Make sure new selected features has a least one feature
        if (sum(ftr_new) != 0){
            score_new <- scorer(cbind(X[ftr_new], y))
            if (score_new < score_old){
                ftr_old <- ftr_new
                score_old <- score_new
            } else {
                # Determine probability of acceptance
                p_accept <- exp((-i/c)*((score_new - score_old)/score_old))
                if (runif(1) > p_accept){
                } else {
                    ftr_old <- ftr_new
                    score_old <- score_new
                }
            }
        }
    }
    
    # Return either feature indicies or booleans
    if (bools){
        return(ftr_old)
    } else {
        return(ftr_all[ftr_old])
    }
}
