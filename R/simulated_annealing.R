#' Feature selector that performs simmulated annealing to select features.
#' 
#' @description Algorithm randomly chooses a set of features, trains on them, scores the model.
#'Then the algorithm slightly modifies the chosen features randomly and tests to see
#'if the model improves. If there is improvement, the newer model is kept, if not the 
#'algorithm tests to see if the worse model is still kept based on a acceptance 
#'probability that decreases as iterations continue and if the model performs worse.
#' 
#' @param scorer  A custom user-supplied function that accepts X and y (as defined below) as input and returns the error of the datasets.
#' @param X data frame of training features
#' @param y data frame of training targets
#' @param c control rate of feature perturbation
#' @param iterations number of iterations
#' @param bools If true function returns array of boolean values instead of column indicies
#' @param random_state Seed for random number generators
#' 
#' @return array of selected features
#' @export
#' 
#' @examples
#' custom_scorer_fn <- function(data) {
#'  model <- lm(Y ~ ., data)
#'  return(mean(model$residuals^2))
#' }
#' df <- tgp::friedman.1.data()
#' data <- dplyr::select(df, -Ytrue)
#' X <- data[1:(length(data)-1)]
#' y <- data[length(data)]
#' features <- featureselection::simulated_annealing(custom_scorer_fn, X, y)
#' 

simulated_annealing <- function(scorer, X, y, c = 1, iterations = 100, bools = FALSE, random_state=NULL) {
    
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

    # Set random state
    set.seed(random_state)
    
    # Obtain initial array of randomly selected features
    ftr_all <- c(1:length(X))
    ftr_old <- c()
    while (length(ftr_old) == 0){
        ftr_old <- (stats::rbinom(length(X), 1, 0.5) != 0)
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
                if (stats::runif(1) > p_accept){
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

