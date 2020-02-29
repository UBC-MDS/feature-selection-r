#' Select features using simmulated annealing
#' 
#' @description The algorithm randomly chooses a set of features, trains on them, scores the model. 
#' Then the algorithm slightly modifies the chosen features randomly and tests to see
#' if the model improves. If there is improvement, the newer model is kept, if not the 
#' algorithm tests to see if the worse model is still kept based on a acceptance 
#' probability that decreases as iterations continue and if the model performs worse.
#' 
#' @param model supervised learning model 
#' @param X data frame of training features
#' @param y data frame of training targets
#' @param c control rate of feature perturbation
#' @param iterations number of iterations
#' @param random_state random state of pseudo-random number generators
#' 
#' @return vector of selected features
#' 
simulated_annealing_select <- function(model, X, y, c = 1, iterations = 100, random_state = NULL) {
}
