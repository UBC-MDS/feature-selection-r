set.seed(0)
df <- tgp::friedman.1.data()

scorer <- function(data) {
  model <- lm(Y ~ ., data)
  names(which.min(model$coefficients[-1]))[[1]]
}

test_that("it works", {
  data <- dplyr::select(df, -Ytrue)
  features <- recursive_feature_elimination(scorer, data, n_features_to_select = 4)
  expect_identical(features, c("X1", "X2", "X4", "X5", "Y"))
})
