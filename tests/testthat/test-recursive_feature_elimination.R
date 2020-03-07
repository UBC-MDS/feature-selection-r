scorer <- function() {

}

test_that("it works", {
  features <- recursive_feature_elimination(scorer, X, y, n_features_to_select = 4)
  expect_identical(features, c())
})
