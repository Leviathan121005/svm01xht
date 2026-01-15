test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)
test.test.data = data.frame(height = c(150, 160, 170, 180, 190),
                            weight = c(80, 60, 70, 90, 70))
test.test.label = c(1, -1, -1, 1, -1)

test_that("tune_parameters() returns a comparison data frame according to number of parameters to tune and kernel types.", {
  linear.tuning = tune_parameters(test.data, test.label, test.test.data, test.test.label, type = "linear", C = c(1, 10))
  poly.tuning = tune_parameters(test.data, test.label, test.test.data, test.test.label, type = "polynomial", C = c(1, 10), c = c(1, 2), degree = c(2, 3))
  rbf.tuning = tune_parameters(test.data, test.label, test.test.data, test.test.label, type = "rbf", C = c(1, 10, 100), gamma = c(0.05, 0.1))

  expect_equal(dim(linear.tuning), c(2, 3))
  expect_equal(dim(poly.tuning), c(8, 5))
  expect_equal(dim(rbf.tuning), c(6, 4))

  expect_equal(all(c("C", "training.accuracy", "test.accuracy") %in% names(linear.tuning)), TRUE)
  expect_equal(all(c("C","c", "degree", "training.accuracy", "test.accuracy") %in% names(poly.tuning)), TRUE)
  expect_equal(all(c("C", "gamma", "training.accuracy", "test.accuracy") %in% names(rbf.tuning)), TRUE)
})
