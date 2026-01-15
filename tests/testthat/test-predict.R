test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)

test_that("predict() returns a vector of -1 and 1.", {
  linear.model = svm(test.data, test.label)

  expect_equal(all(predict(linear.model, test.data) %in% c(-1, 1)), TRUE)
})

