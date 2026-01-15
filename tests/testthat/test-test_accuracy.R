test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)

test_that("test_accuracy() returns a number between 0 and 1.", {
  linear.model = svm(test.data, test.label)
  accuracy = test_accuracy(linear.model, test.data, test.label)

  expect_equal(is.numeric(accuracy), TRUE)
  expect_equal(accuracy >= 0 && accuracy <= 1, TRUE)
})
