test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)

test_that("svm() returns a list with appropriate values.", {
  linear.model = svm(test.data, test.label)
  poly.model = svm(test.data, test.label, type = "polynomial")
  rbf.model = svm(test.data, test.label, type = "rbf")

  # The output validation of svm() are passed to the input validation of predict().
  expect_no_error(predict(linear.model, test.data))
  expect_no_error(predict(poly.model, test.data))
  expect_no_error(predict(rbf.model, test.data))
})
