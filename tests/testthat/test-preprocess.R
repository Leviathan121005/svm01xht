test.data = data.frame(name = c("Ann", "Bob", "Charlie", "Dom", "Eve"),
                       height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)

test_that("preprocess() returns a clean numerical data frame.", {
  preprocessed.data = preprocess(test.data)

  # The output validation of preprocess() are passed to the input validation of svm().
  expect_no_error(svm(preprocessed.data, test.label))
})
