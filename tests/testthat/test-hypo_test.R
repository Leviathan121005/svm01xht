test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)
test.test.data = data.frame(height = c(150, 160, 170, 180, 190),
                            weight = c(80, 60, 70, 90, 70))
test.test.label = c(1, -1, -1, 1, -1)

test_that("hypo_test() returns a list with appropriate values.", {
  linear.model = svm(test.data, test.label)
  linear.model.z.test = hypo_test(linear.model, test.test.data, test.test.label, 0.5)
  linear.model.lrt = hypo_test(linear.model, test.test.data, test.test.label, 0.5, method = "lrt")

  # The output validation of hypo_test() are passed to the input validation of visualize_ht().
  expect_no_error(visualize_ht(linear.model.z.test))
  expect_no_error(visualize_ht(linear.model.lrt))
})

