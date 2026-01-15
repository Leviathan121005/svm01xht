test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)
test.test.data = data.frame(height = c(150, 160, 170, 180, 190),
                            weight = c(80, 60, 70, 90, 70))
test.test.label = c(1, -1, -1, 1, -1)

test_that("visualize_ht() returns a plot with appropriate labels.", {
  linear.model = svm(test.data, test.label)
  linear.model.z.test = hypo_test(linear.model, test.test.data, test.test.label, 0.5)
  linear.model.lrt = hypo_test(linear.model, test.test.data, test.test.label, 0.5, method = "lrt")
  visualize_ht.z.plot <- visualize_ht(linear.model.z.test)
  visualize_ht.z.plot.2 <- visualize_ht(linear.model.z.test, type = "II")
  visualize_ht.lrt.plot <- visualize_ht(linear.model.lrt)

  expect_s3_class(visualize_ht.z.plot, "ggplot")
  expect_s3_class(visualize_ht.z.plot.2, "ggplot")
  expect_s3_class(visualize_ht.lrt.plot, "ggplot")

  expect_equal(visualize_ht.z.plot$labels$x, "\nZ-value")
  expect_equal(visualize_ht.z.plot$labels$y, "Density\n")
  expect_equal(visualize_ht.z.plot$labels$title, "Type I Error Visualization\n")

  expect_equal(visualize_ht.z.plot.2$labels$x, "\nTrue Alternative")
  expect_equal(visualize_ht.z.plot.2$labels$y,"Power (1 - Type II Error)\n")
  expect_equal(visualize_ht.z.plot.2$labels$title, "Power vs True Alternative\n")

  expect_equal(visualize_ht.lrt.plot$labels$x, "\nChi-value")
  expect_equal(visualize_ht.lrt.plot$labels$y, "Density\n")
  expect_equal(visualize_ht.lrt.plot$labels$title, "Type I Error Visualization\n")
})
