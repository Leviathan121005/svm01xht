test.data = data.frame(name = c("Ann", "Bob", "Charlie", "Dom", "Eve"),
                       height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))

test_that("plot_hist() returns a plot with appropriate labels.", {
  plot_hist.name <- plot_hist(test.data, "name")

  expect_s3_class(plot_hist.name, "ggplot")

  expect_equal(plot_hist.name$labels$x, "\nname")
  expect_equal(plot_hist.name$labels$y, "Count")
  expect_equal(plot_hist.name$labels$title, "Histogram of name \n")
})
