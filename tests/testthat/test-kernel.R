test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)

test_that("kernel() returns a number.", {
  scaled.test.data = scale(test.data)
  kernel.value = kernel(scaled.test.data[1, ], scaled.test.data[2, ], type = "polynomial")

  expect_equal(is.numeric(kernel.value), TRUE)
})
