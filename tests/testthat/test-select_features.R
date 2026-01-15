test.data = data.frame(height = c(145, 155, 165, 175, 185),
                       weight = c(50, 70, 80, 60, 90))
test.label = c(-1, 1, 1, -1, 1)

test_that("select_features() returns a vector of column names from column names of data.", {
  selected.features = select_features(test.data, test.label)

  expect_equal(is.character(selected.features), TRUE)
  expect_equal(all(selected.features %in% names(test.data)), TRUE)
})
