#' Test Model's Accuracy
#'
#' Compare predicted values with actual values to calculate model's accuracy.
#'
#' @param model An SVM model list object returned by \code{svm()}.
#' @param data A data frame of numerical features data.
#' @param label A label vector of 1 or -1 with respect to data.
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' label = c(-1, -1, 1, 1, -1)
#' linear.model = svm(data, label)
#' test.data = data.frame(height = c(175, 195, 155, 165, 185), weight = c(75, 85, 80, 70, 75))
#' test.label = data.frame(-1, -1, 1, -1, -1)
#' test_accuracy(linear.model, test.data, test.label)
#'
#' @return The level of accuracy from 0 to 1.
#'
#' @import assertthat
#'
#' @export
test_accuracy = function(model, data, label) {
  # Input validations (other assertions are passed to predict function).
  assert_that(is.list(model), msg = "model must be a list")

  assert_that(is.data.frame(data), msg = "data must be a data frame.")
  assert_that(all(sapply(data, is.numeric)), msg = "data features must be numeric.")

  assert_that(all(label %in% c(-1, 1)), msg = "label must consist only of 1 and -1.")
  assert_that(length(label) == nrow(data), msg = "Number of data and label are not equal.")

  return(mean(predict(model, data) == label))
}
