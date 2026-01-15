#' Test Model's Accuracy
#'
#' Compare predicted values with actual values to evaluate accuracy of a model.
#'
#' @param model A list that represents an SVM model from svm().
#' @param data A data frame of numerical features data.
#' @param label A label vector of 1 or -1 with respect to data.
#'
#' @examples
#' model = list(w = c(-1.58, 1.3), b = 0.2, type = "linear", features = c("height", "weight"))
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' names(model$w) = c("height", "weight")
#' label =  c(-1, -1, 1, 1, -1)
#' test_accuracy(model, data, label)
#'
#' @return A number between 0 and 1 that represents level of accuracy.
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