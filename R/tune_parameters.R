#' Tune SVM Parameters
#'
#' Create several SVM models with same training data and test data, but different parameters to determine better parameters.
#'
#' @param training.data A data frame of numerical features data to be used for training.
#' @param training.label A label vector of 1 or -1 with respect to training.data.
#' @param test.data A data frame of numerical features data to be used for testing.
#' @param test.label A label vector of 1 or -1 with respect to test.data.
#' @param features A vector of column names of features to use for training. Defaults to all column names.
#' @param type Kernel type to use for training. Only linear, polynomial, and RBF kernels are available. Defaults to linear.
#' @param C Regularization parameters (vector) for quadratic programming to test. Defaults to 1.
#' @param c Constants (vector) for polynomial kernel to test. Defaults to 1.
#' @param degree Degrees (vector) of the polynomial kernel to test. Defaults to 2.
#' @param gamma Parameters (vector) for RBF kernel to test. Defaults to 1 / number of features.
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' test.data = data.frame(height = c(175, 195, 155, 165, 185), weight = c(75, 85, 80, 70, 75))
#' label = c(-1, -1, 1, 1, -1)
#' test.label = c(-1, -1, 1, 1, -1)
#' tune_parameters(data, label, test.data, test.label, type = "linear", C = c(0.1, 1, 10))
#'
#' @return A data frame showing the of training accuracy and test accuracy for each combination of parameters.
#'
#' @import assertthat
#'
#' @export
tune_parameters = function(training.data, training.label, test.data, test.label,
                          features = names(training.data), type = "linear", C = 1,
                          degree = 2, c = 1, gamma = 1 / length(features)) {
  # Other assertions are passed to other functions.
  assert_that(type %in% c("linear", "polynomial", "rbf"), msg = "Invalid model type.")

  if (type == "linear") {
    result = data.frame(C = double(), training.accuracy = double(), test.accuracy = double())
    for (i in 1:length(C)) {
      model = svm(training.data, training.label, features = features, type = "linear", C =  C[i])
      result[i, ] = c(C[i], test_accuracy(model, training.data, training.label), test_accuracy(model, test.data, test.label))
    }
    return(result)
  }
  else if (type == "polynomial") {
    result = data.frame(C = double(), c = double(), degree = integer(), training.accuracy = double(), test.accuracy = double())
    for (i in 1:length(C)) {
      for (j in 1:length(c)) {
        for (k in 1:length(degree)) {
          model = svm(training.data, training.label, features = features,
                      type = "polynomial", C =  C[i], c = c[j], degree = degree[k])
          result = rbind(result, data.frame(
            C = C[i], c = c[j], degree = degree[k],
            training.accuracy = test_accuracy(model, training.data, training.label),
            test.accuracy = test_accuracy(model, test.data, test.label)))
        }
      }
    }
    return(result)
  }
  else if (type == "rbf") {
    result = data.frame(C = double(), gamma = double(), training.accuracy = double(), test.accuracy = double())
    for (i in 1:length(C)) {
      for (j in 1:length(gamma)) {
        model = svm(training.data, training.label, features = features,
                      type = "rbf", C =  C[i], gamma = gamma[j])
        result = rbind(result, data.frame(
          C = C[i], gamma = gamma[j],
          training.accuracy = test_accuracy(model, training.data, training.label),
          test.accuracy = test_accuracy(model, test.data, test.label)))
      }
    }
    return(result)
  }
}
