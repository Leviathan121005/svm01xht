#' Predict Label
#'
#' Classify data based on the the trained SVM model's kernel, features, and variables.
#'
#' @param model An SVM model list object returned by \code{svm()}.
#' @param data A data frame of numerical features data.
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' label = c(-1, -1, 1, 1, -1)
#' linear.model = svm(data, label)
#' test.data = data.frame(height = c(175, 195, 155, 165, 185), weight = c(75, 85, 80, 70, 75))
#' test.label = data.frame(-1, -1, 1, -1, -1)
#' predict(linear.model, test.data)
#'
#' @return A vector of -1 and 1 that represents the label prediction.
#'
#' @import assertthat
#'
#' @export
predict = function(model, data) {
  # Input validations.
  assert_that(is.list(model), msg = "model must be a list.")
  assert_that(model$type %in% c("linear", "polynomial", "rbf"), msg = "Invalid model type.")
  if (model$type == "linear") {
    assert_that(all(c("w", "b", "type", "features") %in% names(model)),
                is.numeric(model$w),
                is.numeric(model$b) && length(model$b) == 1,
                is.character(model$features),
                length(model$w) == length(model$features),
                msg = "Invalid linear model.")
  }
  else {
    assert_that(all(c("alpha", "X", "y", "b", "type", "features", "n", "C", "c", "degree", "gamma") %in% names(model)),
                all(sapply(model[!(names(model) %in% c("X", "type", "features"))], is.numeric)),
                all(sapply(model$X, is.numeric)),
                is.character(model$features),
                msg = "Kernel model has invalid data type.")
    assert_that(all(model$n == c(nrow(model$X), length(model$alpha), length(model$y))),
                ncol(model$X) == length(model$features),
                all(1 == c(length(model$b), length(model$n), length(model$C), length(model$c), length(model$degree), length(model$gamma))),
                all(c(model$C, model$c, model$degree, model$gamma) > 0),
                model$degree %% 1 == 0 && model$degree <= 5,
                msg = "Kernel model has invalid dimension or values.")
  }

  assert_that(is.data.frame(data), msg = "data must be a data frame")
  assert_that(all(sapply(data, is.numeric)), msg = "data features must be numeric.")

  # Set selected features.
  used_features = c()
  used_features = intersect(model$features, names(data))
  if (length(used_features) == 0) {
    stop("No matching features were found between the model and data.")
  }
  data = data[, used_features]

  # Adjust data.
  data = scale(data)
  data[is.na(data)] = 0

  # Calculate predictions.
  prediction = c()
  for (i in 1:nrow(data)) {
    predictor = model$b
    if (model$type == "linear") {
      predictor = predictor + sum(model$w[used_features] * data[i, ])
    }
    else {
      for (k in 1:model$n) {
        predictor = predictor + model$alpha[k] * model$y[k] *
          kernel(model$X[k, used_features], data[i, ], model$type, model$degree, model$c, model$gamma)
      }
    }
    sign = 1
    if (predictor < 0) {
      sign = -1
    }
    prediction = c(prediction, sign)
  }
  return(prediction)
}
