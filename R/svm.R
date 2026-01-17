#' SVM Model Training
#'
#' Train SVM binary classifier for numeric features data using quadratic programming. Supports linear, polynomial, and RBF kernel types.
#'
#' @param data A data frame of numerical features data.
#' @param label A label vector of 1 or -1 with respect to data.
#' @param n Number of data to use for training. Defaults to the number of rows in data.
#' @param features A vector of column names of features to use for training. Defaults to all column names.
#' @param type Kernel type to use for training. Only linear, polynomial, and RBF kernels are available. Defaults to linear.
#' @param C Regularization parameter for quadratic programming. Defaults to 1.
#' @param c Constant term for polynomial kernel. Defaults to 1.
#' @param degree Degree of the polynomial kernel. Defaults to 2.
#' @param gamma Parameter for RBF kernel. Defaults to 1 / number of features.
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' label = c(-1, -1, 1, 1, -1)
#' linear.model = svm(data, label)
#'
#' @return A list that represents the SVM model:
#' \describe{
#'   \item{w}{Weight vector for linear kernel.}
#'   \item{alpha}{Dual solution of the quadratic programming.}
#'   \item{X}{Scaled data used in training}
#'   \item{y}{Label used in training}
#'   \item{b}{Bias term.}
#'   \item{type}{Kernel type used in training}
#'   \item{features}{Features used in training.}
#'   \item{n}{Number of data used in training.}
#'   \item{c}{Constant used in the polynomial kernel.}
#'   \item{C}{Regularization parameter used in quadratic programming (dual).}
#'   \item{degree}{Degree used in the polynomial kernel.}
#'   \item{gamma}{Gamma used in the RBF kernel.}
#' }
#'
#' @import assertthat
#' @importFrom quadprog solve.QP
#'
#' @export
svm = function(data, label, n = nrow(data), features = names(data), type = "linear", C = 1,
               degree = 2, c = 1, gamma = 1 / length(features)) {
  # Input validations.
  assert_that(is.data.frame(data), msg = "data must be a data frame.")
  assert_that(all(label %in% c(-1, 1)), msg = "label must consist only of 1 and -1.")
  assert_that(length(label) == nrow(data), msg = "Number of data and label are not equal.")

  assert_that(is.numeric(n) && length(n) == 1 && n %% 1 == 0, msg = "n must be an integer.")
  if (n > nrow(data) || n <= 0) {
    n = nrow(data)
  }
  assert_that(all(features %in% colnames(data)), msg = "Some column names of selected features do not exist in data.")
  assert_that(all(sapply(data[, features], is.numeric)), msg = "Selected features must be numeric.")

  assert_that(type %in% c("linear", "polynomial", "rbf"), msg = "Only linear, polynomial, and rbf kernels are available.")

  assert_that(degree %% 1 == 0 && degree >= 1 && degree <= 5, msg = "Polynomial degree must be a positive integer from 1 to 5.")

  assert_that(C > 0 && c >= 0 && gamma > 0, msg = "Negative values are not allowed for C, c, and gamma.")

  # Adjust data and label.
  random.index = sample(1:nrow(data), size = n, replace = FALSE)
  X = data[random.index, features]
  X = scale(X)
  X[is.na(X)] = 0
  y = label[random.index]

  # Form quadratic programming inputs.
  Dmat = matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (type == "linear") {
        Dmat[i, j] = y[i] * y[j] * sum(X[i, ] * X[j, ])
      }
      else {
        Dmat[i, j] = y[i] * y[j] * kernel(X[i, ], X[j, ], type, degree, c, gamma)
      }
    }
  }
  if (type == "polynomial") {
    Dmat <- Dmat + 1e-6 * diag(n)
  }
  else {
    Dmat <- Dmat + 1e-8 * diag(n)
  }
  dvec = matrix(rep(1, n), byrow = TRUE)
  Amat = cbind(y, diag(n), -diag(n))
  bvec = c(rep(0, n + 1), rep(-C, n))

  # Wrap up the model.
  solution = solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  alpha = solution$solution
  support.index = which(alpha > 1e-8)
  if (type == "linear") {
    w = colSums(alpha * y * X)
    b = (sum(y[support.index]) - sum(w * colSums(X[support.index, ]))) / length(support.index)
    return (list(w = w, b = b, type = type, features = features))
  }
  else {
    b = 0
    for (j in support.index) {
      for (i in 1:n) {
        b = b + alpha[i] * y[i] * kernel(X[i, , drop = TRUE], X[j, , drop = TRUE], type, degree, c, gamma)
      }
    }
    b = b / length(support.index)
    return(list(alpha = alpha, X = X, y = y, b = b, type = type, features = features, n = n,
                C = C, c = c, degree = degree, gamma = gamma))
  }
}
