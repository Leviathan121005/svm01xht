#' Kernel Function
#'
#' Calculate value for polynomial and RBF kernel.
#'
#' @param Xi First vector.
#' @param Xj Second vector.
#' @param type Kernel type polynomial or RBF.
#' @param degree Degree of the polynomial kernel.
#' @param c A constant for polynomial kernel.
#' @param gamma Parameter for RBF kernel.
#'
#' @return A kernel value.
#'
#' @keywords internal
kernel = function(Xi, Xj, type, degree = 2, c = 1, gamma = 1) {
  if (type == "polynomial") {
    return((c + sum(Xi * Xj))^degree)
  }
  else if (type == "rbf") {
    val = exp(-gamma * sum((Xi - Xj)^2))
    return(val)
  }
}