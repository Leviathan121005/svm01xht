#' Kernel Function
#'
#' Calculate value for polynomial and RBF kernel.
#'
#' @param Xi First input vector.
#' @param Xj Second input vector.
#' @param type Kernel type. Polynomial or RBF.
#' @param degree Degree of the polynomial kernel.
#' @param c Constant term for the polynomial kernel.
#' @param gamma Parameter for the RBF kernel.
#'
#' @return The kernel value.
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
