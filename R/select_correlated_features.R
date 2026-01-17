#' Select Correlated Features
#'
#' Choose features with higher correlations with label.
#'
#' @param data A data frame of numerical features data.
#' @param label A label vector of 1 or -1 with respect to data.
#' @param n Number of best features to select.
#' @param show Print the sorted absolute correlations.
#' @param min Minimum correlation for a feature to be selected. Defaults to 0.3
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' label = c(-1, -1, 1, 1, -1)
#' select_correlated_features(data, label, 1)
#'
#' @return
#' A vector of n most meaningful features, or a vector of features with absolute correlation above 0.2 with the label, if n is not specified.
#'
#' @import assertthat
#' @import stats
#'
#' @export
select_correlated_features = function(data, label, n = NULL, min = 0.3, show = FALSE) {
  # Validate input
  assert_that(is.data.frame(data), msg = "data must be a data frame")
  assert_that(all(sapply(data, is.numeric)), msg = "data features must be numeric.")

  assert_that(all(label %in% c(-1, 1)), msg = "label must consist only of 1 and -1.")
  assert_that(length(label) == nrow(data), msg = "Number of data and label are not equal.")

  assert_that(is.numeric(min) && min > 0 && min < 1, msg = "min must be a number between 0 and 1")

  assert_that(is.logical(show), msg = "show must be TRUE or FALSE.")

  # Find the correlations between features and label.
  correlations = sapply(data, function(x) {
    if (sd(x) == 0) {
      return(0)
    }
    else {
      return(abs(cor(x, label)))
    }
  })
  correlations[is.na(correlations)] = 0

  if (show) {
    print(-sort(-correlations))
  }

  # Select n top features, or features with at least weak correlations.
  if (is.null(n)) {
    if (sum(correlations >= min) > 0) {
      return(names(correlations)[correlations >= min])
    }
    else {
      return(names(correlations))
    }
  }
  else {
    if ((n > ncol(data) || n < 0) || n %% 1 != 0) {
      n = ncol(data)
    }
    correlations = -sort(-correlations)
    return(names(correlations)[1:n])
  }
}
