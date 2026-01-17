#' Perform Hypothesis Testing on Model Accuracy
#'
#' Conducts a one-sample proportion Z-test or likelihood ratio test (LRT) to assess if the model's accuracy differs significantly from a
#' specified null value.
#'
#' @param model An SVM model list object returned by \code{svm()}.
#' @param data A data frame of numerical features data.
#' @param label A label vector of 1 or -1 with respect to data.
#' @param null.value Null hypothesis of accuracy value. Defaults to 0.8
#' @param alternative Type of alternative hypothesis. Two sided, greater, or less.
#' @param alpha Significance level. Defaults to 0.05
#' @param method Type of test to use. Z-test or LRT.
#'
#' @examples
#' data = data.frame(height = c(169, 179, 159, 165, 190), weight = c(70, 80, 85, 75, 70))
#' label = c(1, -1, 1, -1, -1)
#' model = svm(data, label)
#' hypo_test(model, data, label, null.value = 0.5, alternative = "less")
#'
#' @return A list containing:
#' \describe{
#'   \item{estimate}{Estimated accuracy of the model.}
#'   \item{n}{Sample size.}
#'   \item{null.value}{Null hypothesis value.}
#'   \item{alternative}{Type of alternative hypothesis.}
#'   \item{method}{Test method. Z-test or LRT.}
#'   \item{alpha}{Significance level.}
#'   \item{test.statistic}{Test statistic value.}
#'   \item{p.value}{P-value.}
#'   \item{status}{Rejection status of the null hypothesis. Reject or fail to reject.}
#'   \item{conf.int}{Confidence interval (only for Z-test).}
#' }
#'
#' @import assertthat
#' @import stats
#'
#' @export
hypo_test <- function(model, data, label, null.value = 0.8, alternative = c("two sided", "less", "greater"),
                      alpha = 0.05, method = c("z", "lrt")) {
  # Input Validations (other assertions are passed to test_accuracy function).
  assert_that(is.list(model), msg = "model must be a list.")

  assert_that(is.data.frame(data), msg = "data must be a data frame.")

  assert_that(all(label %in% c(-1, 1)), msg = "label must consist only of 1 and -1.")
  assert_that(length(label) == nrow(data), msg = "Number of data and label should match")

  assert_that(is.numeric(null.value) && length(null.value) == 1, null.value > 0, null.value < 1,
              msg = "null.value must be a number between 0 and 1.")

  alternative <- match.arg(alternative)

  assert_that(is.numeric(alpha) && length(alpha) == 1, alpha > 0 && alpha < 1,
              msg = "alpha must be a number between 0 and 1")

  method <- match.arg(method)

  # Calculate accuracy and sample size
  accuracy <- test_accuracy(model, data, label)
  n <- length(label)

  status = "Reject null hypothesis"

  if (method == "z") {
    sd <- sqrt(null.value * (1 - null.value) / n)
    z <- (accuracy - null.value) / sd

    if (alternative == "greater") {
      p.val <- pnorm(z, lower.tail = FALSE)
    }
    else if (alternative == "less") {
      p.val <- pnorm(z)
    }
    else {
      p.val <- 2 * pnorm(-abs(z))
    }

    # Confidence interval construction
    z_alpha <- -qnorm(alpha / 2)
    ci <- accuracy + c(-1, 1) * z_alpha * sqrt(accuracy * (1 - accuracy) / n)

    if (p.val > alpha) {
      status = "Fail to reject null hypothesis"
    }

    return(list(estimate = accuracy, n = n, null.value = null.value, alternative = alternative, method = method,
                alpha = alpha, test.statistic = z, p.value = p.val, status = status, conf.int = ci))
  }
  else if (method == "lrt") {
    p.hat = accuracy
    p.hat = min(max(p.hat, 1e-8), 1 - 1e-8)
    p.0 = null.value
    S = accuracy * n

    if (alternative == "greater") {
      p.hat.0 = min(p.hat, p.0)
    }
    else if (alternative == "less") {
      p.hat.0 = max(p.hat, p.0)
    }
    else {
      p.hat.0 = p.0
    }

    loglik.0 = S * log(p.hat.0) + (n - S) * log(1 - p.hat.0)
    loglik.0U1 =  S * log(p.hat) + (n - S) * log(1 - p.hat)
    lrt = 2 * max(0, loglik.0U1 - loglik.0)

    if (alternative == "less") {
      p.val <- ifelse(p.hat >= p.0, 1, 0.5 * pchisq(lrt, df = 1, lower.tail = FALSE))
    }
    else if (alternative == "greater") {
      p.val <- ifelse(p.hat <= p.0, 1, 0.5 * pchisq(lrt, df = 1, lower.tail = FALSE))
    }
    else {
      p.val <- pchisq(lrt, df = 1, lower.tail = FALSE)
    }

    if (p.val > alpha) {
      status = "Fail to reject null hypothesis"
    }

    return(list(estimate = accuracy, n = n, null.value = null.value, alternative = alternative, method = method,
                alpha = alpha, test.statistic = lrt, p.value = p.val, status = status, conf.int = "-"))
  }
}
