#' Visualize Hypothesis Testing Results
#'
#' Creates plots for visualizing type I error of Z-test and LRT, or type II error, but only for Z-test.
#'
#' @param test.result A list of hypothesis testing result from hypo_test().
#' @param type Type I or II error to plot. Type II error plot is only available for Z-test.
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' label = c(-1, -1, 1, 1, -1)
#' linear.model = svm(data, label)
#' test = hypo_test(linear.model, data, label)
#' visualize_ht(test)
#'
#' @return A plot showing either:
#' \itemize{
#'   \item For type = "I": The statistic and rejection region of Z-test or LRT.
#'   \item For type = "II": Power with respect to possible true alternatives.
#' }
#'
#' @import ggplot2
#' @import assertthat
#' @import stats
#'
#' @export
visualize_ht <- function(test.result, type = c("I", "II")) {
  # Input validations.
  assert_that(is.list(test.result), msg = "test.result must be a list.")
  assert_that(all(c("estimate", "n", "null.value", "alternative", "method", "alpha", "test.statistic", "p.value") %in% names(test.result)),
              all(sapply(test.result[!(names(test.result) %in% c("alternative", "method", "status", "conf.int"))], is.numeric)),
              all(sapply(test.result[c("alternative", "method", "status")], is.character)),
              msg = "test.result has invalid data type.")

  # Abbreviations for variable names.
  estimate = test.result$estimate
  n = test.result$n
  null.value = test.result$null.value
  alternative = test.result$alternative
  method = test.result$method
  alpha = test.result$alpha
  statistic = test.result$test.statistic
  p.value = test.result$p.value

  assert_that(all(sapply(test.result[!(names(test.result) %in% c("conf.int"))], length) == 1),
              all(c(null.value, alpha, p.value, n) >= 0),
              all(c(null.value, alpha, p.value) <= 1),
              all(alternative %in% c("two sided", "greater", "less")),
              all(method %in% c("z", "lrt")),
              msg = "test.result has invalid value(s).")
  type <- match.arg(type)

  plot_theme = theme_minimal() +
    theme(plot.title = element_text(margin = margin(r = 24), hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 14), size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin = margin(15, 20, 15, 20))

  if (type == "I") {
    if (method == "z") {
      # Setting plot values.
      lim = abs(statistic) + 1.5
      x.lim = c(-lim, lim)
      xy = data.frame(x = seq(x.lim[1], x.lim[2], length.out = 200))
      xy$y = dnorm(xy$x)

      # Plot Z-test results.
      ggplot(xy, aes(x = .data[["x"]], y = .data[["y"]])) +
        geom_line() +
        geom_vline(xintercept = statistic, color = "red", size = 1) +
        (if (alternative == "two sided") {
            list(
              geom_area(data = xy[which(xy$x > qnorm(1 - alpha / 2)), ], fill = "red", alpha = 0.5),
              geom_area(data = xy[which(xy$x < qnorm(alpha / 2)), ], fill = "red", alpha = 0.5)
            )
          }
         else if (alternative == "greater") {
          geom_area(data = xy[which(xy$x > qnorm(1 - alpha)), ], fill = "red", alpha = 0.5)
         }
         else if (alternative == "less") {
          geom_area(data = xy[which(xy$x < qnorm(alpha)), ], fill = "red", alpha = 0.5)
         }
        ) +
        labs(title = "Type I Error Visualization\n",
             x = "\nZ-value",
             y = "Density\n") +
        theme_minimal() +
        plot_theme
    }
    else if (method == "lrt") {
      # Setting plot values.
      lim = abs(statistic) + qchisq(1 - alpha, 1) + 3
      x.lim = c(0, lim)
      xy <- data.frame(x = seq(x.lim[1], x.lim[2], length.out = 200))
      xy$y <- dchisq(xy$x, 1)

      # Plot LRT results.
      ggplot(xy, aes(x = .data[["x"]], y = .data[["y"]])) +
        geom_line() +
        geom_vline(xintercept = statistic, color = "red", size = 1) +
        geom_area(data = xy[which(xy$x > qchisq(1 - alpha, 1)), ], fill = "red", alpha = 0.5) +
        labs(title = "Type I Error Visualization\n",
             x = "\nChi-value",
             y = "Density\n") +
        theme_minimal() +
        plot_theme
    }
  }
  else if (type == "II" && method == "z") {
      # Setting plot values.
      if (alternative == "two sided") {
        p.1 <- seq(0.01, 0.99, length.out = 99)
        numer.1 = qnorm(1 - alpha / 2) * sqrt(null.value * (1 - null.value) / n) - (p.1 - null.value)
        numer.2 = -qnorm(1 - alpha / 2) * sqrt(null.value * (1 - null.value) / n) - (p.1 - null.value)
        denom <- sqrt(p.1 * (1 - p.1) / n)
        power <- 1 - pnorm(numer.1 / denom) + pnorm(numer.2 / denom)
      }
      else if (alternative == "greater") {
        p.1 <- seq(null.value + 0.01, 0.99, length.out = 50)
        numer <- qnorm(1 - alpha) * sqrt(null.value * (1 - null.value) / n) - (p.1 - null.value)
        denom <- sqrt(p.1 * (1 - p.1) / n)
        z <- numer / denom
        power <- 1 - pnorm(z)
      }
      else if (alternative == "less") {
        p.1 <- seq(0.01, null.value, length.out = 50)
        numer <- -qnorm(1 - alpha) * sqrt(null.value * (1 - null.value) / n) - (p.1 - null.value)
        denom <- sqrt(p.1 * (1 - p.1) / n)
        z <- numer / denom
        power <- 1 - pnorm(z)
      }

      # Plot Z-test power vs true alternative
      ggplot(data.frame(p.1, power), aes(x = .data[["p.1"]], y = .data[["power"]])) +
        geom_line(color = "steelblue", size = 1.5) +
        geom_vline(xintercept = null.value, linetype = "dashed") +
        labs(title = "Power vs True Alternative\n",
             x = "\nTrue Alternative",
             y = "Power (1 - Type II Error)\n") +
        theme_minimal() +
        plot_theme
  }
  else {
    stop("Type II error can only be plotted for Z-test.")
  }
}
