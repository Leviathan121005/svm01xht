#' Plot Histogram of Data
#'
#' Plot a histogram to provide an overview of the data distribution.
#'
#' @param data A data frame.
#' @param column The column index or name of data to plot.
#'
#' @examples
#' data = data.frame(height = c(170, 190, 150, 160, 180), weight = c(70, 80, 85, 75, 70))
#' plot_hist(data, "height")
#'
#' @return A neat histogram plot of a column from data.
#'
#' @import ggplot2
#'
#' @export
plot_hist <- function(data, column = NULL) {
  # Input validations.
  assert_that(is.data.frame(data), msg = "data must be a data frame.")
  if (is.numeric(column)) {
    assert_that(column >= 1 && column <= ncol(data), msg = "column index is out of bounds.")
  }
  else if (is.character(column)) {
    assert_that(column %in% colnames(data), msg = "column does not exist in data")
  }
  else {
    stop("column must be a column name or column index.")
  }

  # Create a neat theme.
  title = paste("Histogram of", column, "\n")
  plot_theme = theme_minimal() + theme(plot.title = element_text(margin = margin(r = 24), hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(margin = margin(r = 14), size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(15, 20, 15, 20))
  xlab =  paste("\n", column, sep = "")

  # Create plots according to feature or label data type.
  col = data[[column]]
  if (is.character(col) || is.factor(col)) {
    ggplot(data, aes(x = .data[[column]])) +
      geom_bar(fill = "skyblue") +
      labs(title = title, x =  xlab , y = "Count") +
      plot_theme +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
  }
  else if (is.numeric(col)) {
    if (length(unique(col)) <= 10) {
      ggplot(data, aes(x = factor(.data[[column]]))) +
        geom_bar(fill = "skyblue") +
        labs(title = title, x = xlab , y = "Count") +
        plot_theme
    }
    else {
      ggplot(data, aes(x = .data[[column]])) +
        geom_histogram(bins = 25, fill = "skyblue", color = "white") +
        labs(title = title, x = xlab , y = "Frequency") +
        plot_theme
    }
  }
  else {
    stop("Unsupported column data type for plotting.")
  }
}