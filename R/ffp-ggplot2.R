#' Quick Inspection of a `ffp` object with ggplott2
#'
#' This functions extends the `autoplot` method for the `ffp` class.
#'
#' @param object An objected of the class `ffp`.
#' @param ... Currently not used.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
#'
#' @importFrom ggplot2 autoplot
#' @rdname autoplot
#'
#' @examples
#' library(ggplot2)
#' x <- smoothing(EuStockMarkets, 0.001)
#' y <- smoothing(EuStockMarkets, 0.01)
#' autoplot(x)
#' autoplot(y)
autoplot.ffp <- function(object, ...) {
  # stopifnot(inherits(object, "ffp"))
  plot_data <- tibble::tibble(id = 1:vctrs::vec_size(object), y = as.double(object))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$id, y = .data$y)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(labels = scales::percent_format())
}

#' @rdname autoplot
#' @importFrom graphics plot
plot.ffp <- function(object, ...) {
  print(ggplot2::autoplot(object, ...))
}
