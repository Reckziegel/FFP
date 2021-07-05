#' Inspection of a `ffp` object with ggplot2
#'
#' Extends the `autoplot` method for the `ffp` class.
#'
#' @param object An objected of the `ffp` class.
#' @param color A \code{logical} flag indicating whether (or not) the `color` argument
#' should be added to the `ggplot2` aesthetics.
#' @param ... Additional arguments to be passed to `autoplot`.
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
#'
#' x <- exp_smoothing(EuStockMarkets, 0.001)
#' y <- exp_smoothing(EuStockMarkets, 0.01)
#'
#' autoplot(x) +
#'   scale_color_viridis_c()
#' autoplot(y) +
#'   scale_color_viridis_c()
autoplot.ffp <- function(object, color = TRUE, ...) {
  # stopifnot(inherits(object, "ffp"))
  plot_data <- tibble::tibble(id = 1:vctrs::vec_size(object), y = as.double(object))
  ggplot2::ggplot(
    data    = plot_data,
    mapping = ggplot2::aes(
      x = .data$id,
      y = .data$y,
      color = if (color) .data$y)
    ) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent_format())
}

#' @rdname autoplot
#' @importFrom graphics plot
plot.ffp <- function(object, ...) {
  print(ggplot2::autoplot(object, ...))
}
