#' Inspection of a `ffp` object with ggplot2
#'
#' This functions extends the `autoplot` method for the `ffp` class.
#'
#' @param object An objected of the class `ffp`.
#' @param color A \code{logical} indicating whether or not the `color` argument
#' should be added to the `ggplot2` aesthetics.
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
#'
#' x <- smoothing(EuStockMarkets, 0.001)
#' y <- smoothing(EuStockMarkets, 0.01)
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
