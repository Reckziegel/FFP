# Autoplot methods --------------------------------------------------------

#' Plot an FFP fit
#'
#' Creates a weighted empirical histogram for a selected scenario feature,
#' comparing the prior distribution with the full-confidence distribution
#' implied by Entropy Pooling.
#'
#' The scenarios themselves are unchanged by FFP. The plot therefore uses
#' common histogram bins for both distributions and changes only the
#' probability mass assigned to the scenarios within each bin.
#'
#' Histogram heights are probability densities. For a bin with width
#' \eqn{\Delta_k} and probability mass \eqn{m_k}, the plotted height is
#' \eqn{m_k / \Delta_k}.
#'
#' @param object An `ffp_fit` object.
#' @param feature A single character string identifying the numeric scenario
#'   feature to plot.
#' @param bins Optional positive integer giving the desired number of
#'   histogram bins. If `NULL`, the Freedman-Diaconis rule is used.
#' @param ... Reserved for future extensions. Must currently be empty.
#'
#' @return A `ggplot2` object.
#'
#' @seealso [ffp_probabilities()]
#'
#' @importFrom ggplot2 autoplot
#' @export
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(
#'     -0.08,
#'     -0.03,
#'     0.01,
#'     0.04,
#'     0.06
#'   )
#' )
#'
#' fit <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_uniform()
#'   ) |>
#'   ffp_view(
#'     view_mean(
#'       equity,
#'       target = 0.02
#'     )
#'   ) |>
#'   ffp_fit()
#'
#' ggplot2::autoplot(
#'   fit,
#'   feature = "equity"
#' )
#'
#' ggplot2::autoplot(
#'   fit,
#'   feature = "equity",
#'   bins = 4
#' )
autoplot.ffp_fit <- function(
    object,
    feature = NULL,
    bins = NULL,
    ...
) {
  rlang::check_dots_empty()

  plot_data <- prepare_distribution_plot_data(
    object = object,
    feature = feature,
    bins = bins,
    call = rlang::caller_env()
  )

  path_data <- distribution_histogram_path(
    plot_data
  )

  ggplot2::ggplot(
    path_data,
    ggplot2::aes(
      x = .data$x,
      y = .data$density,
      color = .data$distribution,
      group = .data$distribution
    )
  ) +
    ggplot2::geom_path() +
    ggplot2::labs(
      x = feature,
      y = "Density",
      color = "Distribution"
    )
}


# Histogram path ----------------------------------------------------------

distribution_histogram_path <- function(data) {
  distributions <- levels(
    data$distribution
  )

  purrr::map(
    distributions,
    \(distribution) {
      histogram <- data |>
        dplyr::filter(
          .data$distribution == .env$distribution
        ) |>
        dplyr::arrange(
          .data$bin
        )

      histogram_distribution_path(
        histogram = histogram,
        distribution = distribution,
        distribution_levels = distributions
      )
    }
  ) |>
    dplyr::bind_rows()
}


histogram_distribution_path <- function(
    histogram,
    distribution,
    distribution_levels
) {
  n_bins <- nrow(histogram)

  horizontal_x <- as.vector(
    rbind(
      histogram$lower,
      histogram$upper
    )
  )

  horizontal_density <- rep(
    histogram$density,
    each = 2L
  )

  tibble::tibble(
    feature = histogram$feature[[1]],
    distribution = factor(
      distribution,
      levels = distribution_levels
    ),
    x = c(
      histogram$lower[[1]],
      horizontal_x,
      histogram$upper[[n_bins]]
    ),
    density = c(
      0,
      horizontal_density,
      0
    )
  )
}

# Data preparation --------------------------------------------------------

prepare_probability_plot_data <- function(
    probabilities,
    x_values
) {
  probabilities |>
    dplyr::mutate(.ffp_x = x_values) |>
    tidyr::pivot_longer(
      cols = c("prior", "full_confidence"),
      names_to = "distribution",
      values_to = "probability"
    ) |>
    dplyr::mutate(
      distribution = factor(
        .data$distribution,
        levels = c(
          "prior",
          "full_confidence"
        ),
        labels = c(
          "Prior",
          "Full confidence"
        )
      )
    )
}


# Plot builders -----------------------------------------------------------

plot_temporal_probabilities <- function(data, x_label) {

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$.ffp_x,
      y = .data$probability,
      color = .data$distribution,
      group = .data$distribution
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = x_label,
      y = "Probability",
      color = "Distribution"
    )
}


plot_scenario_probabilities <- function(probabilities, data, x_values, x_label) {

  segment_data <- probabilities |>
    dplyr::transmute(
      .ffp_x = x_values,
      prior = .data$prior,
      full_confidence = .data$full_confidence
    )

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$.ffp_x,
      y = .data$probability
    )
  ) +
    ggplot2::geom_segment(
      data = segment_data,
      ggplot2::aes(
        x = .data$.ffp_x,
        xend = .data$.ffp_x,
        y = .data$prior,
        yend = .data$full_confidence
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = .data$distribution
      )
    ) +
    ggplot2::labs(
      x = x_label,
      y = "Probability",
      color = "Distribution"
    )
}


# Axis resolution ---------------------------------------------------------

resolve_autoplot_x <- function(probabilities, x, call = rlang::caller_env()) {

  has_index <- probability_table_has_index(probabilities)

  resolved_x <- if (identical(x, "auto")) {
    if (has_index) {
      "index"
    } else {
      "scenario"
    }
  } else {
    x
  }

  if (identical(resolved_x, "index") && !has_index) {
    ffp_abort(
      c(
        "The fitted scenarios do not contain an index.",
        "i" = "Use {.code x = \"scenario\"} to plot by scenario number."
      ),
      class = c(
        "ffp_error_missing_plot_index",
        "ffp_error_invalid_plot"
      ),
      call = call
    )
  }

  values <- probabilities[[resolved_x]]

  list(
    name = resolved_x,
    values = values,
    label = if (identical(resolved_x, "index")) {
      "Index"
    } else {
      "Scenario"
    },
    temporal = is_temporal_plot_index(values)
  )
}


probability_table_has_index <- function(probabilities) {

  if (!"index" %in% names(probabilities)) {
    return(FALSE)
  }

  !all(is.na(probabilities$index))

}


is_temporal_plot_index <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXlt"))
}


# Validation --------------------------------------------------------------

validate_autoplot_x <- function(x, call = rlang::caller_env()) {

  supported <- c("auto", "index", "scenario")

  valid <- is.character(x) &&
    is.null(dim(x)) &&
    length(x) == 1L &&
    !is.na(x) &&
    x %in% supported

  if (!valid) {
    ffp_abort(
      c(
        "{.arg x} must identify the horizontal axis.",
        "i" = paste0(
          "Supported values are {.val auto}, {.val index}, and ",
          "{.val scenario}."
        )
      ),
      class = c(
        "ffp_error_invalid_plot_x",
        "ffp_error_invalid_plot"
      ),
      call = call
    )
  }

  x
}
