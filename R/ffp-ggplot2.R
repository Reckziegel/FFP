# Plot helpers ------------------------------------------------------------


.ffp_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13,
        color = "#1A1A1A",
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10,
        color = "#555555",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = 8,
        color = "#888888",
        hjust = 0
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = "#EEEEEE",
        linewidth = 0.2
      ),
      axis.title = ggplot2::element_text(
        size = 10,
        color = "#333333"
      ),
      axis.text = ggplot2::element_text(
        size = 9,
        color = "#666666"
      ),
      legend.position = "right",
      plot.margin = ggplot2::margin(10, 15, 10, 15)
    )
}


.ffp_annotation_theme <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 13,
      color = "#1A1A1A",
      margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 10,
      color = "#555555",
      margin = ggplot2::margin(b = 8)
    ),
    plot.caption = ggplot2::element_text(
      size = 8,
      color = "#888888",
      hjust = 0,
      margin = ggplot2::margin(t = 5)
    )
  )
}


.ffp_effective_scenarios <- function(p) {
  1 / sum(p^2)
}


.ffp_validate_probabilities <- function(p, label = "Probabilities") {
  if (!length(p)) {
    stop(
      sprintf("%s cannot be empty.", label),
      call. = FALSE
    )
  }

  if (any(!is.finite(p))) {
    stop(
      sprintf("%s must contain only finite values.", label),
      call. = FALSE
    )
  }

  if (any(p < 0)) {
    stop(
      sprintf("%s cannot contain negative values.", label),
      call. = FALSE
    )
  }

  total <- sum(p)

  if (!isTRUE(all.equal(total, 1, tolerance = 1e-8))) {
    stop(
      sprintf(
        "%s must sum to 1. Current sum: %.12f.",
        label,
        total
      ),
      call. = FALSE
    )
  }

  invisible(p)
}


.ffp_prior_data <- function(prior) {
  if (inherits(prior, "ffp")) {
    return(as.numeric(vctrs::vec_data(prior)))
  }

  as.numeric(prior)
}


.ffp_largest_shifts <- function(data, n = 3L) {
  if (n <= 0L || !nrow(data)) {
    return(data[0, , drop = FALSE])
  }

  positive <- which(
    is.finite(data$change) &
      data$change > 0
  )

  negative <- which(
    is.finite(data$change) &
      data$change < 0
  )

  if (length(positive)) {
    positive <- positive[
      order(
        data$change[positive],
        decreasing = TRUE
      )
    ]

    positive <- head(positive, n)
  }

  if (length(negative)) {
    negative <- negative[
      order(
        data$change[negative],
        decreasing = FALSE
      )
    ]

    negative <- head(negative, n)
  }

  idx <- c(positive, negative)

  out <- data[idx, , drop = FALSE]

  out$label_vjust <- ifelse(
    out$change > 0,
    -0.8,
    1.6
  )

  out
}


# Distribution plot -------------------------------------------------------


.ffp_plot_distribution <- function(object, color = TRUE) {
  p_data <- as.numeric(vctrs::vec_data(object))

  plot_data <- tibble::tibble(
    id = seq_along(p_data),
    probability = p_data
  )

  mean_prob <- mean(p_data)
  median_prob <- median(p_data)
  sd_prob <- stats::sd(p_data)

  if (color) {
    p <- ggplot2::ggplot(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$id,
        y = .data$probability,
        color = .data$probability
      )
    ) +
      ggplot2::geom_line(
        linewidth = 0.6,
        alpha = 0.8
      ) +
      ggplot2::scale_color_gradient(
        low = "#3498DB",
        high = "#E74C3C",
        labels = scales::percent_format(
          accuracy = 0.01
        ),
        guide = ggplot2::guide_colorbar(
          title = "Probability"
        )
      )

    caption <- "Red: High probability | Blue: Low probability"
  } else {
    p <- ggplot2::ggplot(
      data = plot_data,
      mapping = ggplot2::aes(
        x = .data$id,
        y = .data$probability
      )
    ) +
      ggplot2::geom_line(
        linewidth = 0.6,
        alpha = 0.8,
        color = "#2E86AB"
      )

    caption <- NULL
  }

  p +
    ggplot2::geom_vline(
      xintercept = which.max(p_data),
      linetype = "dashed",
      color = "#2E3440",
      alpha = 0.4,
      linewidth = 0.5
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(
        accuracy = 0.01
      ),
      expand = ggplot2::expansion(
        mult = c(0.05, 0.10)
      )
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    ggplot2::labs(
      title = "Flexible Forward Probabilities",
      subtitle = sprintf(
        paste0(
          "Mean: %.2f%% | ",
          "Median: %.2f%% | ",
          "Std Dev: %.2f%%"
        ),
        mean_prob * 100,
        median_prob * 100,
        sd_prob * 100
      ),
      x = "Scenario Index",
      y = "Probability",
      caption = caption
    ) +
    .ffp_plot_theme()
}


# Comparison plot ---------------------------------------------------------


.ffp_plot_comparison <- function(
    object,
    prior,
    change = c("relative", "absolute", "log_ratio"),
    n_highlight = 3L
) {
  change <- match.arg(change)

  posterior <- as.numeric(
    vctrs::vec_data(object)
  )

  prior <- .ffp_prior_data(prior)

  # Validation ------------------------------------------------------------

  if (length(prior) != length(posterior)) {
    stop(
      paste0(
        "`prior` and `object` must contain the same ",
        "number of scenarios."
      ),
      call. = FALSE
    )
  }

  .ffp_validate_probabilities(
    posterior,
    "Posterior probabilities"
  )

  .ffp_validate_probabilities(
    prior,
    "Prior probabilities"
  )

  if (
    change %in% c("relative", "log_ratio") &&
    any(prior == 0)
  ) {
    stop(
      paste0(
        "`prior` contains zero probabilities, so ",
        "`change = \"", change, "\"` is undefined. ",
        "Use `change = \"absolute\"` instead."
      ),
      call. = FALSE
    )
  }

  n_highlight <- as.integer(n_highlight)

  if (
    length(n_highlight) != 1L ||
    is.na(n_highlight) ||
    n_highlight < 0L
  ) {
    stop(
      "`n_highlight` must be a non-negative integer.",
      call. = FALSE
    )
  }

  id <- seq_along(posterior)

  # Summary statistics ----------------------------------------------------

  n_eff_prior <- .ffp_effective_scenarios(prior)

  n_eff_posterior <- .ffp_effective_scenarios(
    posterior
  )

  mass_reallocated <- 0.5 * sum(
    abs(posterior - prior)
  )

  subtitle <- sprintf(
    paste0(
      "Prior vs Posterior | ",
      "Effective scenarios: %.0f -> %.0f | ",
      "Probability mass reallocated: %s"
    ),
    n_eff_prior,
    n_eff_posterior,
    scales::percent(
      mass_reallocated,
      accuracy = 0.1
    )
  )

  # Prior / posterior data ------------------------------------------------

  probability_data <- rbind(
    tibble::tibble(
      id = id,
      probability = prior,
      distribution = "Prior"
    ),
    tibble::tibble(
      id = id,
      probability = posterior,
      distribution = "Posterior"
    )
  )

  probability_data$distribution <- factor(
    probability_data$distribution,
    levels = c("Prior", "Posterior")
  )

  # Panel 1: prior vs posterior -------------------------------------------

  p_probability <- ggplot2::ggplot(
    probability_data,
    ggplot2::aes(
      x = .data$id,
      y = .data$probability,
      color = .data$distribution
    )
  ) +
    ggplot2::geom_line(
      linewidth = 0.55,
      alpha = 0.85
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Prior" = "#AEB4BC",
        "Posterior" = "#2E86AB"
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(
        accuracy = 0.01
      ),
      expand = ggplot2::expansion(
        mult = c(0.05, 0.10)
      )
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Probability"
    ) +
    .ffp_plot_theme() +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "left",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        5, 15, 0, 15
      )
    )

  # Probability change ----------------------------------------------------

  change_value <- switch(
    change,

    relative = {
      posterior / prior - 1
    },

    absolute = {
      posterior - prior
    },

    log_ratio = {
      log(posterior / prior)
    }
  )

  change_data <- tibble::tibble(
    id = id,
    prior = prior,
    posterior = posterior,
    change = change_value
  )

  change_data$direction <- ifelse(
    change_data$change > 0,
    "Upweighted",
    ifelse(
      change_data$change < 0,
      "Downweighted",
      "Unchanged"
    )
  )

  change_data$direction <- factor(
    change_data$direction,
    levels = c(
      "Upweighted",
      "Downweighted",
      "Unchanged"
    )
  )

  highlight_data <- .ffp_largest_shifts(
    change_data,
    n = n_highlight
  )

  # Axis formatting according to selected change -------------------------

  if (change == "relative") {
    change_y_lab <- "Change from Prior"

    change_labels <- scales::percent_format(
      accuracy = 1
    )
  } else if (change == "absolute") {
    change_y_lab <- "Probability Change"

    change_labels <- scales::percent_format(
      accuracy = 0.01
    )
  } else {
    change_y_lab <- "Log(Post / Prior)"

    change_labels <- scales::number_format(
      accuracy = 0.1
    )
  }

  # Panel 2: probability shifts -------------------------------------------

  p_change <- ggplot2::ggplot(
    change_data,
    ggplot2::aes(
      x = .data$id
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#555555",
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = .data$id,
        y = 0,
        yend = .data$change,
        color = .data$direction
      ),
      linewidth = 0.35,
      alpha = 0.70
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Upweighted" = "#3498DB",
        "Downweighted" = "#E74C3C",
        "Unchanged" = "#B8B8B8"
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = change_labels,
      expand = ggplot2::expansion(
        mult = c(0.15, 0.15)
      )
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    ggplot2::labs(
      x = "Scenario Index",
      y = change_y_lab
    ) +
    .ffp_plot_theme() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(
        0, 15, 5, 15
      )
    )

  # Highlight the largest shifts -----------------------------------------

  if (nrow(highlight_data)) {
    p_change <- p_change +
      ggplot2::geom_point(
        data = highlight_data,
        mapping = ggplot2::aes(
          x = .data$id,
          y = .data$change,
          color = .data$direction
        ),
        size = 2,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        data = highlight_data,
        mapping = ggplot2::aes(
          x = .data$id,
          y = .data$change,
          label = .data$id,
          vjust = .data$label_vjust
        ),
        size = 2.7,
        color = "#444444",
        check_overlap = TRUE,
        show.legend = FALSE
      )
  }

  # Combine panels --------------------------------------------------------

  patchwork::wrap_plots(
    p_probability,
    p_change,
    ncol = 1,
    heights = c(2, 1)
  ) +
    patchwork::plot_annotation(
      title = "Flexible Forward Probabilities",
      subtitle = subtitle,
      caption = paste0(
        "Blue: probability increased | ",
        "Red: probability decreased | ",
        "Labels identify the largest shifts"
      ),
      theme = .ffp_annotation_theme()
    )
}


# Autoplot ----------------------------------------------------------------


#' Inspection of a `ffp` object with ggplot2
#'
#' Extends the `autoplot` method for the `ffp` class. When `prior` is not
#' supplied, the current probability distribution is displayed. When `prior`
#' is supplied, the method compares prior and posterior probabilities and
#' displays the probability shifts induced by entropy pooling.
#'
#' @param object An object of class `ffp`.
#' @param color A logical flag indicating whether probabilities should be
#' colored using a probability gradient when `prior = NULL`.
#' @param prior An optional `ffp` object or numeric vector containing prior
#' probabilities. If supplied, a prior-versus-posterior comparison is shown.
#' @param change Character string indicating how probability changes should
#' be represented. One of `"relative"`, `"absolute"`, or `"log_ratio"`.
#' @param n_highlight Number of largest upward and downward probability
#' shifts to identify in the comparison plot. Default is 3.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A `ggplot2` object when `prior = NULL`, or a `patchwork` object
#' containing the comparison panels when `prior` is supplied.
#'
#' @export
#'
#' @importFrom ggplot2 autoplot
#' @rdname autoplot
#'
#' @examples
#' library(ggplot2)
#'
#' # Flexible probabilities
#' posterior <- exp_decay(EuStockMarkets, 0.001)
#'
#' # Distribution only
#' autoplot(posterior)
#'
#' # Without color gradient
#' autoplot(posterior, color = FALSE)
#'
#' \dontrun{
#' # Prior versus posterior
#' autoplot(
#'   posterior,
#'   prior = prior
#' )
#'
#' # Absolute probability change
#' autoplot(
#'   posterior,
#'   prior = prior,
#'   change = "absolute"
#' )
#'
#' # Log probability ratio
#' autoplot(
#'   posterior,
#'   prior = prior,
#'   change = "log_ratio"
#' )
#' }
autoplot.ffp <- function(
    object,
    color = TRUE,
    prior = NULL,
    change = c(
      "relative",
      "absolute",
      "log_ratio"
    ),
    n_highlight = 3L,
    ...
) {
  if (is.null(prior)) {
    return(
      .ffp_plot_distribution(
        object = object,
        color = color
      )
    )
  }

  .ffp_plot_comparison(
    object = object,
    prior = prior,
    change = change,
    n_highlight = n_highlight
  )
}


# Plot --------------------------------------------------------------------


#' @rdname autoplot
#' @importFrom graphics plot
#' @exportS3Method
plot.ffp <- function(object, ...) {
  p <- ggplot2::autoplot(
    object,
    ...
  )

  print(p)

  invisible(p)
}
