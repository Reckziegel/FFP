#' @rdname scenario_density
#'
#' @export
scenario_histogram <- function(x, p, n = 10000) {
  vctrs::vec_assert(n, double(), 1)
  if (!inherits(p, "ffp")) cli::cli_abort("Argument {.arg p} must be of class {.cls ffp}")
  assert_is_equal_size(x, p)
  .size <- vctrs::vec_size(x)
  ew <- as_ffp(rep(1 / .size, .size))

  scenarios_conditional <- bootstrap_scenarios(x, p, n)[[1]]
  scenarios_unconditional <- bootstrap_scenarios(x, ew, n)[[1]]

  cond_data <- vctrs::vec_data(scenarios_conditional)
  uncon_data <- vctrs::vec_data(scenarios_unconditional)

  # Calculate statistics for annotations
  cond_mean <- mean(cond_data)
  uncon_mean <- mean(uncon_data)

  tib_cond  <- tibble::tibble(.pnl = cond_data, scenario = as.factor("Conditional"))
  tib_uncon <- tibble::tibble(.pnl = uncon_data, scenario = as.factor("Unconditional"))

  # Create base plot with improved aesthetics
  plot_data <- dplyr::bind_rows(tib_uncon, tib_cond)
  xlim_range <- stats::quantile(uncon_data, c(0.001, 0.999))

  plot_data |>
    ggplot2::ggplot(ggplot2::aes(fill = .data$scenario, color = .data$scenario, x = .data$.pnl)) +
    # Add histogram with transparency for overlap visibility
    ggdist::stat_histinterval(
      alpha = 0.7,
      breaks = 10 * log(.size),
      outline_bars = TRUE,
      slab_color = NA,
      show.legend = FALSE
    ) +
    # Add subtle mean lines
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = cond_mean, color = "Conditional"),
      linetype = "dashed",
      linewidth = 0.8,
      alpha = 0.6,
      key_glyph = "blank"
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = uncon_mean, color = "Unconditional"),
      linetype = "dashed",
      linewidth = 0.8,
      alpha = 0.6,
      key_glyph = "blank"
    ) +
    ggplot2::coord_cartesian(xlim = xlim_range) +
    ggplot2::scale_color_manual(
      values = c("Conditional" = "#2E86AB", "Unconditional" = "#A23B72"),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(
      values = c("Conditional" = "#2E86AB", "Unconditional" = "#A23B72"),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::labs(
      title = "Impact of Forward-Looking Probabilities on P&L Distribution",
      x = "Return",
      y = "Frequency",
      fill = "Scenario",
      color = "Scenario",
      caption = "Dashed lines indicate mean returns"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold", margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = 9, color = "grey50", margin = ggplot2::margin(t = 10)),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
