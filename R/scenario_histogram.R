#' @rdname scenario_density
#'
#' @export
scenario_histogram <- function(x, p, n = 10000) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assert_is_equal_size(x, p)
  .size <- vctrs::vec_size(x)
  ew <- as_ffp(rep(1 / .size, .size))

  scenarios_conditional <- bootstrap_scenarios(x, p, n)[[1]]
  scenarios_unconditional <- bootstrap_scenarios(x, ew, n)[[1]]

  tib_cond  <- tibble::tibble(.pnl  = scenarios_conditional, scenario = as.factor("Conditional"))
  tib_uncon <- tibble::tibble(.pnl = scenarios_unconditional, scenario = as.factor("Unconditional"))

  dplyr::bind_rows(tib_uncon, tib_cond) %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$scenario, color = .data$scenario, x = .data$.pnl)) +
    ggdist::stat_histinterval(alpha = 0.8, breaks = 10 * log(.size), outline_bars = FALSE) +
    ggplot2::coord_cartesian(xlim = stats::quantile(scenarios_unconditional, c(0.001, 0.999))) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Scenario", color = "Scenario")
}
