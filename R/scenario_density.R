#' Plot Scenarios
#'
#' This functions are designed to make it easier to visualize the impact of a
#' _View_ in the P&L distribution.
#'
#' To generate a scenario-distribution the margins are bootstrapped using
#' \code{\link{bootstrap_scenarios}}. The number of resamples can be controlled
#' with the `n` argument (default is `n = 10000`).
#'
#' @param x An univariate marginal distribution.
#' @param p A probability from the `ffp` class.
#' @param n An \code{integer} scalar with the number of scenarios to be generated.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))[, 1]
#' p <- exp_decay(x, 0.005)
#'
#' scenario_density(x, p, 500)
#'
#' scenario_histogram(x, p, 500)
scenario_density <- function(x, p, n = 10000) {

  stopifnot(inherits(p, "ffp"))
  vctrs::vec_assert(n, double(), 1)
  assert_is_equal_size(x, p)
  .size <- vctrs::vec_size(x)
  ew <- as_ffp(rep(1 / .size, .size))

  scenarios_conditional   <- bootstrap_scenarios(x, p, n)[[1]]
  scenarios_unconditional <- bootstrap_scenarios(x, ew, n)[[1]]

  tib_cond  <- tibble::tibble(.pnl  = scenarios_conditional, scenario = as.factor("Conditional"))
  tib_uncon <- tibble::tibble(.pnl = scenarios_unconditional, scenario = as.factor("Unconditional"))

  dplyr::bind_rows(tib_uncon, tib_cond) %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$scenario, color = .data$scenario, x = .data$.pnl)) +
    ggdist::stat_slab(alpha = 0.75) +
    ggdist::stat_pointinterval(position = ggplot2::position_dodge(width = .4, preserve = "single")) +
    ggplot2::coord_cartesian(xlim = stats::quantile(scenarios_unconditional, c(0.001, 0.999))) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Scenario", color = "Scenario") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

}
