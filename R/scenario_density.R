#' Plot Scenario Densities
#'
#' This function is designed make it easy to visualize the impact of a view in the
#' P&L distribution.
#'
#' @param x An univariate marginal distribution.
#' @param p A vector of fully flexible probabilities.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
#' pnl <- diff(log(EuStockMarkets))[, 1]
#' p <- smoothing(pnl, 0.005)
#' scenario_density(pnl, p)
#'
#' p2 <- double_decay(pnl, 0.01, 0.001)
#' scenario_density(pnl, p2)
scenario_density <- function(x, p) {

  stopifnot(inherits(p, "ffp"))
  assert_is_equal_size(x, p)
  .size <- vctrs::vec_size(x)
  ew <- as_ffp(rep(1 / .size, .size))

  scenarios_conditional <- bootstrap_scenarios(x, p, 10000)[[1]]
  scenarios_unconditional <- bootstrap_scenarios(x, ew, 10000)[[1]]

  tib_cond  <- tibble::tibble(.pnl  = scenarios_conditional, scenario = as.factor("Conditional"))
  tib_uncon <- tibble::tibble(.pnl = scenarios_unconditional, scenario = as.factor("Unconditional"))

  dplyr::bind_rows(tib_uncon, tib_cond) %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$scenario, color = .data$scenario, x = .data$.pnl)) +
    ggdist::stat_slab(alpha = 0.75) +
    ggdist::stat_pointinterval(position = ggplot2::position_dodge(width = .4, preserve = "single")) +
    ggplot2::coord_cartesian(xlim = stats::quantile(scenarios_unconditional, c(0.001, 0.999))) +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggdist::theme_ggdist() +
    ggplot2::labs(x = NULL, y = NULL, fill = "Scenario", color = "Scenario")

}
