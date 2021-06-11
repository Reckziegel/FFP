#' Plot Scenario Densities
#'
#' This function is designed make it easy to visualize the impact of a view in the
#' distribution of the P&L.
#'
#' @param x An univariate marginal distribution
#' @param p A vector of fully flexible probabilities.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
#' ret <- diff(log(EuStockMarkets))[, 1]
#' p <- smoothing(ret, 0.005)
#' scenario_density(ret, p)
scenario_density <- function(x, p) {

  assert_is_equal_size(x, p)
  .size <- vctrs::vec_size(x)

  scenarios_conditional <- bootstrap_scenarios(x, p, 10000)[[1]]
  #mean_conditional <- mean(scenarios_conditional, na.rm = TRUE)

  scenarios_unconditional <- bootstrap_scenarios(x, rep(1 / .size, .size), 10000)[[1]]
  #mean_unconditional <- mean(scenarios_unconditional, na.rm = TRUE)

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
