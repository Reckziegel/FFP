# Distribution plots ------------------------------------------------------

#' Plot a distribution under Flexible Probabilities
#'
#' Visualizes the marginal distribution of a numeric scenario feature under
#' Flexible Probabilities.
#'
#' For an [ffp_fit] object, the plot compares the prior distribution with the
#' full-confidence distribution obtained through Entropy Pooling.
#'
#' For an `ffp_opinion_pool` object, the plot additionally includes the
#' confidence-weighted posterior obtained through opinion pooling.
#'
#' The distributions are represented by weighted Gaussian kernel density
#' estimates. All distributions in the same plot use the same scenario
#' support, bandwidth, and evaluation grid. Therefore, differences between
#' curves arise exclusively from differences in scenario probabilities.
#'
#' @param object An [ffp_fit] or `ffp_opinion_pool` object.
#' @param feature A single character string identifying the numeric scenario
#'   feature to plot.
#' @param distributions Optional character vector selecting the distributions
#'   to display. Supported values depend on `object` and include `"prior"`,
#'   `"full_confidence"`, and, for an `ffp_opinion_pool`, `"posterior"`.
#'   If `NULL`, all distributions available in `object` are shown.
#' @param statistics Optional character vector selecting location statistics
#'   to display. Supported values are `"mean"` and `"median"`. Use `NULL` to
#'   omit them.
#' @param quantiles Optional numeric vector containing quantile probabilities
#'   strictly between 0 and 1. By default, the 5 percent quantile is shown.
#'   Use `NULL` to omit quantiles.
#' @param adjust A single positive number multiplying the common kernel density
#'   bandwidth. Values below 1 reduce smoothing and values above 1 increase
#'   smoothing.
#' @param ... Reserved for future extensions. Must currently be empty.
#'
#' @return A `ggplot2` object.
#'
#' @details
#' Fully Flexible Probabilities leave the original scenarios unchanged and
#' modify only their probabilities. The kernel density estimate therefore has
#' the form
#'
#' \deqn{
#' \hat f_p(x)
#' =
#' \sum_{t=1}^{T}
#' p_t
#' \frac{1}{h}
#' K\left(\frac{x-x_t}{h}\right),
#' }
#'
#' where \eqn{p_t} are the scenario probabilities and \eqn{h} is a common
#' bandwidth shared by all distributions shown in the figure.
#'
#' Mean, median, and requested quantiles are computed directly under each
#' probability distribution. Their vertical segments extend from zero to the
#' corresponding weighted density curve.
#'
#' Because the same kernel and bandwidth are used throughout, an opinion-pooled
#' posterior also preserves the linear pooling identity at the density level.
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
#' ffp_plot_distribution(
#'   fit,
#'   feature = "equity"
#' )
#'
#' pooled <- ffp_opinion_pooling(
#'   fit,
#'   confidence = 0.75
#' )
#'
#' ffp_plot_distribution(
#'   pooled,
#'   feature = "equity"
#' )
#'
#' ffp_plot_distribution(
#'   pooled,
#'   feature = "equity",
#'   distributions = c(
#'     "prior",
#'     "posterior"
#'   ),
#'   statistics = "mean",
#'   quantiles = c(
#'     0.01,
#'     0.05
#'   )
#' )
#'
#' @seealso
#' [ffp_probabilities()], [ffp_stats()], [ffp_opinion_pooling()]
#'
#' @export
ffp_plot_distribution <- function(
    object,
    feature = NULL,
    distributions = NULL,
    statistics = c(
      "mean",
      "median"
    ),
    quantiles = 0.05,
    adjust = 1,
    ...
) {
  rlang::check_dots_empty()

  plot_data <- prepare_distribution_density_plot_data(
    object = object,
    feature = feature,
    distributions = distributions,
    statistics = statistics,
    quantiles = quantiles,
    adjust = adjust,
    n = 512L,
    call = rlang::caller_env()
  )

  plot_distribution_density(
    data = plot_data,
    feature = feature
  )
}


# Density plot ------------------------------------------------------------

plot_distribution_density <- function(
    data,
    feature
) {
  plot <- ggplot2::ggplot(
    data$density,
    ggplot2::aes(
      x = .data$x,
      y = .data$density,
      color = .data$distribution,
      group = .data$distribution
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = feature,
      y = "Density",
      color = "Distribution"
    )

  if (nrow(data$statistics) == 0L) {
    return(plot)
  }

  plot +
    ggplot2::geom_segment(
      data = data$statistics,
      ggplot2::aes(
        x = .data$value,
        xend = .data$value,
        y = 0,
        yend = .data$density,
        color = .data$distribution,
        linetype = .data$statistic
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      linetype = "Statistic"
    )
}
