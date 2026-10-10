#' Summary Statistics for Empirical Distributions
#'
#' Computes marginal distribution statistics under flexible probabilities.
#'
#' `empirical_stats()` is the compatibility interface for the original FFP API.
#' The statistics are computed using the FFP 2.0 distributional core.
#'
#' @param x A time series or tabular object defining the scenario distribution.
#' @param p An object of class `ffp`.
#' @param level A single number strictly between 0 and 1 defining the
#'   lower-tail probability used for Value-at-Risk and Expected Shortfall.
#'
#' @return A tibble with one row per variable and the columns:
#'
#' - `variable`: variable name;
#' - `mean`: probability-weighted mean;
#' - `sd`: probability-weighted standard deviation;
#' - `skewness`: probability-weighted skewness;
#' - `kurtosis`: probability-weighted kurtosis;
#' - `value_at_risk`: lower-tail quantile reported using a loss convention;
#' - `expected_shortfall`: lower-tail expected shortfall reported using a
#'   loss convention.
#'
#' @export
#'
#' @examples
#' ret <- diff(log(EuStockMarkets))
#'
#' p <- exp_decay(ret, lambda = 0.015)
#'
#' empirical_stats(x = ret,p = p)
empirical_stats <- function(x, p, level = 0.01) {
  UseMethod("empirical_stats", x)
}


#' @rdname empirical_stats
#' @export
empirical_stats.default <- function(x, p, level = 0.01) {
  cli::cli_abort(
    c(
      "{.fn empirical_stats} is not implemented for class {.cls {class(x)}}.",
      "i" = paste0(
        "Supported classes: {.cls numeric}, {.cls matrix}, {.cls ts}, ",
        "{.cls xts}, {.cls data.frame}, {.cls tbl_df}."
      )
    )
  )
}


#' @rdname empirical_stats
#' @export
empirical_stats.numeric <- function(x, p, level = 0.01) {
  legacy_empirical_stats(x = x, p = p, level = level)
}


#' @rdname empirical_stats
#' @export
empirical_stats.matrix <- function(x, p, level = 0.01) {
  legacy_empirical_stats(x = x, p = p, level = level)
}


#' @rdname empirical_stats
#' @export
empirical_stats.ts <- function(x, p, level = 0.01) {
  legacy_empirical_stats(x = x, p = p, level = level)
}


#' @rdname empirical_stats
#' @export
empirical_stats.xts <- function(x, p, level = 0.01) {
  legacy_empirical_stats(x = x, p = p, level = level)
}


#' @rdname empirical_stats
#' @export
empirical_stats.data.frame <- function(x, p, level = 0.01) {
  legacy_empirical_stats(x = x, p = p, level = level)
}


#' @rdname empirical_stats
#' @export
empirical_stats.tbl_df <- function(x, p, level = 0.01) {
  legacy_empirical_stats(x = x, p = p, level = level)
}


legacy_empirical_stats <- function(x, p, level) {

  if (!is_ffp(p)) {
    cli::cli_abort(
      "{.arg p} must be an {.cls ffp} object."
    )
  }

  level <- ffp_stats_validate_prob(level)

  statistics <- ffp_stats(x = x, p = p, prob = level)$statistics

  dplyr::transmute(
    statistics,
    variable = .data$variable,
    mean     = .data$mean,
    sd       = .data$sd,
    skewness = .data$skewness,
    kurtosis = .data$kurtosis,
    value_at_risk      = -.data$quantile,
    expected_shortfall = -.data$expected_shortfall
  )
}


