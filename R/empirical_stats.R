#' Summary Statistics for Empirical Distributions
#'
#' Computes the mean, standard deviation, skewness, kurtosis, Value-at-Risk (VaR) and
#' Conditional Value-at-Risk CVaR) under flexible probabilities.
#'
#' The data in \code{x} and \code{p} are expected to have the same number of rows
#' (size).
#'
#' @param x A time series defining the scenario-probability distribution.
#' @param p An object of the `ffp` class.
#' @param level A number with the desired probability level. The default is
#' `level = 0.01`.
#'
#' @return A tidy \code{tibble} with 3 columns:
#'   * stat: a column with `Mu`, `Std`, `Skew`, `Kurt`, `VaR` and `CVaR`.
#'   * name: the asset names.
#'   * value: the computed value for each statistic.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(ggplot2)
#'
#' ret <- diff(log(EuStockMarkets))
#'
#' # with equal weights (standard scenario)
#' ew  <- rep(1 / nrow(ret), nrow(ret))
#' empirical_stats(x = ret, p = as_ffp(ew)) %>%
#'   ggplot(aes(x = name, y = value)) +
#'   geom_col() +
#'   facet_wrap(~stat, scales = "free") +
#'   labs(x = NULL, y = NULL)
#'
#' # with ffp
#' exp_smooth <- exp_decay(ret, 0.015)
#' empirical_stats(ret, exp_smooth) %>%
#'   ggplot(aes(x = name, y = value)) +
#'   geom_col() +
#'   facet_wrap(~stat, scales = "free") +
#'   labs(x = NULL, y = NULL)
empirical_stats <- function(x, p, level = 0.01) {
  UseMethod("empirical_stats", x)
}

#' @rdname empirical_stats
#' @export
empirical_stats.default <- function(x, p, level = 0.01) {
  stop("function not implemented in this class yet.", call. = FALSE)
}

#' @rdname empirical_stats
#' @export
empirical_stats.numeric <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x)

  make_empirical_stats(x = x, p = as.matrix(p), level = level) %>%
    tidyr::pivot_longer(-.data$stat) %>%
    dplyr::mutate(stat = forcats::fct_relevel(
      .data$stat,
      "Mu", "Std", "Skew", "Kurt", "VaR", "CVaR"
      )
    )

}

#' @rdname empirical_stats
#' @export
empirical_stats.matrix <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  make_empirical_stats(x = x, p = as.matrix(p), level = level) %>%
    tidyr::pivot_longer(-.data$stat) %>%
    dplyr::mutate(stat = forcats::fct_relevel(
      .data$stat,
      "Mu", "Std", "Skew", "Kurt", "VaR", "CVaR"
    )
    )
}

#' @rdname empirical_stats
#' @export
empirical_stats.xts <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x)

  make_empirical_stats(x = x, p = as.matrix(p), level = level) %>%
    tidyr::pivot_longer(-.data$stat) %>%
    dplyr::mutate(stat = forcats::fct_relevel(
      .data$stat,
      "Mu", "Std", "Skew", "Kurt", "VaR", "CVaR"
    )
    )
}

#' @rdname empirical_stats
#' @export
empirical_stats.ts <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  .x <- matrix(x, nrow = NROW(x), ncol = NCOL(x))
  if (!is.null(colnames(x))) {
    colnames(.x) <- colnames(x)
  }

  make_empirical_stats(x = .x, p = as.matrix(p), level = level) %>%
    tidyr::pivot_longer(-.data$stat) %>%
    dplyr::mutate(stat = forcats::fct_relevel(
      .data$stat,
      "Mu", "Std", "Skew", "Kurt", "VaR", "CVaR"
    )
    )
}

#' @rdname empirical_stats
#' @export
empirical_stats.data.frame <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])

  make_empirical_stats(x = x, p = as.matrix(p), level = level) %>%
    tidyr::pivot_longer(-.data$stat) %>%
    dplyr::mutate(stat = forcats::fct_relevel(
      .data$stat,
      "Mu", "Std", "Skew", "Kurt", "VaR", "CVaR"
    )
    )
}

#' @rdname empirical_stats
#' @export
empirical_stats.tbl_df <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])

  make_empirical_stats(x = x, p = as.matrix(p), level = level) %>%
    tidyr::pivot_longer(-.data$stat) %>%
    dplyr::mutate(stat = forcats::fct_relevel(
      .data$stat,
      "Mu", "Std", "Skew", "Kurt", "VaR", "CVaR"
    )
    )
}


