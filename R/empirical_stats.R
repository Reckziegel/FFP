#' Summary Statistics for Empirical Distributions
#'
#' Computes some of the most commonly used statistics by portfolio managers.
#'
#' The data in \code{x} and \code{p} are expected to have the same number of rows
#' (size).
#'
#' @param x A set of joint-scenarios of risk-drivers.
#' @param p A probability from the `ffp` class.
#' @param level A number with the desired confidence level.
#'
#' @return A \code{tibble} with 6 rows and the following components:
#'   * Mu,
#'   * Std
#'   * Skew
#'   * Kurt
#'   * Var
#'   * CVaR
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' ret <- diff(log(EuStockMarkets))
#'
#' # with equal weights
#' ew  <- rep(1 / nrow(ret), nrow(ret))
#' empirical_stats(x = ret, p = as_ffp(ew))
#'
#' # with ffp
#' exp_smooth <- exp_smoothing(ret, 0.015)
#' empirical_stats(ret, exp_smooth)
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
  #vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)

  make_empirical_stats(x = x, p = as.matrix(p), level = level)

}

#' @rdname empirical_stats
#' @export
empirical_stats.matrix <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  #vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)

  make_empirical_stats(x = x, p = as.matrix(p), level = level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.xts <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  #vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)

  make_empirical_stats(x = x, p = as.matrix(p), level = level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.ts <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  #vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)

  make_empirical_stats(x = x, p = as.matrix(p), level = level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.data.frame <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  #vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(purrr::keep(x, is.double))

  make_empirical_stats(x = x, p = as.matrix(p), level = level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.tbl_df <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  #vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(purrr::keep(x, is.double))

  make_empirical_stats(x = x, p = as.matrix(p), level = level)
}


