#' Summary Statistics for Empirical Distributions
#'
#' Computes some of the most commonly used statistics by portfolio managers.
#'
#' The data in \code{x} and \code{p} are expected to have the same number of rows
#' (size).
#'
#' @param x A set of joint-scenarios of risk-drivers.
#' @param p A numeric vector with flexible probabilities.
#' @param level A number with the desired confidence level.
#'
#' @return A table with 6 components:
#'     \itemize{
#'       \item{\code{mu}}: A \code{numeric} with the location parameter.
#'       \item{\code{sd}}: A \code{numeric} with the standard deviation.
#'       \item{\code{skew}}: A \code{numeric} with the skewness.
#'       \item{\code{kurtosis}}: A \code{numeric} with the kurtosis.
#'       \item{\code{VaR}}: A \code{numeric} with the Value-at-Risk.
#'       \item{\code{CVaR}}: A \code{numeric} with the Expected-Shortfall.
#'     }
#'
#' @export
#'
#' @examples
#' ret <- diff(log(EuStockMarkets))
#' ew  <- rep(1 / nrow(ret), nrow(ret))
#'
#' empirical_stats(x = ret, p = ew)
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
empirical_stats.double <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  make_empirical_stats(as.matrix(x), p, level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.matrix <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  make_empirical_stats(x, p, level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.xts <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  nrow_ <- NROW(x)
  ncol_ <- NCOL(x)
  attributes(x)$class <- "matrix"
  x <- matrix(x, nrow = nrow_, ncol = ncol_)

  make_empirical_stats(x, p, level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.ts <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } else {
    x <- matrix(x, ncol = NCOL(x), nrow = NROW(x))
  }

  make_empirical_stats(x, p, level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.data.frame <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  #x <- ffp_tbl(x)
  x <- as_ffp_mat(x)
  make_empirical_stats(x, p, level)
}

#' @rdname empirical_stats
#' @export
empirical_stats.tbl_df <- function(x, p, level = 0.01) {
  vctrs::vec_assert(level, double(), 1)
  vctrs::vec_assert(p, double())
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  #x <- ffp_tbl(x)
  x <- as_ffp_mat(x)
  make_empirical_stats(x, p, level)
}


