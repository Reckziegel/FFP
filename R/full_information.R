# Crisp Conditioning ------------------------------------------------------

#' Full Information by Market Conditioning
#'
#' Give full weight to occurrences when a macroeconomic statement satisfies
#' a logical condition.
#'
#' @param x An univariate or a multivariate distribution.
#' @param lgl A \code{logical} vector with TRUE's and FALSE's indicating which scenarios should considered.
#'
#' @return A numerical vector of class \code{ffp} with the new
#' probabilities distribution.
#'
#' @export
#'
#' @seealso \code{\link{exp_decay}} \code{\link{kernel_normal}}
#'
#' @examples
#' library(ggplot2)
#' # invariance (stationarity)
#' ret <- diff(log(EuStockMarkets))
#'
#' # full weight on scenarios where CAC operated above 2%
#' market_condition <- crisp(x = ret, ret[ , 3] > 0.02)
#' market_condition
#'
#' autoplot(market_condition) +
#'   scale_color_viridis_c()
crisp <- function(x, lgl) {
  UseMethod("crisp", x)
}

#' @rdname crisp
#' @export
crisp.default <- function(x, lgl) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname crisp
#' @export
crisp.numeric <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  ffp(p)
}

#' @rdname crisp
#' @export
crisp.matrix <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  ffp(p)

}

#' @rdname crisp
#' @export
crisp.ts <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)
  ffp(p)
}

#' @rdname crisp
#' @export
crisp.xts <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  ffp(p)
}

#' @rdname crisp
#' @export
crisp.data.frame <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  ffp(p)
}

#' @rdname crisp
#' @export
crisp.tbl_df <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  ffp(p)
}


# Exponential Decay ---------------------------------------------------

#' Full Information by Exponential Decay
#'
#' Exponential smoothing twists probabilities by giving relatively more weight
#' to recent observations at an exponential rate.
#'
#' The half-life is linked with the lambda parameter as follows:
#'
#' * \code{HL = log(2) / lambda}.
#'
#' For example: log(2) / 0.0166 is approximately 42. So, a parameter `lambda` of 0.0166 can be associated with a half-life of two-months.
#'
#' @param x An univariate or a multivariate distribution.
#' @param lambda A number for the decay parameter.
#'
#' @return A numerical vector of class \code{ffp} with the new
#' probabilities distribution.
#'
#' @seealso \code{\link{crisp}} \code{\link{kernel_normal}} \code{\link{half_life}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # long half_life
#' long_hl <- exp_decay(EuStockMarkets, 0.001)
#' long_hl
#' autoplot(long_hl) +
#'   scale_color_viridis_c()
#'
#' # short half_life
#' short_hl <- exp_decay(EuStockMarkets, 0.015)
#' short_hl
#' autoplot(short_hl) +
#'   scale_color_viridis_c()
exp_decay <- function(x, lambda) {
  UseMethod("exp_decay", x)
}


#' @rdname exp_decay
#' @export
exp_decay.default <- function(x, lambda) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname exp_decay
#' @export
exp_decay.numeric <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_decay(x, lambda)
  ffp(p)
}

#' @rdname exp_decay
#' @export
exp_decay.matrix <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_decay(x, lambda)
  ffp(p)
}

#' @rdname exp_decay
#' @export
exp_decay.ts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_decay(x, lambda)
  ffp(p)
}

#' @rdname exp_decay
#' @export
exp_decay.xts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_decay(x, lambda)
  ffp(p)
}

#' @rdname exp_decay
#' @export
exp_decay.data.frame <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_decay(x, lambda)
  ffp(p)
}

#' @rdname exp_decay
#' @export
exp_decay.tbl <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_decay(x, lambda)
  ffp(p)
}


# Normal Kernel -----------------------------------------------------------

#' Full Information by Kernel-Damping
#'
#' In this framework, historical realizations receive a weight proportional to
#' its distance from a target mean that is surrounded by normal kernel.
#'
#' @param x An univariate or a multivariate distribution.
#' @param mean A numeric vector in which the kernel should be centered.
#' @param sigma The uncertainty (volatility) around the mean.
#'
#' @return A numerical vector of class \code{ffp} with the new
#' probabilities distribution.
#'
#' @export
#'
#' @seealso \code{\link{crisp}} \code{\link{exp_decay}}
#'
#' @examples
#' library(ggplot2)
#'
#' ret <- diff(log(EuStockMarkets[ , 1]))
#' mean <- -0.01 # scenarios around -1%
#' sigma <- var(diff(ret))
#'
#' kn <- kernel_normal(ret, mean, sigma)
#' kn
#'
#' autoplot(kn) +
#'   scale_color_viridis_c()
#'
#' # A larger sigma spreads out the distribution
#' sigma <- var(diff(ret)) / 0.05
#' kn <- kernel_normal(ret, mean, sigma)
#'
#' autoplot(kn) +
#'   scale_color_viridis_c()
kernel_normal <- function(x, mean, sigma) {
  UseMethod("kernel_normal", x)
}

#' @rdname kernel_normal
#' @export
kernel_normal.default <- function(x, mean, sigma) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname kernel_normal
#' @export
kernel_normal.numeric <- function(x, mean, sigma) {
  vctrs::vec_assert(mean, double(), 1)
  vctrs::vec_assert(sigma, double(), 1)

  p <- make_kernel_normal(x = x, mean, sigma)
  ffp(p)
}

#' @rdname kernel_normal
#' @export
kernel_normal.matrix <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    vctrs::vec_assert(sigma, double(), 1)
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
  }

  p <- make_kernel_normal(x = x, mean, sigma)

  ffp(p)

}

#' @rdname kernel_normal
#' @export
kernel_normal.ts <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    vctrs::vec_assert(sigma, double(), 1)
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
  }

  p <- make_kernel_normal(x = x, mean, sigma)

  ffp(p)

}

#' @rdname kernel_normal
#' @export
kernel_normal.xts <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    vctrs::vec_assert(sigma, double(), 1)
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
  }

  p <- make_kernel_normal(x = x, mean, sigma)

  ffp(p)
}

#' @rdname kernel_normal
#' @export
kernel_normal.tbl_df <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    vctrs::vec_assert(sigma, double(), 1)
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
  }
  x <- dplyr::select(x, where(is.numeric) & where(is.double))
  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])

  p <- make_kernel_normal(x = x, mean, sigma)

  ffp(p)

}

#' @rdname kernel_normal
#' @export
kernel_normal.data.frame <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    vctrs::vec_assert(sigma, double(), 1)
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
  }
  x <- dplyr::select(x, where(is.numeric) & where(is.double))
  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])

  p <- make_kernel_normal(x = x, mean, sigma)

  ffp(p)
}


