# Crisp Conditioning ------------------------------------------------------

#' Full Information by Market Conditioning
#'
#' In this function probabilities are given full weight when a macroeconomic
#' condition satisfy a logical statement.
#'
#' @param x The risk-drivers.
#' @param lgl A \code{logical} vector with TRUE's and FALSE's indicating
#' which scenarios should considered.
#'
#' @return A \code{tibble} with the new probabilities distribution.
#'
#' @export
#'
#' @seealso \code{\link{smoothing}} \code{\link{kernel_normal}}
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
#' autoplot(market_condition)
crisp <- function(x, lgl) {
  UseMethod("crisp", x)
}

#' @rdname crisp
#' @export
crisp.default <- function(x, lgl) {
  stop("function not implemented in this class yet.", call. = FALSE)
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


# Exponential Smoothing ---------------------------------------------------

#' Full Information by Exponential Smoothing
#'
#' Exponential smoothing twists probabilities by giving relatively more weight
#' to recent observations at an exponential rate.
#'
#' The half-life is computed as:
#'
#' \code{HL = log(2) / lambda}.
#'
#' For example: log(2) / 0.0166 is approximately 42. So, a parameter `lambda` of
#' 0.0166 can be associated with a half-life of two-months.
#'
#' @param x The risk-drivers.
#' @param lambda A number with the decay parameter that generated the half-life.
#'
#' @return A \code{tibble} with the new probabilities distribution.
#'
#' @seealso \code{\link{crisp}} \code{\link{kernel_normal}} \code{\link{half_life}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # long half_life
#' long_hl <- smoothing(EuStockMarkets, 0.001)
#' long_hl
#'
#' # long half_file
#' autoplot(long_hl)
#'
#' # short half_life
#' short_hl <- smoothing(EuStockMarkets, 0.015)
#' short_hl
#' autoplot(short_hl)
smoothing <- function(x, lambda) {
  UseMethod("smoothing", x)
}


#' @rdname smoothing
#' @export
smoothing.default <- function(x, lambda) {
  stop("function not implemented in this class yet.", call. = FALSE)
}

#' @rdname smoothing
#' @export
smoothing.numeric <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)
  ffp(p)
}

#' @rdname smoothing
#' @export
smoothing.matrix <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)
  ffp(p)
}

#' @rdname smoothing
#' @export
smoothing.ts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)
  ffp(p)
}

#' @rdname smoothing
#' @export
smoothing.xts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)
  ffp(p)
}

#' @rdname smoothing
#' @export
smoothing.data.frame <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)
  ffp(p)
}

#' @rdname smoothing
#' @export
smoothing.tbl <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)
  ffp(p)
}


# Normal Kernel -----------------------------------------------------------

#' Full Information by Kernel-Damping
#'
#' In this framework, historical realizations receive a weight proportional to
#' it's distance from the target mean, which is enveloped by
#' normal kernel with a bandwidth equal sigma.
#'
#' @param x The risk-drivers.
#' @param mean The point in which the kernel should be centered.
#' @param sigma A number with the uncertainty (volatility) around the mean.
#'
#' @return A \code{tibble} with the new probabilities distribution.
#'
#' @export
#'
#' @seealso \code{\link{crisp}} \code{\link{smoothing}}
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
#' autoplot(kn)
#'
#' # Spread the distribution with a larger sigma
#' sigma <- var(diff(ret)) / 0.05
#' kn <- kernel_normal(ret, mean, sigma)
#'
#' autoplot(kn)
kernel_normal <- function(x, mean, sigma) {
  UseMethod("kernel_normal", x)
}

#' @rdname kernel_normal
#' @export
kernel_normal.default <- function(x, mean, sigma) {
  stop("function not implemented in this class yet.", call. = FALSE)
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
  x <- as_ffp_mat(x)

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
  x <- as_ffp_mat(x)

  p <- make_kernel_normal(x = x, mean, sigma)

  ffp(p)
}


