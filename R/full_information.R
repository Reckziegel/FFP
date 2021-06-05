# Crisp Conditioning ------------------------------------------------------

#' Full Information by Market Conditioning
#'
#' In this function probabilities are given full weight when a certain macroeconomic
#' condition satisfy a logical statement.
#'
#' @param x A vector of risk-drivers.
#' @param condition A \code{logical} vector with TRUE's and FALSE's indicating
#' which scenarios should considered.
#'
#' @return A vector with the new probabilities distribution.
#'
#' @export
#'
#' @examples
#' # invariance (stationarity)
#' ret <- diff(log(EuStockMarkets))[ , 1]
#'
#' # full weight on scenarios above 2%
#' probs <- crisp(x = ret, ret > 0.02)
#' plot(probs, type = 'l')
crisp <- function(x, condition) {
  UseMethod("crisp", x)
}

#' @rdname crisp
#' @export
crisp.default <- function(x, condition) {
  stop("function not implemented in this class yet.", call. = FALSE)
}

#' @rdname crisp
#' @export
crisp.numeric <- function(x, condition) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(condition))
  )
  vctrs::vec_assert(condition, logical())

  make_crisp(x, condition)
}

#' @rdname crisp
#' @export
crisp.matrix <- function(x, condition) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(condition))
  )
  vctrs::vec_assert(condition, logical())

  make_crisp(x, condition)
}

#' @rdname crisp
#' @export
crisp.ts <- function(x, condition) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(condition))
  )
  vctrs::vec_assert(condition, logical())

  make_crisp(x, condition)
}

#' @rdname crisp
#' @export
crisp.xts <- function(x, condition) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(condition))
  )
  vctrs::vec_assert(condition, logical())

  make_crisp(x, condition)
}

#' @rdname crisp
#' @export
crisp.data.frame <- function(x, condition) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(condition))
  )
  vctrs::vec_assert(condition, logical())

  make_crisp(x, condition)
}

#' @rdname crisp
#' @export
crisp.tbl_df <- function(x, condition) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(condition))
  )
  vctrs::vec_assert(condition, logical())

  make_crisp(x, condition)
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
#' For example: log(2) / 0.0166 is approximately 42. So, a parameter lambda of
#' 0.0166 can be associated with a half-life of two-months.
#'
#' @param x A vector of risk-drivers.
#' @param lambda A number with the decay parameter that generated the half-life.
#'
#' @return A vector with the new probabilities distribution.
#'
#' @export
#'
#' @examples
#' ffp1 <- smoothing(EuStockMarkets, 0.015)
#' ffp2 <- smoothing(EuStockMarkets, 0.01)
#' ffp3 <- smoothing(EuStockMarkets, 0.0075)
#' ffp4 <- smoothing(EuStockMarkets, 0.005)
#'
#' plot(ffp1, type = 'l')
#' lines(ffp2, type = 'l', col = 'red')
#' lines(ffp3, type = 'l', col = 'blue')
#' lines(ffp4, type = 'l', col = 'green')
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
  make_smoothing(x, lambda)
}

#' @rdname smoothing
#' @export
smoothing.matrix <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  make_smoothing(x, lambda)
}

#' @rdname smoothing
#' @export
smoothing.ts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  make_smoothing(x, lambda)
}

#' @rdname smoothing
#' @export
smoothing.xts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  make_smoothing(x, lambda)
}

#' @rdname smoothing
#' @export
smoothing.data.frame <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  make_smoothing(x, lambda)
}

#' @rdname smoothing
#' @export
smoothing.tbl <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  make_smoothing(x, lambda)
}


# Normal Kernel -----------------------------------------------------------

#' Full Information by Kernel-Damping
#'
#' In this framework, historical realizations receive a weight proportional to
#' the distance from the target mean, which is enveloped
#' normal kernel with a bandwidth equal sigma.
#'
#' @param x An univariate set of risk-drivers.
#' @param mean A number in which the kernel should be centered.
#' @param sigma A number with the uncertainty (volatility) around mean.
#'
#' @return A vector with the new probabilities distribution.
#'
#' @export
#'
#' @examples
#' ret <- diff(log(EuStockMarkets[ , 1]))
#' mean <- -0.01 # scenarios around -1%
#' sigma <- var(diff(ret))
#'
#' plot(kernel_normal(ret, mean, sigma), type = 'l')
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

  make_kernel_normal(x = x, mean, sigma)
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

  make_kernel_normal(x = x, mean, sigma)

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

  make_kernel_normal(x = x, mean, sigma)

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

  make_kernel_normal(x = x, mean, sigma)
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

  make_kernel_normal(x = x, mean, sigma)

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

  make_kernel_normal(x = x, mean, sigma)
}


