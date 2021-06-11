# Crisp Conditioning ------------------------------------------------------

#' Full Information by Market Conditioning
#'
#' In this function probabilities are given full weight when a certain macroeconomic
#' condition satisfy a logical statement.
#'
#' @param x A vector of risk-drivers.
#' @param lgl A \code{logical} vector with TRUE's and FALSE's indicating
#' which scenarios should considered.
#'
#' @return A \code{tibble} with the new probabilities distribution.
#'
#' @export
#'
#' @examples
#' # invariance (stationarity)
#' ret <- diff(log(EuStockMarkets))[ , 1]
#'
#' # full weight on scenarios above 2%
#' crisp(x = ret, ret > 0.02)
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

  # FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Crisp Conditioning")
}

#' @rdname crisp
#' @export
crisp.matrix <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Crisp Conditioning")

}

#' @rdname crisp
#' @export
crisp.ts <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)
  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Crisp Conditioning")
}

#' @rdname crisp
#' @export
crisp.xts <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Crisp Conditioning")
}

#' @rdname crisp
#' @export
crisp.data.frame <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Crisp Conditioning")
}

#' @rdname crisp
#' @export
crisp.tbl_df <- function(x, lgl) {
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(lgl))
  )
  vctrs::vec_assert(lgl, logical())

  p <- make_crisp(x, lgl)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Crisp Conditioning")
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
#' @return A \code{tibble} with the new probabilities distribution.
#'
#' @export
#'
#' @examples
#' smoothing(EuStockMarkets, 0.015)
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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Exponential Smoothing")
}

#' @rdname smoothing
#' @export
smoothing.matrix <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Exponential Smoothing")
}

#' @rdname smoothing
#' @export
smoothing.ts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Exponential Smoothing")
}

#' @rdname smoothing
#' @export
smoothing.xts <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Exponential Smoothing")
}

#' @rdname smoothing
#' @export
smoothing.data.frame <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Exponential Smoothing")
}

#' @rdname smoothing
#' @export
smoothing.tbl <- function(x, lambda) {
  vctrs::vec_assert(lambda, double(), 1)
  p <- make_smoothing(x, lambda)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Exponential Smoothing")
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
#' @return A \code{tibble} with the new probabilities distribution.
#'
#' @export
#'
#' @examples
#' ret <- diff(log(EuStockMarkets[ , 1]))
#' mean <- -0.01 # scenarios around -1%
#' sigma <- var(diff(ret))
#'
#' kernel_normal(ret, mean, sigma)
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

  call_label <- dplyr::as_label(match.call())

  p <- make_kernel_normal(x = x, mean, sigma)

  #FIXME
  #call_label <- dplyr::as_label(match.call())
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Damping")
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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Damping")

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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Damping")

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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Damping")
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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Damping")

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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Damping")
}


