# kernel_entropy ----------------------------------------------------------

#' Partial Information Kernel-Damping
#'
#' Find the probability distribution that can
#' constrain the first two moments while imposing the minimal structure in the data.
#'
#' @param x An univariate or a multivariate distribution.
#' @param mean A numeric vector in which the kernel should be centered.
#' @param sigma The uncertainty (volatility) around the mean. When \code{NULL}, only the mean is constrained.
#'
#' @return A numerical vector of class \code{ffp} with the new
#' probabilities distribution.
#'
#' @export
#'
#' @seealso \code{\link{double_decay}}
#'
#' @examples
#' library(ggplot2)
#'
#' ret <- diff(log(EuStockMarkets[ , 1]))
#' mean <- -0.01 # scenarios around -1%
#' sigma <- var(diff(ret))
#'
#' ke <- kernel_entropy(ret, mean, sigma)
#' ke
#'
#' autoplot(ke) +
#'   scale_color_viridis_c()
kernel_entropy <- function(x, mean, sigma = NULL) {
  UseMethod("kernel_entropy", x)
}

#' @rdname kernel_entropy
#' @export
kernel_entropy.default <- function(x, mean, sigma = NULL) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname kernel_entropy
#' @export
kernel_entropy.numeric <- function(x, mean, sigma = NULL) {
  vctrs::vec_assert(mean, double(), 1)
  if (!is.null(sigma)) {
    vctrs::vec_assert(sigma, double(), 1)
  }
  x <- as.matrix(x)
  p <- make_kernel_entropy(x, mean, sigma)

  ffp(p, fn = "kernel_entropy", user_call = match.call())

}

#' @rdname kernel_entropy
#' @export
kernel_entropy.matrix <- function(x, mean, sigma = NULL) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    if (!is.null(sigma)) {
      vctrs::vec_assert(sigma, double(), 1)
    }
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
    if (is.vector(mean)) mean <- as.matrix(mean)
  }

  p <- make_kernel_entropy(x, mean, sigma)

  ffp(p, fn = "kernel_entropy", user_call = match.call())

}

#' @rdname kernel_entropy
#' @export
kernel_entropy.ts <- function(x, mean, sigma = NULL) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    if (!is.null(sigma)) {
      vctrs::vec_assert(sigma, double(), 1)
    }
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
    if (is.vector(mean)) mean <- as.matrix(mean)
  }

  x <- as.matrix(x)
  p <- make_kernel_entropy(x, mean, sigma)

  ffp(p, fn = "kernel_entropy", user_call = match.call())

}

#' @rdname kernel_entropy
#' @export
kernel_entropy.xts <- function(x, mean, sigma = NULL) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    if (!is.null(sigma)) {
      vctrs::vec_assert(sigma, double(), 1)
    }
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
    if (is.vector(mean)) mean <- as.matrix(mean)
  }

  x <- as.matrix(x)
  p <- make_kernel_entropy(x, mean, sigma)

  ffp(p, fn = "kernel_entropy", user_call = match.call())

}

#' @rdname kernel_entropy
#' @export
kernel_entropy.tbl_df <- function(x, mean, sigma = NULL) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    if (!is.null(sigma)) {
      vctrs::vec_assert(sigma, double(), 1)
    }
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
    if (is.vector(mean)) mean <- as.matrix(mean)
  }

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])
  p <- make_kernel_entropy(x, mean, sigma)

  ffp(p, fn = "kernel_entropy", user_call = match.call())

}

#' @rdname kernel_entropy
#' @export
kernel_entropy.data.frame <- function(x, mean, sigma = NULL) {
  if (NCOL(x) == 1) {
    vctrs::vec_assert(mean, double(), 1)
    if (!is.null(sigma)) {
      vctrs::vec_assert(sigma, double(), 1)
    }
  } else {
    assertthat::are_equal(NCOL(x), vctrs::vec_size(mean))
    assert_is_equal_size(mean, sigma)
    if (is.vector(mean)) mean <- as.matrix(mean)
  }

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])
  p <- make_kernel_entropy(x, mean, sigma)

  ffp(p, fn = "kernel_entropy", user_call = match.call())

}



# Double Decay ------------------------------------------------------------

#' Flexible Probabilities using Partial Information
#'
#' Match different decay-factors on the covariance matrix.
#'
#' @param x An univariate or a multivariate distribution.
#' @param slow A \code{double} with the long half-life (slow decay) for the correlation
#' matrix.
#' @param fast A \code{double} with the short-life (high decay) for the volatility.
#'
#' @return A numerical vector of class \code{ffp} with the new
#' probabilities distribution.
#'
#' @export
#'
#' @seealso \code{\link{kernel_entropy}} \code{\link{half_life}}
#'
#' @references
#' De Santis, G., R. Litterman, A. Vesval, and K. Winkelmann, 2003,
#' Covariance matrix estimation, Modern investment management: an equilibrium
#' approach, Wiley.
#'
#' @examples
#' \donttest{
#'   library(ggplot2)
#'
#'   slow <- 0.0055
#'   fast <- 0.0166
#'   ret <- diff(log(EuStockMarkets))
#'
#'   dd <- double_decay(ret, slow, fast)
#'   dd
#'
#'   autoplot(dd) +
#'     scale_color_viridis_c()
#' }
double_decay <- function(x, slow, fast) {
  UseMethod("double_decay", x)
}

#' @rdname double_decay
#' @export
double_decay.default <- function(x, slow, fast) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname double_decay
#' @export
double_decay.numeric <- function(x, slow, fast) {
  vctrs::vec_assert(slow, double(), 1)
  vctrs::vec_assert(fast, double(), 1)

  x  <- as.matrix(x)
  p <- make_double_decay(x, slow, fast)

  ffp(p, fn = "double_decay", user_call = match.call())

}

#' @rdname double_decay
#' @export
double_decay.matrix <- function(x, slow, fast) {
  vctrs::vec_assert(slow, double(), 1)
  vctrs::vec_assert(fast, double(), 1)

  p <- make_double_decay(x, slow, fast)

  ffp(p, fn = "double_decay", user_call = match.call())

}

#' @rdname double_decay
#' @export
double_decay.ts <- function(x, slow, fast) {
  vctrs::vec_assert(slow, double(), 1)
  vctrs::vec_assert(fast, double(), 1)
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } else {
    x <- as.matrix(x)
  }

  p <- make_double_decay(x, slow, fast)

  ffp(p, fn = "double_decay", user_call = match.call())

}

#' @rdname double_decay
#' @export
double_decay.xts <- function(x, slow, fast) {
  vctrs::vec_assert(slow, double(), 1)
  vctrs::vec_assert(fast, double(), 1)

  x <- as.matrix(x)
  p <- make_double_decay(x, slow, fast)

  ffp(p, fn = "double_decay", user_call = match.call())

}

#' @rdname double_decay
#' @export
double_decay.tbl <- function(x, slow, fast) {
  vctrs::vec_assert(slow, double(), 1)
  vctrs::vec_assert(fast, double(), 1)

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])
  p <- make_double_decay(x, slow, fast)

  ffp(p, fn = "double_decay", user_call = match.call())

}

#' @rdname double_decay
#' @export
double_decay.data.frame <- function(x, slow, fast) {
  vctrs::vec_assert(slow, double(), 1)
  vctrs::vec_assert(fast, double(), 1)

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])
  p <- make_double_decay(x, slow, fast)

  ffp(p, fn = "double_decay", user_call = match.call())

}



