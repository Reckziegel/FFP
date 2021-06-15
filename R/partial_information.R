# kernel_entropy ----------------------------------------------------------

#' Partial Information Kernel-Damping
#'
#' This function uses entropy-pooling to find the probability distribution that can
#' constrain the first two moments while imposing the minimal structure in the data.
#'
#' @param x The risk-drivers.
#' @param mean A number in which the kernel should be centered.
#' @param sigma A number with the uncertainty around mean. When \code{NULL}, only
#' the mean is constrained.
#'
#' @return A \code{tibble} with the new probabilities distribution.
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
#' ggplot(ke, aes(x = .rowid, y = .p, color = .p)) +
#'   geom_line(show.legend = FALSE) +
#'   scale_color_viridis_c()
kernel_entropy <- function(x, mean, sigma = NULL) {
  UseMethod("kernel_entropy", x)
}

#' @rdname kernel_entropy
#' @export
kernel_entropy.default <- function(x, mean, sigma = NULL) {
  stop("function not implemented in this class yet.", call. = FALSE)
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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Entropy-Pooling")
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

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Entropy-Pooling")

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

  x <- as_ffp_mat(x)
  p <- make_kernel_entropy(x, mean, sigma)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Entropy-Pooling")
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

  x <- as_ffp_mat(x)
  p <- make_kernel_entropy(x, mean, sigma)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Entropy-Pooling")
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

  x <- as_ffp_mat(x)

  p <- make_kernel_entropy(x, mean, sigma)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Entropy-Pooling")
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

  x <- as_ffp_mat(x)
  p <- make_kernel_entropy(x, mean, sigma)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Kernel Entropy-Pooling")
}



# Double Decay ------------------------------------------------------------

#' Flexible Probabilities using Partial Information
#'
#' This function uses entropy-pooling to match different decay-factors on the
#' covariance matrix.
#'
#' @param x The risk-drivers.
#' @param decay_low A number with the long half-life (slow decay) for the correlation
#' matrix.
#' @param decay_high A number with the short-life (high decay) for the volatility
#' structure.
#'
#' @return A \code{tibble} with the new probabilities distribution.
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
#' library(ggplot2)
#'
#' l_c <- 0.0055
#' l_s <- 0.0166
#' ret <- diff(log(EuStockMarkets))
#'
#' dd <- double_decay(ret, l_c, l_s)
#' dd
#'
#' ggplot(dd, aes(x = .rowid, y = .p, color = .p)) +
#'   geom_line(show.legend = FALSE) +
#'   scale_color_viridis_c()
double_decay <- function(x, decay_low, decay_high) {
  UseMethod("double_decay", x)
}

#' @rdname double_decay
#' @export
double_decay.default <- function(x, decay_low, decay_high) {
  stop("function not implemented in this class yet.", call. = FALSE)
}

#' @rdname double_decay
#' @export
double_decay.numeric <- function(x, decay_low, decay_high) {
  vctrs::vec_assert(decay_low, double(), 1)
  vctrs::vec_assert(decay_high, double(), 1)
  x  <- as.matrix(x)

  p <- make_double_decay(x, decay_low, decay_high)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Double-Decay")
}

#' @rdname double_decay
#' @export
double_decay.matrix <- function(x, decay_low, decay_high) {
  vctrs::vec_assert(decay_low, double(), 1)
  vctrs::vec_assert(decay_high, double(), 1)

  p <- make_double_decay(x, decay_low, decay_high)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Double-Decay")
}

#' @rdname double_decay
#' @export
double_decay.ts <- function(x, decay_low, decay_high) {
  vctrs::vec_assert(decay_low, double(), 1)
  vctrs::vec_assert(decay_high, double(), 1)
  if (is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } else {
    x <- as.matrix(x)
  }
  p <- make_double_decay(x, decay_low, decay_high)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Double-Decay")
}

#' @rdname double_decay
#' @export
double_decay.xts <- function(x, decay_low, decay_high) {
  vctrs::vec_assert(decay_low, double(), 1)
  vctrs::vec_assert(decay_high, double(), 1)
  x <- as_ffp_mat(x)
  p <- make_double_decay(x, decay_low, decay_high)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Double-Decay")
}

#' @rdname double_decay
#' @export
double_decay.tbl <- function(x, decay_low, decay_high) {
  vctrs::vec_assert(decay_low, double(), 1)
  vctrs::vec_assert(decay_high, double(), 1)

  x <- as_ffp_mat(x)
  p <- make_double_decay(x, decay_low, decay_high)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Double-Decay")
}

#' @rdname double_decay
#' @export
double_decay.data.frame <- function(x, decay_low, decay_high) {
  vctrs::vec_assert(decay_low, double(), 1)
  vctrs::vec_assert(decay_high, double(), 1)

  x <- as_ffp_mat(x)
  p <- make_double_decay(x, decay_low, decay_high)

  #FIXME
  new_ffp(tibble::tibble(.rowid = 1:vctrs::vec_size(x), .p = p),
          type = "Double-Decay")
}



