#' Moments under Flexible Probabilities
#'
#' Computes the location and dispersion statistics under flexible probabilities.
#'
#' @param x A tabular (non-tidy) data structure.
#' @param p A probability vector.
#'
#' @return A \code{list} with 2 elements: \code{mu} and \code{sigma}.
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' colnames(x) <- colnames(EuStockMarkets)
#' p <- stats::runif(nrow(x))
#' p <- p / sum(p)
#'
#' ffp_moments(x = x, p = p)
#'
#' # compare with the standard approach
#' colMeans(x)
#' cov(x)
ffp_moments <- function(x, p = NULL) {
  UseMethod("ffp_moments", x)
}

#' @rdname ffp_moments
#' @export
ffp_moments.default <- function(x, p = NULL) {
  stop("`ffp_moments` doesn't know how to deal with the `", class(x)[[1L]], "` class yet.", call. = FALSE)
}

#' @rdname ffp_moments
#' @export
ffp_moments.numeric <- function(x, p = NULL) {
  ffp_moments_(x = x, p = check_p(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.matrix <- function(x, p = NULL) {
  ffp_moments_(x = x, p = check_p(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.xts <- function(x, p = NULL) {
  ffp_moments_(x = as.matrix(x), p = check_p(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.data.frame <- function(x, p = NULL) {
  x <- dplyr::select(x, where(is.numeric))
  assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
  ffp_moments_(x = as.matrix(x), p = check_p(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.tbl_df <- function(x, p = NULL) {
  x <- dplyr::select(x, where(is.numeric))
  assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
  ffp_moments_(x = as.matrix(x), p = check_p(p))
}

#' @keywords internal
ffp_moments_ <- function(x, p = NULL) {

  if (is.null(dim(x)) | is.vector(x)) {
    if (is.null(p)) {
      p <- rep(1 / length(x), length(x))
    }

    mu    <- sum(p * x)
    sigma <- sum(p * x * x) - (sum(p * x) * sum(p * x))

  }

  if (!is.null(dim(x)) | is.matrix(x)) {
    if (is.null(p)) {
      p <- rep(1 / nrow(x), nrow(x))
    }

    mu         <- t(x) %*% p
    x_centered <- t(t(x) - as.vector(mu))
    sigma      <- t(x_centered) %*% (x_centered * p %*% (matrix(1, 1, ncol(x))))
  }

  list(mu = mu, sigma = sigma)

}


# -------------------------------------------------------------------------


#' Internal functions to check the consistency of probabilities.
#'
#' @param p A vector, a matrix, a xts or a tibble object.
#'
#' @return A matrix object with 1 column.
#'
#' @export
#'
#' @examples
#' #
check_p <- function(p) {
  UseMethod("check_p", p)
}

#' @rdname check_p
#' @export
check_p.default <- function(p) {
  stop("`ffp` doesn't support the `", class(p)[[1L]], "` yet.", call. = FALSE)
}

#' @rdname check_p
#' @export
check_p.NULL <- function(p) NULL

#' @rdname check_p
#' @export
check_p.numeric <- function(p) {
  p <- as.matrix(p)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p
#' @export
check_p.ffp <- function(p) {
  p <- as.matrix(p)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}


#' @rdname check_p
#' @export
check_p.matrix <- function(p) {
  p <- matrix(p, ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p
#' @export
check_p.xts <- function(p) {
  p <- matrix(p, ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p
#' @export
check_p.data.frame <- function(p) {
  p <- as.matrix(dplyr::select(p, where(is.numeric)))
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}


#' @rdname check_p
#' @export
check_p.tbl <- function(p) {
  p <- as.matrix(dplyr::select(p, where(is.numeric)))
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}
