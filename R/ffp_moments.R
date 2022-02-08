
# Fully Flexible Moments --------------------------------------------------

#' Moments under Flexible Probabilities
#'
#' Computes the location and dispersion statistics under flexible probabilities.
#'
#' @param x A tabular (non-tidy) data structure.
#' @param p An object of the `ffp` class.
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
  ffp_moments_(x = x, p = as_ffp(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.matrix <- function(x, p = NULL) {
  ffp_moments_(x = x, p = as_ffp(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.xts <- function(x, p = NULL) {
  ffp_moments_(x = as.matrix(x), p = as_ffp(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.data.frame <- function(x, p = NULL) {
  x <- dplyr::select(x, where(is.numeric))
  assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
  ffp_moments_(x = as.matrix(x), p = as_ffp(p))
}

#' @rdname ffp_moments
#' @export
ffp_moments.tbl_df <- function(x, p = NULL) {
  x <- dplyr::select(x, where(is.numeric))
  assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
  ffp_moments_(x = as.matrix(x), p = as_ffp(p))
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



# Check generic input -----------------------------------------------------

#' Internal function used to check the validity of inputs.
#'
#' @param x Any object passed to other functions in the package.
#'
#' @return A matrix
#'
#' @keywords internal
check_input <- function(x) {
  UseMethod("check_input", x)
}

#' @rdname check_input
check_input.default <- function(x) {
  stop("`ffp` doesn't support the `", class(x)[[1L]], "` yet.", call. = FALSE)
}

#' @rdname check_input
check_input.numeric <- function(x) as.matrix(x)

#' @rdname check_input
check_input.matrix <- function(x) x

#' @rdname check_input
check_input.xts <- function(x) as.matrix(x)

#' @rdname check_input
check_input.tbl_df <- function(x) tbl_to_mtx(x)
