
# Fully Flexible Moments --------------------------------------------------

#' Moments with Flexible Probabilities
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
  rlang::abort("`ffp_moments` doesn't know how to deal with the `", class(x)[[1L]], "` class yet.", call. = FALSE)
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

  mom <- stats::cov.wt(x = x, wt = p)

  list(mu = mom$center, sigma = mom$cov)

}




