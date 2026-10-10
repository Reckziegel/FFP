
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
  cli::cli_abort(
    c("x" = "{.fn ffp_moments} doesn't know how to deal with class {.cls {class(x)[[1L]]}}.",
      "i" = "Supported classes: {.cls numeric}, {.cls matrix}, {.cls xts}, {.cls ts}, {.cls mts}")
  )
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

#' @export
#' @noRd
ffp_moments.ffp_fit <- function(x, p = NULL) {

  validate_ffp_fit(x)

  if (!is.null(p)) {
    cli::cli_abort(
      c(
        "{.arg p} must not be supplied for an {.cls ffp_fit} object.",
        "i" = paste0(
          "The posterior probabilities are obtained directly ",
          "from the fitted object."
        )
      )
    )
  }
  ffp_moments(x = x$model$scenarios, p = x$posterior)
}

#' @export
#' @noRd
ffp_moments.ffp_opinion_pool <- function(x, p = NULL) {
  validate_ffp_opinion_pool(x)

  if (!is.null(p)) {
    cli::cli_abort(
      c(
        "{.arg p} must not be supplied for an {.cls ffp_opinion_pool} object.",
        "i" = paste0(
          "The confidence-weighted posterior probabilities are obtained ",
          "directly from the opinion-pool object."
        )
      )
    )
  }

  ffp_moments(x = x$fit$model$scenarios, p = x$posterior)
}

#' @keywords internal
weighted_moments <- function(x, p, call = rlang::caller_env()) {

  x <- as.matrix(x)
  probabilities <- as.double(p)

  validate_reference_prior(prior = probabilities,n_scenarios = nrow(x), call = call)

  probabilities <- probabilities / sum(probabilities)
  mu <- reference_feature_means(feature_values = x, prior = probabilities, call = call)

  centered <- sweep(x, MARGIN = 2, STATS = mu, FUN = "-")
  weighted_centered <- sweep(centered, MARGIN = 1, STATS = probabilities, FUN = "*")

  sigma <- crossprod(centered, weighted_centered)
  sigma <- (sigma + t(sigma)) / 2

  variable_names <- colnames(x)

  if (!is.null(variable_names)) {
    names(mu) <- variable_names
    dimnames(sigma) <- list(variable_names, variable_names)
  }

  list(mu = mu, sigma = sigma)
}

#' @keywords internal
ffp_moments_ <- function(x, p) {
  weighted_moments(x = x,p = p)
}




