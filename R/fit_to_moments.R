#' Double-Decay Covariance Matrix by Entropy-Pooling
#'
#' Computes flexible probabilities that match target first and second moments
#' while minimizing relative entropy from a uniform prior.
#'
#' @param X A numeric matrix of size `T x N` containing the scenario features.
#' @param m A numeric vector or matrix of length `N` containing the target
#'   posterior means.
#' @param S A numeric matrix of size `N x N` containing the target posterior
#'   covariance matrix.
#'
#' @return A numeric vector containing the posterior probabilities.
#'
#' @export
#'
#' @keywords internal
fit_to_moments <- function(X, m, S) {
  moment_entropy_probabilities(
    x = X,
    target_mean = as.double(m),
    target_covariance = S
  )
}
