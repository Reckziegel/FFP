#' Double-Decay Covariance Matrix
#'
#' This function computes the covariance matrix using two different decay
#' factors.
#'
#' A common practice is to estimate covariance using a slower decay for
#' correlations and a faster decay for volatilities.
#'
#' @param x A numeric matrix containing the relevant risk drivers.
#' @param decay_low A numeric decay rate used to estimate correlations.
#' @param decay_high A numeric decay rate used to estimate volatilities.
#'
#' @return A list containing the target posterior mean and covariance matrix.
#'
#' @keywords internal
DoubleDecay <- function(x, decay_low, decay_high) {

  n_scenarios <- nrow(x)
  n_features  <- ncol(x)
  target_mean <- matrix(0, nrow = n_features, ncol = 1)
  correlation_probabilities <- exp_decay_probabilities(n_scenarios = n_scenarios, half_life = log(2) / decay_low)
  correlation_second_moment <- crossprod(x, sweep(x, MARGIN = 1, STATS = correlation_probabilities, FUN = "*"))

  correlation <- stats::cov2cor(correlation_second_moment)
  volatility_probabilities <- exp_decay_probabilities(n_scenarios = n_scenarios, half_life = log(2) / decay_high)
  volatility_second_moment <- crossprod(x, sweep(x, MARGIN = 1, STATS = volatility_probabilities, FUN = "*"))
  volatility <- sqrt(diag(volatility_second_moment))

  if (length(volatility) == 1L) {
    target_covariance <- volatility * correlation * volatility
  } else {
    target_covariance <- (diag(volatility) %*% correlation %*% diag(volatility))
  }

  list(m = target_mean, s = target_covariance)
}
