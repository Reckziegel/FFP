#' Double-Decay Covariance Matrix
#'
#' This function computes the covariance matrix using two different decay factors.
#'
#' A common practice is to estimate the covariance of the risk drivers using a high
#' decay (short half-life) for the volatilities and a low decay (long half-life)
#' for the correlations.
#'
#' @param x A set of relevant risk drivers.
#' @param decay_low A \code{numeric} value with the low decay (long half-life).
#' @param decay_high A \code{numeric} value with the high decay (short half-life).
#'
#' @return A \code{list} with the posterior mean ans sigma.
#'
#' @keywords internal
DoubleDecay <- function(x, decay_low, decay_high) {

  T_ <- nrow(x)
  N  <- ncol(x)

  m <- matrix(0, nrow = N, ncol = 1)

  p_c <- exp(-decay_low * (T_ - t(rbind(1:T_))))
  p_c <- kronecker(matrix(1, 1, N), p_c / sum(p_c))
  S_1 <- t(p_c * x) %*% x
  C   <- stats::cov2cor(S_1)

  p_s <- exp(-decay_high * (T_ - t(rbind(1:T_))))
  p_s <- kronecker(matrix(1, 1, N), p_s / sum(p_s))
  S_2 <- t(p_s * x) %*% x
  s   <- sqrt(diag(S_2))

  if (length(s) == 1) {
    S <- s * C * s
  } else {
    S <- diag(s) %*% C %*% diag(s)
  }

  list(m = m, s = S)

}
