#' Double-Decay Covariance Matrix by Entropy-Pooling
#'
#' Internal function that computes the flexible probabilities given by
#' double-decay covariance matrix.
#'
#' @param X A \code{matrix} of size \code{TxN} with relevant the risk drivers.
#' @param m A \code{matrix} of size \code{Nx1} given by \code{\link{DoubleDecay}}
#' with risk drivers location.
#' @param S A \code{matrix} of size \code{NxN} given by \code{\link{DoubleDecay}}
#' with the dispersion matrix of the risk drivers.
#'
#' @return A \code{matrix} vector with the posterior probabilities.
#'
#' @export
#'
#' @keywords internal
Fit2Moms <- function(X, m, S) {

  T_ <- nrow(X)
  N  <- ncol(X)

  Aeq <- matrix(1, nrow = 1, ncol = T_) # constrain probabilities to sum to one...
  beq <- 1

  Aeq <- rbind(Aeq , t(X))  # ...constrain the first moments...
  beq <- rbind(beq, m)

  SecMom <- S + m %*% t(m)   #...constrain the second moments...

  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq , t(X[ , k] * X[ , l]))
      beq <- rbind(beq, SecMom[k, l])
    }
  }

  p_0 <- matrix(1, nrow = T_, ncol = 1) / T_

  # ...compute posterior probabilities
  entropy_pooling(p = p_0, A = NULL, b = NULL, Aeq = Aeq, beq = beq)

}
