#' Least Information Kernel-Smoothing
#'
#' This is an internal function that uses the kernel-smoothing approach to compute
#' the posterior probabilities that satisfies some macroeconomic statement.
#'
#' @param Y A \code{matrix} with the macroeconomic indicators.
#' @param y A \code{numeric} scalar with the target to which \code{Y} is expected
#' to be near by.
#' @param h2 A \code{matrix} with the covariance of the \code{Y} factor.
#'
#' @return A vector with the new probability distribution.
#'
#' @keywords internal
least_info_kernel <- function(Y, y, h2) {

  T_ <- NROW(Y)
  N  <- NCOL(Y)

  # constrain probabilities to sum to one...
  Aeq <- matrix(1, nrow = 1, ncol = T_)
  beq <- 1

  # ...constrain the first moments...
  Aeq <- rbind(Aeq, t(Y))
  beq <- rbind(beq, y)

  if (!is.null(h2)) {

    # ...constrain the second moments...
    SecMom <- h2 + y %*% t(y)

    for (k in 1:N) {
      for (l in k:N) {
        Aeq <- rbind(Aeq, (Y[ , k] * Y[ , l]))
        beq <- rbind(beq, SecMom[k, l])
      }
    }

  }

  # equal weights prior
  p_0 <- matrix(1, nrow = T_, ncol = 1) / T_

 # compute posterior probabilities
 entropy_pooling(p = p_0, A = NULL, b = NULL, Aeq = Aeq, beq = beq, solver = "nlminb")

}
