#' Numerical Entropy Minimization
#'
#' This function solves the entropy minimization problem with equality and inequality
#' constraints. The solution is a vector of posterior probabilities that distorts
#' the least the prior (equal-weights probabilities), given the constraints.
#'
#' @param p A vector of prior probabilities
#' @param A The linear inequality constraint (left-hand side)
#' @param b The linear inequality constraint (right-hand side)
#' @param Aeq The linear equality constraint (left-hand side)
#' @param beq The linear equality constraint (right-hand side)
#'
#' @return A vector of posterior probabilities.
#'
#' @export
#'
#' @examples
#' #
entropy_pooling <- function(p, A = NULL, b = NULL, Aeq, beq) {

  if (!is.matrix(p)) {
    p <- matrix(p, ncol = 1)
  }
  if (is.vector(b)) {
    b <- matrix(b, nrow = length(b))
  }
  if (is.vector(beq)) {
    beq <- matrix(beq, nrow = length(beq))
  }
  if (!length(b)) {
    A <- matrix(NA_real_, nrow = 0, ncol = 0)
  }
  if (!length(b)) {
    b <- matrix(NA_real_, nrow = 0, ncol = 0)
  }

  K_ <- nrow(A)
  K  <- nrow(Aeq)
  A_   <- t(A)
  b_   <- t(b)
  Aeq_ <- t(Aeq)
  beq_ <- t(beq)
  x0   <- matrix(0, nrow = K_ + K, ncol = 1)

  InqMat <- -diag(1, K_ + K)
  InqMat <- InqMat[-c(K_ + 1:nrow(InqMat)), ]
  InqVec <- matrix(0, K_, 1)

  if (!K_) {

    nestedfunU <- function(v, p, Aeq_, beq_) {
      x  <- exp(log(p) - 1 - Aeq_ %*% v)
      x  <- apply(cbind(x, 10 ^ -32), 1, max)
      L  <- t(x) %*% (log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v # formula (88)
      -L
    }

    opts <- suppressWarnings(
      pracma::fmincon(
        x0 = x0,
        fn = nestedfunU,
        p = p, Aeq_ = Aeq_, beq_ = beq_,
        tol = 1e-16,
        maxiter = 10000,
        maxfeval = 10000
      )
    )

    v  <- opts$par
    p_ <- exp(log(p) - 1 - Aeq_ %*% v)

  } else {

    nestedfunC <- function(lv, K_, p, A_, Aeq_, .A, .b, .Aeq, .beq) {
      lv <- as.matrix(lv)
      l  <- lv[1:K_, , drop = FALSE]
      v  <- lv[(K_ + 1):length(lv), , drop = FALSE]
      x  <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
      x  <- apply(cbind(x, 10 ^ -32), 1, max)
      L  <- t(x) %*% (log(x) - log(p)) + t(l) %*% (.A %*% x - .b) + t(v) %*% (.Aeq %*% x - .beq)
      - L
    }

    opts <- suppressWarnings(
      pracma::fmincon(
        x0 = x0,
        fn = nestedfunC,
        K_ = K_, A_ = A_, Aeq_ = Aeq_, p = p, .A = A, .b = b, .Aeq = Aeq, .beq = beq,
        A = InqMat,
        b = InqVec,
        tol = 1e-16,
        maxiter = 10000,
        maxfeval = 10000
      )
    )

    lv <- matrix(opts$par , ncol = 1)
    l  <- lv[1:K_, , drop = FALSE]
    v  <- lv[(K_ + 1):nrow(lv), , drop = FALSE]
    p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)

  }

  if (any(p_ < 0)) {
    p_[p_ < 0] <- 1e-32
  }
  if (sum(p_) < 0.9998 && sum(p_) > 1.0002) {
    p_ <- p_ / sum(p_)
  }

  p_

}