#' Numerical Entropy Minimization
#'
#' This function solves the entropy minimization problem with equality and inequality
#' constraints. The solution is a vector of posterior probabilities that distorts
#' the least the prior (equal-weights probabilities) given the constraints (views on
#' the market).
#'
#' When imposing views constraints there is no need to specify the non-negativity
#' constraint for probabilities, which is done automatically by `entropy_pooling`.
#'
#' For the arguments accepted in \code{...}, please see the documentation of
#' \code{\link[stats]{nlminb}}, \code{\link[NlcOptim]{solnl}} and \code{\link[nloptr]{nloptr}}.
#'
#' @param p A vector of prior probabilities.
#' @param A The linear inequality constraint (left-hand side).
#' @param b The linear inequality constraint (right-hand side).
#' @param Aeq The linear equality constraint (left-hand side).
#' @param beq The linear equality constraint (right-hand side).
#' @param solver A \code{character}. One of: "nlminb", "solnl" or "nloptr".
#' @param ... Further arguments passed to one of the solvers.
#'
#' @return A vector of posterior probabilities.
#' @export
entropy_pooling <- function(p, A = NULL, b = NULL, Aeq = NULL, beq = NULL, solver = c("nlminb", "solnl", "nloptr"), ...) {

  assertthat::assert_that(assertthat::is.string(solver))

  if (solver == "nlminb" & (!is.null(A) | !is.null(b))) {
    rlang::abort("Inequalities can only be solved with `solnl` or `nloptr`.")
  }
  solver <- rlang::arg_match(solver, c("nlminb", "solnl", "nloptr"))

  if (!is.matrix(p)) {
    p <- matrix(p, ncol = 1)
  }
  if (is.vector(b)) {
    b <- matrix(b, nrow = vctrs::vec_size(b))
  }
  if (!vctrs::vec_size(b)) {
    b <- matrix(NA_real_, nrow = 0, ncol = 0)
  }
  if (is.vector(beq)) {
    beq <- matrix(beq, nrow = vctrs::vec_size(beq))
  }
  if (!vctrs::vec_size(b)) {
    A <- matrix(NA_real_, nrow = 0, ncol = 0)
  }

  # non-negativiy constraint
  Aeq_non_neg <- matrix(1, nrow = 1, ncol = vctrs::vec_size(p))
  beq_non_neg <- 1

  Aeq <- rbind(Aeq, Aeq_non_neg)
  beq <- rbind(beq, beq_non_neg)

  K_ <- nrow(A)
  K  <- nrow(Aeq)
  A_   <- t(A)
  b_   <- t(b)
  Aeq_ <- t(Aeq)
  beq_ <- t(beq)
  x0   <- matrix(0, nrow = K_ + K, ncol = 1)

  # Equalities Constraint
  if (!K_) {

    if (solver == "nlminb") {

      # objective and gradient
      objective <- function(v, p, Aeq, beq, Aeq_, beq_){
        x <- exp(log(p) - 1 - Aeq_ %*% v)
        x[x < 10e-33] <- 10e-33
        L <- crossprod(x, log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v
        -L
      }
      gradient <- function(v, p, Aeq, beq, Aeq_, beq_){
        x <- exp(log(p) - 1 - Aeq_ %*% v)
        beq - Aeq %*% x
      }

      p_ <- ep_nlminb(p = p, Aeq = Aeq, beq = beq, Aeq_ = Aeq_, beq_ = beq_, objective = objective, gradient = gradient, ...)

      # Solving equalities constraint with solnl
    } else if (solver == "solnl") {

      nestedfunU_solnl <- function(v, p, Aeq_, beq_) {
        x  <- exp(log(p) - 1 - Aeq_ %*% v)
        x[x < 10e-33] <- 10e-33
        L <- crossprod(x, log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v
        -L
      }

      ep_solnl <- ep_solnl(
        x0  = x0,
        fn  = nestedfunU_solnl,
        ... = ...,
        p   = p, Aeq_ = Aeq_, beq_ = beq_,
        tolX = 1e-10, tolFun = 1e-10, tolCon = 1e-10, maxIter = 10000
      )
      v  <- ep_solnl$par
      p_ <- exp(log(p) - 1 - Aeq_ %*% v)

      # Solving equalities contraint with nloptr
    } else {

      ep_nloptr <- nloptr::auglag(
        x0 = x0,
        fn = function(v) {
          x <- exp(log(p) - 1 - Aeq_ %*% v)
          x[x < 10e-33] <- 10e-33
          L <- crossprod(x, log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v
          -L
        },
        gr = function(v) {
          x <- exp(log(p) - 1 - Aeq_ %*% v)
          beq - Aeq %*% x
        },
        localsolver = "SLSQP", ...)

      v <- ep_nloptr$par
      p_ <- exp(log(p) - 1 - Aeq_ %*% v)

    }

    # Inequalities can only be solved with `solnl` or `nloptr`
  } else {

    InqMat <- -diag(1, K_ + K)
    InqMat <- InqMat[-c(K_ + 1:nrow(InqMat)), ]
    InqVec <- matrix(0, K_, 1)

    # Solving inequalities with solnl
    if (solver == "solnl") {

      nestedfunC_solnl <- function(lv, K_, p, A_, Aeq_, .A, .b, .Aeq, .beq) {
        lv <- as.matrix(lv)
        l  <- lv[1:K_, , drop = FALSE]
        v  <- lv[(K_ + 1):length(lv), , drop = FALSE]
        x  <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
        x[x < 10e-33] <- 10e-33
        L  <- crossprod(x, log(x) - log(p)) + crossprod(l, .A %*% x - .b) + crossprod(v, .Aeq %*% x - .beq)
        - L
      }

      ep_solnl <- ep_solnl(
        x0 = x0,
        fn = nestedfunC_solnl,
        K_ = K_, A_ = A_, Aeq_ = Aeq_, p = p, .A = A, .b = b, .Aeq = Aeq, .beq = beq,
        A  = if (is.null(dim(InqMat))) matrix(InqMat, nrow = 1) else as.matrix(InqMat),
        b  = InqVec,
        tolX = 1e-10, tolFun = 1e-10, tolCon = 1e-10,maxIter = 10000, ... = ...
      )

      lv <- matrix(ep_solnl$par , ncol = 1)
      l  <- lv[1:K_, , drop = FALSE]
      v  <- lv[(K_ + 1):nrow(lv), , drop = FALSE]
      p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)

      # Solving inequalities with nloptr
    } else {

      ep_nloptr <- nloptr::slsqp(
        x0 = x0,
        fn = function(lv) {
          lv <- as.matrix(lv)
          l  <- lv[1:K_ , , drop = FALSE]
          v  <- lv[(K_ + 1):length(lv) , , drop = FALSE]
          x  <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
          x[x <= 10e-33] <- 10e-33
          L <- crossprod(x, log(x) - log(p)) + crossprod(l, A %*% x - b) + crossprod(v, Aeq %*% x - beq)
          - L
        },
        ...)
      lv <- matrix(ep_nloptr$par, ncol = 1)
      l  <- lv[1:K_ , , drop = FALSE]
      v  <- lv[(K_ + 1):nrow(lv) , , drop = FALSE]
      p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)

    }

  }

  if (any(p_ < 0)) {
    p_[p_ < 0] <- 1e-32
  }
  if (sum(p_) < 0.9999 || sum(p_) > 1.0001) {
    p_ <- p_ / sum(p_)
  }

  new_ffp(as.double(p_))

}

#' @keywords internal
ep_solnl <- function(x0, fn, gr = NULL, ...,
                     A = NULL, b = NULL,
                     Aeq = NULL, beq = NULL,
                     lb = NULL, ub = NULL,
                     tolX , tolFun, tolCon, maxIter ) {

  fun <- match.fun(fn)
  fn  <- function(x) fun(x, ...)

  sol <- suppressWarnings(
    NlcOptim::solnl(X = x0, objfun = fn, A = A, B = b, Aeq = Aeq, Beq = beq,
                    lb = lb, ub = ub, tolX = tolX, maxIter = maxIter, tolFun = tolFun, tolCon = tolFun)
  )

  sol

}

#' @keywords internal
ep_nlminb <- function(p, Aeq, beq, Aeq_, beq_, objective, gradient, ...) {
  v_dual <- stats::nlminb(
    start     = rep(0, NROW(Aeq)),
    objective = objective,
    gradient  = gradient,
    Aeq       = Aeq,
    beq       = beq,
    Aeq_      = Aeq_,
    beq_      = beq_,
    p         = p,
    ...       = ...)
  v <- v_dual$par
  p_ <- exp(log(p) - 1 - Aeq_ %*% v)
  p_
}

# @keywords internal
# ep_optim <- function(p, Aeq, beq, objective, gradient) {
#   v_dual <- stats::optim(
#     par    = rep(0, NROW(Aeq)),
#     fn     = objective,
#     gr     = gradient,
#     Aeq    = Aeq,
#     beq    = beq,
#     p      = p,
#     method = "BFGS"
#   )
#   v  <- v_dual$par
#   p_ <- exp(log(p) - 1 - t(Aeq) %*% v)
#   p_
# }



# gr = function(lv, v) {
#   lv <- as.matrix(lv)
#   l  <- lv[1:K_ , , drop = FALSE]
#   v  <- lv[(K_ + 1):length(lv) , , drop = FALSE]
#   x  <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
#   x  <- apply(cbind(x, 1e-32), 1, max)
#   rbind(b - A %*% x, beq - Aeq %*% x)
# },
# hin    = function(x) InqMat %*% x,
# hinjac = function(x) InqMat,
