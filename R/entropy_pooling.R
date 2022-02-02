#' Numerical Entropy Minimization
#'
#' This function solves the entropy minimization problem with equality and inequality
#' constraints. The solution is a vector of posterior probabilities that distorts
#' the least the prior (equal-weights probabilities), given the constraints.
#'
#' @param p A vector of prior probabilities.
#' @param A The linear inequality constraint (left-hand side).
#' @param b The linear inequality constraint (right-hand side).
#' @param Aeq The linear equality constraint (left-hand side).
#' @param beq The linear equality constraint (right-hand side).
#' @param solver A \code{character}. One of: "optim", "nlminb", "solnl" and "nloptr".
#' @param ... Further arguments passed to one of the solvers above.
#'
#' @return A vector of posterior probabilities.
#' @export
#'
#' @examples
#' #
entropy_pooling <- function(p, A = NULL, b = NULL, Aeq, beq, solver = c("optim", "nlminb", "solnl", "nloptr"), ...) {

  assertthat::assert_that(assertthat::is.string(solver))
  if (solver %in% c("optim", "nlminb") & (!is.null(A) | !is.null(b))) {
    stop("Inequalities can only be solved with `solnl` or `nloptr`.", call. = FALSE)
  }
  solver <- rlang::arg_match(solver, c("optim", "nlminb", "solnl", "nloptr"))

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

    if (solver %in% c("optim", "nlminb")) {

      # objective and gradient
      objective <- function(v, p, Aeq, beq){
        x <- exp(log(p) - 1 - t(Aeq) %*% v)
        x <- apply(cbind(x, 10 ^ -32), 1, max)
        L <- t(x) %*% (log(x) - log(p) + t(Aeq) %*% v) - t(beq) %*% v
        -L
      }
      gradient <- function(v, p, Aeq, beq){
        x <- exp(log(p) - 1 - t(Aeq) %*% v)
        beq - Aeq %*% x
      }

      # Solving with optim
      if (solver == "optim") {

        p_ <- ep_optim(p = p, Aeq = Aeq, beq = beq, objective = objective, gradient = gradient)

        # Solving with nlminb
      } else {

        p_ <- ep_nlminb(p = p, Aeq = Aeq, beq = beq, objective = objective, gradient = gradient)

      }

      # Solving equalities constraint with solnl
    } else if (solver == "solnl") {

      nestedfunU <- function(v, p, Aeq_, beq_) {
        x  <- exp(log(p) - 1 - Aeq_ %*% v)
        x  <- apply(cbind(x, 10 ^ -32), 1, max)
        L  <- t(x) %*% (log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v # formula (88)
        -L
      }

      ep_solnl <- ep_solnl(
        x0  = x0,
        fn  = nestedfunU,
        p   = p, Aeq_ = Aeq_, beq_ = beq_,
        tolX = 1e-10, tolFun = 1e-10, tolCon = 1e-10,maxIter = 10000
      )
      v  <- ep_solnl$par
      p_ <- exp(log(p) - 1 - Aeq_ %*% v)

      # Solving equalities contraint with nloptr
    } else {

      eval_f_list <- function(v) {
        x <- exp(log(p) - 1 - Aeq_ %*% v)
        x <- apply(cbind(x , 1e-32), 1, max)
        L <- t(x) %*% (log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v
        L <- -L # take negative values since we want to maximize
        # evaluate gradient
        gradient <- beq - Aeq %*% x
        list(objective = L, gradient = gradient)
      }

      ep_nloptr <- nloptr::nloptr(x0 = x0, eval_f = eval_f_list,
                                  opts = list(algorithm = "NLOPT_LD_LBFGS" ,
                                              xtol_rel = 1e-5 ,
                                              check_derivatives = TRUE,
                                              maxeval = 1000))
      v <- ep_nloptr$solution
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
        x  <- apply(cbind(x, 1e-32), 1, max)
        L  <- t(x) %*% (log(x) - log(p)) + t(l) %*% (.A %*% x - .b) + t(v) %*% (.Aeq %*% x - .beq)
        - L
      }

      ep_solnl <- ep_solnl(
        x0 = x0,
        fn = nestedfunC_solnl,
        K_ = K_, A_ = A_, Aeq_ = Aeq_, p = p, .A = A, .b = b, .Aeq = Aeq, .beq = beq,
        A = as.matrix(InqMat), #if (vctrs::vec_size(b) > 1) InqMat else t(InqMat),
        b = InqVec,
        tolX = 1e-10, tolFun = 1e-10, tolCon = 1e-10,maxIter = 10000
      )

      lv <- matrix(ep_solnl$par , ncol = 1)
      l  <- lv[1:K_, , drop = FALSE]
      v  <- lv[(K_ + 1):nrow(lv), , drop = FALSE]
      p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)

      # Solving inequalities with nloptr
    } else {

      InqConstraint <- function(x) InqMat %*% x
      jacobian_constraint <- function(x) InqMat

      nestedfunC <- function(lv) {
        lv <- as.matrix(lv)
        l  <- lv[1:K_ , , drop = FALSE] # inequality Lagrange multiplier
        v  <- lv[(K_ + 1):length(lv) , , drop = FALSE] # equality Lagrange multiplier
        x  <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
        x  <- apply(cbind(x, 1e-32), 1, max)

        L <- t(x) %*% (log(x) - log(p)) + t(l) %*% (A %*% x - b) + t(v) %*% (Aeq %*% x - beq)
        objective = -L

        # calculate the gradient
        gradient <- rbind(b - A %*% x, beq - Aeq %*% x)

        list(objective = objective, gradient = gradient)

      }

      local_opts <- list( algorithm = "NLOPT_LD_SLSQP",
                          xtol_rel = 1e-10 ,
                          check_derivatives = TRUE
      )
      ep_nloptr = nloptr::nloptr( x0 = x0 ,
                                  eval_f = nestedfunC ,
                                  eval_g_ineq = InqConstraint ,
                                  eval_jac_g_ineq = jacobian_constraint ,
                                  opts = list(algorithm = "NLOPT_LD_AUGLAG",
                                              local_opts = local_opts,
                                              maxeval = 1000,
                                              check_derivatives = TRUE,
                                              xtol_rel = 1e-10))

      lv <- matrix(ep_nloptr$solution, ncol = 1)
      l  <- lv[1:K_ , , drop = FALSE]
      v  <- lv[(K_ + 1):nrow(lv) , , drop = FALSE]
      p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)

    }

  }

  if (any(p_ < 0)) {
    p_[p_ < 0] <- 1e-32
  }
  # if (sum(p_) < 0.99999999 && sum(p_) > 1.00000001) {
  #   p_ <- p_ / sum(p_)
  # }

  as_ffp(as.double(p_ / sum(p_)))

}

#' @keywords internal
ep_solnl <- function(x0, fn, gr = NULL, ...,
                     A = NULL, b = NULL,
                     Aeq = NULL, beq = NULL,
                     lb = NULL, ub = NULL,
                     tolX , tolFun, tolCon, maxIter ) {

  fun <- match.fun(fn)
  fn  <- function(x) fun(x, ...)

  sol <- NlcOptim::solnl(X = x0, objfun = fn, A = A, B = b, Aeq = Aeq, Beq = beq,
                         lb = lb, ub = ub, tolX = tolX, maxIter = maxIter, tolFun = tolFun, tolCon = tolFun)
  sol

}

#' @keywords internal
ep_nlminb <- function(p, Aeq, beq, objective, gradient) {
  v_dual <- stats::nlminb(
    start     = rep(0, NROW(Aeq)),
    objective = objective,
    gradient  = gradient,
    Aeq       = Aeq,
    beq       = beq,
    p         = p)
  v <- v_dual$par
  v <- v_dual$par
  p_ <- exp(log(p) - 1 - t(Aeq) %*% v)
  p_
}

#' @keywords internal
ep_optim <- function(p, Aeq, beq, objective, gradient) {
  v_dual <- stats::optim(
    par    = rep(0, NROW(Aeq)),
    fn     = objective,
    gr     = gradient,
    Aeq    = Aeq,
    beq    = beq,
    p      = p,
    method = "BFGS"
  )
  v  <- v_dual$par
  p_ <- exp(log(p) - 1 - t(Aeq) %*% v)
  p_
}



# entropy_pooling <- function(p, A = NULL, b = NULL, Aeq, beq) {
#
#   if (!is.matrix(p)) {
#     p <- matrix(p, ncol = 1)
#   }
#   if (is.vector(b)) {
#     b <- matrix(b, nrow = vctrs::vec_size(b))
#   }
#   if (!vctrs::vec_size(b)) {
#     b <- matrix(NA_real_, nrow = 0, ncol = 0)
#   }
#   if (is.vector(beq)) {
#     beq <- matrix(beq, nrow = vctrs::vec_size(beq))
#   }
#   if (!vctrs::vec_size(b)) {
#     A <- matrix(NA_real_, nrow = 0, ncol = 0)
#   }
#
#   K_ <- nrow(A)
#   K  <- nrow(Aeq)
#   A_   <- t(A)
#   b_   <- t(b)
#   Aeq_ <- t(Aeq)
#   beq_ <- t(beq)
#   x0   <- matrix(0, nrow = K_ + K, ncol = 1)
#
#   InqMat <- -diag(1, K_ + K)
#   InqMat <- InqMat[-c(K_ + 1:nrow(InqMat)), ]
#   InqVec <- matrix(0, K_, 1)
#
#   if (!K_) {
#
#     nestedfunU <- function(v, p, Aeq_, beq_) {
#       x  <- exp(log(p) - 1 - Aeq_ %*% v)
#       x  <- apply(cbind(x, 10 ^ -32), 1, max)
#       L  <- t(x) %*% (log(x) - log(p) + Aeq_ %*% v) - beq_ %*% v # formula (88)
#       -L
#     }
#
#     opts <- ep_optimization(
#       x0  = x0,
#       fn  = nestedfunU,
#       p   = p, Aeq_ = Aeq_, beq_ = beq_,
#       tol = 1e-10
#     )
#
#     v  <- opts$par
#     p_ <- exp(log(p) - 1 - Aeq_ %*% v)
#
#   } else {
#
#     nestedfunC <- function(lv, K_, p, A_, Aeq_, .A, .b, .Aeq, .beq) {
#       lv <- as.matrix(lv)
#       l  <- lv[1:K_, , drop = FALSE]
#       v  <- lv[(K_ + 1):length(lv), , drop = FALSE]
#       x  <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
#       x  <- apply(cbind(x, 10 ^ -32), 1, max)
#       L  <- t(x) %*% (log(x) - log(p)) + t(l) %*% (.A %*% x - .b) + t(v) %*% (.Aeq %*% x - .beq)
#       - L
#     }
#
#     opts <- ep_optimization(
#       x0 = x0,
#       fn = nestedfunC,
#       K_ = K_, A_ = A_, Aeq_ = Aeq_, p = p, .A = A, .b = b, .Aeq = Aeq, .beq = beq,
#       A = if (vctrs::vec_size(b) > 1) InqMat else t(InqMat),
#       b = InqVec,
#       tol = 1e-10
#     )
#
#     lv <- matrix(opts$par , ncol = 1)
#     l  <- lv[1:K_, , drop = FALSE]
#     v  <- lv[(K_ + 1):nrow(lv), , drop = FALSE]
#     p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
#
#   }
#
#   if (any(p_ < 0)) {
#     p_[p_ < 0] <- 1e-32
#   }
#   # if (sum(p_) < 0.99999999 && sum(p_) > 1.00000001) {
#   #   p_ <- p_ / sum(p_)
#   # }
#
#   p_ / sum(p)
#
# }
#
# #@keywords internal
# ep_optimization <- function(x0, fn, gr = NULL, ...,
#                             A = NULL, b = NULL,
#                             Aeq = NULL, beq = NULL,
#                             lb = NULL, ub = NULL,
#                             tol) {
#
#   fun <- match.fun(fn)
#   fn  <- function(x) fun(x, ...)
#
#   sol <- NlcOptim::solnl(X = x0, objfun = fn, A = A, B = b, Aeq = Aeq, Beq = beq,
#                          lb = lb, ub = ub, tolX = tol, maxIter = 10000)
#   list(
#     par = c(sol$par),
#     value = sol$fn,
#     convergence = 0,
#     info = list(lambda = sol$lambda, grad = sol$grad, hessian = sol$hessian)
#   )
#
# }
