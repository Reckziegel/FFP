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
#' \code{\link[stats]{nlminb}}, \code{\link[NlcOptim]{solnl}}, \code{\link[nloptr]{nloptr}}
#' and the examples bellow.
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
#'
#' @examples
#' # setup
#' ret <- diff(log(EuStockMarkets))
#' n   <- nrow(ret)
#'
#' # View on expected returns (here is 2% for each asset)
#' mean <- rep(0.02, 4)
#'
#' # Prior probabilities (usually equal weight scheme)
#' prior <- rep(1 / n, n)
#'
#' # View
#' views <- view_on_mean(x = ret, mean = mean)
#'
#' # Optimization
#' ep <- entropy_pooling(
#'  p      = prior,
#'  Aeq    = views$Aeq,
#'  beq    = views$beq,
#'  solver = "nlminb"
#' )
#' ep
#'
#' ### Using the ... argument to control the optimization parameters
#'
#' # nlminb
#' ep <- entropy_pooling(
#'  p      = prior,
#'  Aeq    = views$Aeq,
#'  beq    = views$beq,
#'  solver = "nlminb",
#'  control = list(
#'      eval.max = 1000,
#'      iter.max = 1000,
#'      trace    = TRUE
#'    )
#' )
#' ep
#'
#' # nloptr
#' ep <- entropy_pooling(
#'  p      = prior,
#'  Aeq    = views$Aeq,
#'  beq    = views$beq,
#'  solver = "nloptr",
#'  control = list(
#'      xtol_rel = 1e-10,
#'      maxeval  = 1000,
#'      check_derivatives = TRUE
#'    )
#' )
#' ep
entropy_pooling <- function(p, A = NULL, b = NULL, Aeq = NULL, beq = NULL, solver = "nlminb", ...) {

  call   <- rlang::caller_env()
  solver <- rlang::arg_match(solver, c("nlminb", "solnl", "nloptr"))

  problem <- build_legacy_entropy_problem(p = p, A = A, b = b, Aeq = Aeq,beq = beq, call = call)

  if (solver == "nlminb" && nrow(problem$a_ineq) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Inequalities can only be solved with {.fn solnl} or {.fn nloptr}.",
        "i" = "Use {.code solver = \"solnl\"} or {.code solver = \"nloptr\"} for inequality constraints."
        )
      )
  }

  p <- matrix(problem$prior, ncol = 1)
  A <- problem$a_ineq
  b <- matrix(problem$b_ineq, ncol = 1)
  Aeq <- problem$a_eq
  beq <- matrix(problem$b_eq, ncol = 1)
  K_  <- nrow(A)
  K   <- nrow(Aeq)
  A_  <- t(A)
  b_  <- t(b)
  Aeq_ <- t(Aeq)
  beq_ <- t(beq)

  x0 <- matrix(0, nrow = K_ + K, ncol = 1)

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

normalize_legacy_entropy_matrix <- function(x, n_scenarios, arg, call = rlang::caller_env()) {

  if (is.null(x)) {
    return(empty_constraint_matrix(n_scenarios))
  }

  if (is.numeric(x) && is.null(dim(x))) {
    x <- matrix(x, nrow = 1L)
  }

  if (!is.matrix(x) || !is.numeric(x)) {
    ffp_abort(
      paste0("`", arg, "` must be a numeric matrix or vector."),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  if (ncol(x) != n_scenarios) {
    ffp_abort(
      c(
        paste0("`", arg, "` has an invalid scenario dimension."),
        "x" = paste0("Expected ", n_scenarios, " column(s), but found ", ncol(x), ".")
      ),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  if (anyNA(x) || any(!is.finite(x))) {
    ffp_abort(
      paste0("`", arg, "` must contain only finite values."),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  storage.mode(x) <- "double"
  dimnames(x)     <- NULL

  x
}


normalize_legacy_entropy_rhs <- function(x, n_constraints, arg, call = rlang::caller_env()) {

  if (is.null(x)) {
    x <- numeric()
  }

  if (!is.numeric(x)) {
    ffp_abort(
      paste0("`", arg, "` must be numeric."),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  x <- as.double(x)

  if (length(x) != n_constraints) {
    ffp_abort(
      c(
        paste0("`", arg, "` has an invalid length."),
        "x" = paste0("Expected ", n_constraints, " value(s), but found ", length(x), ".")
      ),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  if (anyNA(x) || any(!is.finite(x))) {
    ffp_abort(
      paste0("`", arg, "` must contain only finite values."),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  x
}

legacy_entropy_constraint_metadata <- function(n_equalities, n_inequalities) {

  make_legacy_metadata <- function(type, n_constraints) {
    if (n_constraints == 0L) {
      return(empty_constraint_metadata())
    }
    new_constraint_metadata(
      type = type,
      method = "legacy",
      feature_1 = rep(NA_character_, n_constraints),
      label = paste0("Legacy ", type, " ", seq_len(n_constraints))
    )
  }

  equality_metadata   <- make_legacy_metadata(type = "equality", n_constraints = n_equalities)
  inequality_metadata <- make_legacy_metadata(type = "inequality", n_constraints = n_inequalities)

  metadata <- rbind(equality_metadata, inequality_metadata)
  reindex_constraint_metadata(metadata)
}

build_legacy_entropy_problem <- function(p, A = NULL, b = NULL, Aeq = NULL, beq = NULL, call = rlang::caller_env()) {

  if (!is.numeric(p)) {
    ffp_abort(
      "`p` must contain numeric prior probabilities.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  prior <- as.double(p)
  n_scenarios <- length(prior)

  validate_entropy_scenario_count(n_scenarios = n_scenarios, call = call)
  validate_entropy_prior(prior = prior, n_scenarios = n_scenarios, call = call)

  a_eq <- normalize_legacy_entropy_matrix(x = Aeq, n_scenarios = n_scenarios, arg = "Aeq", call = call)
  b_eq <- normalize_legacy_entropy_rhs(x = beq, n_constraints = nrow(a_eq), arg = "beq", call = call)
  a_ineq <- normalize_legacy_entropy_matrix(x = A, n_scenarios = n_scenarios, arg = "A", call = call)
  b_ineq <- normalize_legacy_entropy_rhs(x = b, n_constraints = nrow(a_ineq), arg = "b", call = call)

  metadata <- legacy_entropy_constraint_metadata(n_equalities = nrow(a_eq), n_inequalities = nrow(a_ineq))

  constraints <- new_ffp_constraints(
    n_scenarios = n_scenarios,
    a_eq = a_eq,
    b_eq = b_eq,
    a_ineq = a_ineq,
    b_ineq = b_ineq,
    metadata = metadata
  )

  build_entropy_problem_from_constraints(prior = prior, constraints = constraints, call = call)
}
