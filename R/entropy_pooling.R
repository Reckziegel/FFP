#' Entropy Pooling
#'
#' Solves the relative-entropy minimization problem under linear equality and
#' inequality constraints.
#'
#' Given prior probabilities \eqn{q}, `entropy_pooling()` finds posterior
#' probabilities \eqn{p} that minimize the Kullback-Leibler divergence
#'
#' \deqn{
#'   \sum_t p_t \log(p_t / q_t)
#' }
#'
#' subject to the supplied views,
#'
#' \deqn{
#'   A_{eq} p = b_{eq}
#' }
#'
#' and
#'
#' \deqn{
#'   A p \le b,
#' }
#'
#' together with non-negativity and normalization of the posterior
#' probabilities.
#'
#' `entropy_pooling()` is the low-level matrix interface retained for
#' compatibility with earlier versions of the package. New workflows should
#' generally use [ffp_model()], [ffp_prior()], [ffp_view()], and [ffp_fit()],
#' which provide a higher-level interface to the same Entropy Pooling core.
#'
#' @param p A numeric vector of prior probabilities.
#' @param A Optional matrix defining linear inequality constraints.
#' @param b Optional numeric vector containing the right-hand side of the
#'   inequality constraints.
#' @param Aeq Optional matrix defining linear equality constraints.
#' @param beq Optional numeric vector containing the right-hand side of the
#'   equality constraints.
#' @param solver Legacy solver identifier retained for backwards compatibility.
#'   One of `"nlminb"`, `"solnl"`, or `"nloptr"`. Entropy Pooling is now solved
#'   by the unified FFP solver regardless of this value.
#' @param ... Legacy arguments retained for backwards compatibility.
#'
#' @return An `ffp` vector containing the posterior probabilities.
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
#' full_posterior <- entropy_pooling(
#'  p      = prior,
#'  Aeq    = views$Aeq,
#'  beq    = views$beq
#' )
#' full_posterior
entropy_pooling <- function(p, A = NULL, b = NULL, Aeq = NULL, beq = NULL, solver = "nlminb", ...) {

  call <- rlang::caller_env()
  rlang::arg_match(solver, c("nlminb", "solnl", "nloptr"))

  problem  <- build_legacy_entropy_problem(p = p, A = A, b = b, Aeq = Aeq, beq = beq, call = call)
  solution <- solve_entropy_problem(problem = problem, call = call)

  new_ffp(solution$posterior)
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
