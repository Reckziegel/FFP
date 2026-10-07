# Solver control ----------------------------------------------------------

#' Control the Entropy Pooling solver
#'
#' `entropy_solver_control()` defines numerical controls used by the
#' full-confidence Entropy Pooling solver called by [ffp_fit()].
#'
#' The default values are intended to work well for typical FFP problems.
#' Most users should not need to change them. The controls are exposed for
#' cases that require stricter numerical accuracy, additional iterations, or
#' solver diagnostics.
#'
#' @param max_iterations Maximum number of iterations allowed for the numerical
#'   optimizer. Must be a positive whole number.
#' @param relative_tolerance Positive numerical tolerance controlling relative
#'   convergence of the dual objective.
#' @param gradient_tolerance Positive numerical tolerance used for convergence
#'   of the dual gradient when inequality constraints are present.
#' @param constraint_tolerance Positive numerical tolerance used to verify that
#'   the posterior satisfies normalization, equality, and inequality
#'   constraints.
#' @param refinement_iterations Maximum number of equality-refinement
#'   iterations performed after the main numerical optimization. Must be a
#'   positive whole number.
#'
#' @return A list of numerical controls consumed by [ffp_fit()] and the
#'   internal Entropy Pooling solver.
#'
#' @details
#' Entropy Pooling is solved numerically after the model's views have been
#' translated into equality and inequality constraints.
#'
#' The different tolerances have distinct roles:
#'
#' - `relative_tolerance` controls convergence of the dual objective;
#' - `gradient_tolerance` controls first-order convergence when inequality
#'   constraints are present;
#' - `constraint_tolerance` determines whether the resulting posterior is
#'   considered numerically consistent with the mathematical constraints.
#'
#' These tolerances affect numerical computation only. They do not relax or
#' modify the economic or statistical meaning of the views.
#'
#' Decreasing tolerances may improve numerical accuracy, but can require more
#' optimizer iterations and may expose conditioning problems in difficult or
#' nearly redundant systems of constraints.
#'
#' @examples
#' # Default controls
#' control <- entropy_solver_control()
#' control
#'
#' # Require stricter constraint verification
#' control <- entropy_solver_control(
#'   constraint_tolerance = 1e-9
#' )
#'
#' # Allow more iterations for a difficult problem
#' control <- entropy_solver_control(
#'   max_iterations = 20000L,
#'   refinement_iterations = 50L
#' )
#'
#' @seealso [ffp_fit()]
#'
#' @export
entropy_solver_control <- function(
    max_iterations = 10000L,
    relative_tolerance = 1e-12,
    gradient_tolerance = 1e-10,
    constraint_tolerance = 1e-8,
    refinement_iterations = 20L
) {
  control <- list(
    max_iterations = max_iterations,
    relative_tolerance = relative_tolerance,
    gradient_tolerance = gradient_tolerance,
    constraint_tolerance = constraint_tolerance,
    refinement_iterations = refinement_iterations
  )

  validate_entropy_solver_control(control)

  control
}


validate_entropy_solver_control <- function(control, call = rlang::caller_env()) {
  required <- c(
    "max_iterations",
    "relative_tolerance",
    "gradient_tolerance",
    "constraint_tolerance",
    "refinement_iterations"
  )

  if (!is.list(control) || !all(required %in% names(control))) {
    ffp_abort(
      "`control` must be a valid Entropy Pooling solver control object.",
      class = "ffp_error_invalid_solver_control",
      call = call
    )
  }

  integer_fields <- c("max_iterations", "refinement_iterations")

  valid_integers <- vapply(
    integer_fields,
    function(name) {
      value <- control[[name]]
      is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) && value >= 1 && value == floor(value)
    },
    logical(1)
  )

  if (!all(valid_integers)) {
    ffp_abort(
      "Solver iteration limits must be positive whole numbers.",
      class = "ffp_error_invalid_solver_control",
      call = call
    )
  }

  tolerance_fields <- c("relative_tolerance", "gradient_tolerance", "constraint_tolerance")

  valid_tolerances <- vapply(
    tolerance_fields,
    function(name) {
      value <- control[[name]]
      is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) && value > 0
    },
    logical(1)
  )

  if (!all(valid_tolerances)) {
    ffp_abort(
      "Solver tolerances must be finite positive numbers.",
      class = "ffp_error_invalid_solver_control",
      call = call
    )
  }

  invisible(control)
}


# Solver result -----------------------------------------------------------

new_ffp_solver_result <- function(
    posterior,
    objective,
    converged,
    status,
    backend,
    method,
    dual = NULL,
    counts = integer(),
    residuals,
    control = entropy_solver_control()
) {
  result <- structure(
    list(
      posterior = posterior,
      objective = objective,
      converged = converged,
      status = status,
      backend = backend,
      method = method,
      dual = dual,
      counts = counts,
      residuals = residuals,
      control = control
    ),
    class = "ffp_solver_result"
  )

  validate_ffp_solver_result(result)

  result
}


validate_ffp_solver_result <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "ffp_solver_result")) {
    ffp_abort(
      "The object must inherit from {.cls ffp_solver_result}.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  valid_posterior <- is.numeric(x$posterior) && is.null(dim(x$posterior)) && length(x$posterior) >= 1L && !anyNA(x$posterior) && all(is.finite(x$posterior)) && all(x$posterior >= 0)

  if (!valid_posterior) {
    ffp_abort(
      "`posterior` must contain finite, non-negative probabilities.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  valid_objective <- is.numeric(x$objective) && length(x$objective) == 1L && !is.na(x$objective) && is.finite(x$objective)

  if (!valid_objective) {
    ffp_abort(
      "`objective` must be a finite numeric scalar.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  valid_converged <- is.logical(x$converged) && length(x$converged) == 1L && !is.na(x$converged)

  if (!valid_converged) {
    ffp_abort(
      "`converged` must be a single logical value.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  character_fields <- c("status", "backend", "method")

  valid_character <- vapply(
    character_fields,
    function(name) {
      value <- x[[name]]
      is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
    },
    logical(1)
  )

  if (!all(valid_character)) {
    ffp_abort(
      "Solver status fields must be non-empty character values.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  if (!is.list(x$residuals)) {
    ffp_abort(
      "`residuals` must be a list.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  valid_control <- tryCatch(
    {
      validate_entropy_solver_control(control = x$control, call = call)
      TRUE
    },
    error = function(cnd) {
      FALSE
    }
  )

  if (!valid_control) {
    ffp_abort(
      "`control` must contain valid Entropy Pooling solver controls.",
      class = "ffp_error_invalid_solver_result",
      call = call
    )
  }

  invisible(x)
}

# Relative entropy --------------------------------------------------------

entropy_kl_divergence <- function(posterior, prior) {
  valid <- is.numeric(posterior) && is.numeric(prior) && is.null(dim(posterior)) &&
    is.null(dim(prior)) && length(posterior) == length(prior) && length(posterior) >= 1L &&
    !anyNA(posterior) && !anyNA(prior) && all(is.finite(posterior)) && all(is.finite(prior)) &&
    all(posterior >= 0) && all(prior >= 0)

  if (!valid) {
    return(NaN)
  }

  positive <- posterior > 0

  if (!any(positive)) {
    return(0)
  }

  if (any(prior[positive] == 0)) {
    return(Inf)
  }

  sum(posterior[positive] * log(posterior[positive] / prior[positive]))

}


# Posterior diagnostics ---------------------------------------------------

entropy_solution_residuals <- function(problem, posterior) {

  equality_residuals <- drop(problem$a_eq %*% posterior - problem$b_eq)
  inequality_residuals <- drop(problem$a_ineq %*% posterior - problem$b_ineq)

  max_equality_residual <- if (length(equality_residuals) == 0L) {
    0
  } else {
    max(abs(equality_residuals))
  }

  max_inequality_violation <- if (length(inequality_residuals) == 0L) {
    0
  } else {
    max(pmax(inequality_residuals, 0))
  }

  list(
    probability_sum = sum(posterior),
    normalization_residual = abs(sum(posterior) - 1),
    min_probability = min(posterior),
    max_equality_residual = max_equality_residual,
    max_inequality_violation = max_inequality_violation
  )
}


entropy_solution_is_valid <- function(problem, posterior, tolerance) {
  valid_probabilities <- is.numeric(posterior) &&
    is.null(dim(posterior)) &&
    length(posterior) == problem$n_scenarios &&
    !anyNA(posterior) &&
    all(is.finite(posterior))

  if (!valid_probabilities) {
    return(FALSE)
  }

  residuals <- entropy_solution_residuals(problem = problem, posterior = posterior)

  residuals$min_probability >= -tolerance &&
    residuals$normalization_residual <= tolerance &&
    residuals$max_equality_residual <= tolerance &&
    residuals$max_inequality_violation <= tolerance
}


validate_entropy_solution <- function(problem, posterior, tolerance, call = rlang::caller_env()) {
  if (!is.numeric(posterior) || !is.null(dim(posterior)) || length(posterior) != problem$n_scenarios || anyNA(posterior) || any(!is.finite(posterior))) {
    ffp_abort(
      "The solver returned an invalid posterior probability vector.",
      class = "ffp_error_invalid_posterior",
      call = call
    )
  }

  residuals <- entropy_solution_residuals(problem = problem, posterior = posterior)

  if (residuals$min_probability < -tolerance) {
    ffp_abort(
      c(
        "The solver returned negative posterior probabilities.",
        "x" = paste0(
          "Minimum probability: ",
          format(residuals$min_probability, scientific = TRUE),
          "."
        )
      ),
      class = "ffp_error_invalid_posterior",
      call = call
    )
  }

  if (residuals$normalization_residual > tolerance) {
    ffp_abort(
      c(
        "The posterior probabilities do not sum to one.",
        "x" = paste0(
          "Normalization residual: ",
          format(residuals$normalization_residual, scientific = TRUE),
          "."
        )
      ),
      class = "ffp_error_invalid_posterior",
      call = call
    )
  }

  if (residuals$max_equality_residual > tolerance) {
    ffp_abort(
      c(
        "The posterior does not satisfy all equality constraints.",
        "x" = paste0(
          "Maximum equality residual: ",
          format(residuals$max_equality_residual, scientific = TRUE),
          "."
        )
      ),
      class = "ffp_error_invalid_posterior",
      call = call
    )
  }

  if (residuals$max_inequality_violation > tolerance) {
    ffp_abort(
      c(
        "The posterior violates at least one inequality constraint.",
        "x" = paste0(
          "Maximum inequality violation: ",
          format(residuals$max_inequality_violation, scientific = TRUE),
          "."
        )
      ),
      class = "ffp_error_invalid_posterior",
      call = call
    )
  }

  invisible(residuals)
}


# Dual helpers ------------------------------------------------------------

entropy_view_equality_count <- function(problem) {
  nrow(problem$a_eq) - 1L
}


entropy_view_equality_rows <- function(problem) {
  n_equalities <- entropy_view_equality_count(problem)

  if (n_equalities == 0L) {
    return(integer())
  }

  seq_len(n_equalities)
}


split_entropy_dual_parameters <- function(parameters, problem) {

  n_inequalities <- nrow(problem$a_ineq)
  n_equalities <- entropy_view_equality_count(problem)

  lambda <- if (n_inequalities == 0L) {
    numeric()
  } else {
    parameters[seq_len(n_inequalities)]
  }

  nu <- if (n_equalities == 0L) {
    numeric()
  } else {
    start <- n_inequalities + 1L

    parameters[seq.int(start, length.out = n_equalities)]
  }

  list(lambda = lambda, nu = nu)
}


entropy_dual_state <- function(parameters, problem) {

  dual <- split_entropy_dual_parameters(parameters = parameters, problem = problem)

  linear_predictor <- numeric(problem$n_scenarios)

  if (length(dual$lambda) > 0L) {
    linear_predictor <- linear_predictor + drop(crossprod(problem$a_ineq, dual$lambda))
  }

  equality_rows <- entropy_view_equality_rows(problem)

  if (length(dual$nu) > 0L) {
    linear_predictor <- linear_predictor + drop(crossprod(problem$a_eq[equality_rows, , drop = FALSE], dual$nu))
  }

  positive_prior <- problem$prior > 0

  log_weights <- log(problem$prior[positive_prior]) - linear_predictor[positive_prior]
  shift <- max(log_weights)
  scaled_weights <- exp(log_weights - shift)
  posterior <- numeric(problem$n_scenarios)
  posterior[positive_prior] <- scaled_weights / sum(scaled_weights)

  list(
    posterior = posterior,
    log_normalizer = shift + log(sum(scaled_weights)),
    lambda = dual$lambda,
    nu = dual$nu
  )
}


# Dual objective ----------------------------------------------------------

entropy_dual_posterior <- function(parameters, problem) {
  entropy_dual_state(parameters = parameters, problem = problem)$posterior
}


entropy_negative_dual <- function(parameters, problem) {
  state <- entropy_dual_state(parameters = parameters, problem = problem)
  equality_rows <- entropy_view_equality_rows(problem)
  inequality_term <- if (length(state$lambda) == 0L) {
    0
  } else {
    sum(state$lambda * problem$b_ineq)
  }

  equality_term <- if (length(state$nu) == 0L) {
    0
  } else {
    sum(state$nu * problem$b_eq[equality_rows])
  }

  state$log_normalizer + inequality_term + equality_term
}


entropy_negative_dual_gradient <- function(parameters, problem) {
  state <- entropy_dual_state(parameters = parameters, problem = problem)
  posterior <- state$posterior

  inequality_gradient <- if (nrow(problem$a_ineq) == 0L) {
    numeric()
  } else {
    problem$b_ineq - drop(problem$a_ineq %*% posterior)
  }

  equality_rows <- entropy_view_equality_rows(problem)

  equality_gradient <- if (length(equality_rows) == 0L) {
    numeric()
  } else {
    problem$b_eq[equality_rows] - drop(problem$a_eq[equality_rows, , drop = FALSE] %*% posterior)
  }

  c(inequality_gradient, equality_gradient)
}


# Equality refinement -----------------------------------------------------

entropy_weighted_covariance <- function(constraints, posterior) {
  means <- drop(constraints %*% posterior)

  centered <- sweep(constraints, MARGIN = 1L, STATS = means, FUN = "-")
  weighted <- sweep(centered, MARGIN = 2L, STATS = sqrt(posterior), FUN = "*")

  weighted %*% t(weighted)
}


solve_psd_system <- function(matrix, rhs, tolerance = 1e-12) {
  decomposition <- eigen(matrix, symmetric = TRUE)
  scale <- max(abs(decomposition$values), 1)
  keep <- decomposition$values > tolerance * scale

  if (!any(keep)) {
    return(rep(0, length(rhs)))
  }

  vectors <- decomposition$vectors[, keep, drop = FALSE]
  values <- decomposition$values[keep]

  drop(vectors %*% (crossprod(vectors, rhs) / values))
}


refine_entropy_equalities <- function(parameters, problem, control) {
  n_inequalities <- nrow(problem$a_ineq)
  equality_rows <- entropy_view_equality_rows(problem)

  if (n_inequalities > 0L || length(equality_rows) == 0L) {
    return(parameters)
  }

  constraints <- problem$a_eq[equality_rows, , drop = FALSE]
  target  <- problem$b_eq[equality_rows]
  current <- parameters

  for (iteration in seq_len(control$refinement_iterations)) {
    state <- entropy_dual_state(parameters = current, problem = problem)
    posterior <- state$posterior
    gradient  <- target - drop(constraints %*% posterior)

    if (max(abs(gradient)) <= control$constraint_tolerance / 10) {
      break
    }

    hessian <- entropy_weighted_covariance(constraints = constraints, posterior = posterior)
    step    <- solve_psd_system(matrix = hessian, rhs = gradient)

    if (anyNA(step) || any(!is.finite(step)) || max(abs(step)) == 0) {
      break
    }

    current_objective <- entropy_negative_dual(parameters = current, problem = problem)

    step_size <- 1
    accepted  <- FALSE

    for (line_search in seq_len(30L)) {
      candidate <- current - step_size * step
      candidate_objective <- entropy_negative_dual(parameters = candidate, problem = problem)

      if (is.finite(candidate_objective) && candidate_objective <= current_objective) {
        current  <- candidate
        accepted <- TRUE
        break
      }

      step_size <- step_size / 2
    }

    if (!accepted) {
      break
    }
  }

  current
}


# Optimizer ---------------------------------------------------------------

run_entropy_dual_optimizer <- function(problem, control, call = rlang::caller_env()) {
  n_inequalities <- nrow(problem$a_ineq)
  n_equalities   <- entropy_view_equality_count(problem)

  initial <- rep(0, n_inequalities + n_equalities)

  if (length(initial) == 0L) {
    ffp_abort(
      "The entropy optimizer received a problem with no view constraints.",
      class = "ffp_error_solver_failure",
      call = call
    )
  }

  if (n_inequalities == 0L) {
    optimizer <- tryCatch(
      stats::optim(
        par = initial,
        fn  = entropy_negative_dual,
        gr  = entropy_negative_dual_gradient,
        problem = problem,
        method  = "BFGS",
        control = list(maxit = as.integer(control$max_iterations), reltol = control$relative_tolerance)
      ),
      error = function(cnd) {
        ffp_abort(
          c(
            "The Entropy Pooling optimizer failed.",
            "x" = conditionMessage(cnd)
          ),
          class = "ffp_error_solver_failure",
          call = call
        )
      }
    )

    optimizer$par <- refine_entropy_equalities(parameters = optimizer$par, problem = problem, control = control)

    return(list(optimizer = optimizer, method = "BFGS"))
  }

  lower <- c(rep(0, n_inequalities), rep(-Inf, n_equalities))
  factr <- max(control$relative_tolerance / .Machine$double.eps, 1)

  optimizer <- tryCatch(
    stats::optim(
      par = initial,
      fn = entropy_negative_dual,
      gr = entropy_negative_dual_gradient,
      problem = problem,
      method = "L-BFGS-B",
      lower = lower,
      control = list(maxit = as.integer(control$max_iterations), factr = factr, pgtol = control$gradient_tolerance)
    ),
    error = function(cnd) {
      ffp_abort(
        c(
          "The Entropy Pooling optimizer failed.",
          "x" = conditionMessage(cnd)
        ),
        class = "ffp_error_solver_failure",
        call = call
      )
    }
  )

  list(optimizer = optimizer, method = "L-BFGS-B")
}


# Solver ------------------------------------------------------------------

solve_entropy_problem <- function(problem, control = entropy_solver_control(), call = rlang::caller_env()) {
  validate_ffp_entropy_problem(problem, call = call)
  validate_entropy_solver_control(control, call = call)

  tolerance <- control$constraint_tolerance

  if (entropy_solution_is_valid(problem = problem, posterior = problem$prior, tolerance = tolerance)) {
    residuals <- entropy_solution_residuals(problem = problem, posterior = problem$prior)

    return(
      new_ffp_solver_result(
        posterior = problem$prior,
        objective = 0,
        converged = TRUE,
        status = "prior_satisfies_constraints",
        backend = "none",
        method = "none",
        dual = NULL,
        counts = integer(),
        residuals = residuals,
        control = control
      )
    )
  }

  optimized <- run_entropy_dual_optimizer(problem = problem, control = control, call = call)
  optimizer <- optimized$optimizer

  if (optimizer$convergence != 0L) {
    classification <- diagnose_entropy_problem(problem = problem, call = call)
    optimizer_message <- optimizer$message

    if (is.null(optimizer_message) || !nzchar(optimizer_message)) {
      optimizer_message <- paste0("Optimizer convergence code: ", optimizer$convergence, ".")
    }

    ffp_abort(
      c(
        "The Entropy Pooling solver did not converge.",
        "x" = optimizer_message,
        "i" = paste0(
          "The mathematical problem was classified as regular; ",
          "the failure therefore appears to be numerical."
        )
      ),
      class = "ffp_error_solver_nonconvergence",
      call = call
    )
  }

  state <- entropy_dual_state(parameters = optimizer$par, problem = problem)
  posterior <- state$posterior

  if (!entropy_solution_is_valid(problem = problem, posterior = posterior, tolerance = tolerance)) {
    classification <- diagnose_entropy_problem(problem = problem, call = call)
    validate_entropy_solution(problem = problem, posterior = posterior, tolerance = tolerance, call = call)
  }

  if (entropy_solution_needs_boundary_check(problem = problem, posterior = posterior, trigger_tolerance = tolerance)) {
    classification <- diagnose_entropy_problem(problem = problem, call = call)
  }

  residuals <- entropy_solution_residuals(problem = problem, posterior = posterior)
  objective <- entropy_kl_divergence(posterior = posterior, prior = problem$prior)

  if (!is.finite(objective)) {
    classification <- diagnose_entropy_problem(problem = problem, call = call)

    ffp_abort(
      "The solver returned a posterior with non-finite relative entropy.",
      class = "ffp_error_invalid_posterior",
      call = call
    )
  }

  new_ffp_solver_result(
    posterior = posterior,
    objective = objective,
    converged = TRUE,
    status = "converged",
    backend = "stats::optim",
    method = optimized$method,
    dual = list(lambda = state$lambda, nu = state$nu),
    counts = optimizer$counts,
    residuals = residuals,
    control = control
  )
}
