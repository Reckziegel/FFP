# Solver diagnostics ------------------------------------------------------

entropy_solution_needs_boundary_check <- function(problem, posterior, trigger_tolerance) {

  positive_prior <- problem$prior > 0

  if (!any(positive_prior)) {
    return(FALSE)
  }

  any(posterior[positive_prior] <= trigger_tolerance)

}


abort_infeasible_entropy_problem <- function(classification, call = rlang::caller_env()) {

  ffp_abort(
    c(
      "The Entropy Pooling problem is infeasible.",
      "x" = paste0(
        "No probability distribution satisfies all compiled ",
        "constraints simultaneously."
      ),
      "i" = paste0(
        "Review the model views for contradictory or impossible ",
        "requirements."
      )
    ),
    class = c(
      "ffp_error_infeasible_problem",
      "ffp_error_solver_failure"
    ),
    call = call
  )
}


abort_simplex_boundary_problem <- function(classification, call = rlang::caller_env()) {

  ffp_abort(
    c(
      "The Entropy Pooling solution lies on a boundary of the simplex.",
      "x" = paste0(
        "At least one scenario with positive prior probability must ",
        "receive zero posterior probability."
      ),
      "i" = paste0(
        "The current dual solver represents positive-prior scenarios ",
        "with strictly positive posterior probabilities and cannot ",
        "attain this boundary solution exactly."
      )
    ),
    class = c(
      "ffp_error_boundary_simplex_face",
      "ffp_error_boundary_problem",
      "ffp_error_solver_failure"
    ),
    call = call
  )

}


abort_prior_support_boundary_problem <- function(classification, call = rlang::caller_env()) {

  ffp_abort(
    c(
      "The views require probability outside the current prior support.",
      "x" = paste0(
        "At least one scenario with zero prior probability must receive ",
        "positive posterior probability for the constraints to be feasible."
      ),
      "i" = paste0(
        "The current KL dual solver cannot create positive posterior ",
        "mass where the prior probability is exactly zero."
      )
    ),
    class = c(
      "ffp_error_boundary_prior_support",
      "ffp_error_boundary_problem",
      "ffp_error_solver_failure"
    ),
    call = call
  )

}


diagnose_entropy_problem <- function(problem, boundary_tolerance = 1e-10, call = rlang::caller_env()) {

  classification <- classify_entropy_problem(
    problem = problem,
    boundary_tolerance = boundary_tolerance,
    call = call
  )

  if (identical(classification$category, "infeasible")) {
    abort_infeasible_entropy_problem(classification = classification, call = call)
  }

  if (identical(classification$category, "boundary") && identical(classification$subtype, "simplex_face")) {
    abort_simplex_boundary_problem(classification = classification, call = call)
  }

  if (identical(classification$category, "boundary") && identical(classification$subtype, "prior_support")) {
    abort_prior_support_boundary_problem(classification = classification, call = call)
  }

  classification

}
