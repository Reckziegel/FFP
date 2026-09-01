# Entropy problem classification -----------------------------------------

new_entropy_problem_classification <- function(
    category,
    subtype,
    full_feasible,
    support_feasible,
    interior_margin,
    witness = NULL
) {
  structure(
    list(
      category = category,
      subtype = subtype,
      full_feasible = full_feasible,
      support_feasible = support_feasible,
      interior_margin = interior_margin,
      witness = witness
    ),
    class = "ffp_entropy_problem_classification"
  )
}


validate_entropy_boundary_tolerance <- function(tolerance, call = rlang::caller_env()) {

  valid <- is.numeric(tolerance) &&
    length(tolerance) == 1L &&
    !is.na(tolerance) &&
    is.finite(tolerance) &&
    tolerance > 0

  if (!valid) {
    ffp_abort(
      "`boundary_tolerance` must be a finite positive number.",
      class = "ffp_error_invalid_entropy_classification",
      call = call
    )
  }

  invisible(tolerance)
}


# LP helpers --------------------------------------------------------------

entropy_lp_base_constraints <- function(
    problem,
    extra_variables = 0L
) {
  n_extra <- as.integer(extra_variables)

  equality_matrix <- cbind(
    problem$a_eq,
    matrix(
      0,
      nrow = nrow(problem$a_eq),
      ncol = n_extra
    )
  )

  inequality_matrix <- cbind(
    problem$a_ineq,
    matrix(
      0,
      nrow = nrow(problem$a_ineq),
      ncol = n_extra
    )
  )

  matrix <- rbind(
    equality_matrix,
    inequality_matrix
  )

  direction <- c(
    rep("=", nrow(problem$a_eq)),
    rep("<=", nrow(problem$a_ineq))
  )

  rhs <- c(
    problem$b_eq,
    problem$b_ineq
  )

  storage.mode(matrix) <- "double"
  dimnames(matrix) <- NULL

  list(
    matrix = matrix,
    direction = direction,
    rhs = rhs
  )
}


entropy_support_constraints <- function(
    problem,
    n_variables
) {
  zero_prior <- which(
    problem$prior == 0
  )

  if (length(zero_prior) == 0L) {
    return(
      list(
        matrix = matrix(
          numeric(),
          nrow = 0L,
          ncol = n_variables
        ),
        direction = character(),
        rhs = numeric()
      )
    )
  }

  matrix <- matrix(
    0,
    nrow = length(zero_prior),
    ncol = n_variables
  )

  matrix[
    cbind(
      seq_along(zero_prior),
      zero_prior
    )
  ] <- 1

  list(
    matrix = matrix,
    direction = rep("=", length(zero_prior)),
    rhs = rep(0, length(zero_prior))
  )
}


bind_entropy_lp_constraints <- function(...) {
  blocks <- list(...)

  list(
    matrix = do.call(
      rbind,
      lapply(blocks, `[[`, "matrix")
    ),
    direction = unlist(
      lapply(blocks, `[[`, "direction"),
      use.names = FALSE
    ),
    rhs = unlist(
      lapply(blocks, `[[`, "rhs"),
      use.names = FALSE
    )
  )
}


run_entropy_lp <- function(
    direction,
    objective,
    constraints,
    call = rlang::caller_env()
) {
  result <- tryCatch(
    lpSolve::lp(
      direction = direction,
      objective.in = objective,
      const.mat = constraints$matrix,
      const.dir = constraints$direction,
      const.rhs = constraints$rhs
    ),
    error = function(cnd) {
      ffp_abort(
        c(
          "The entropy-problem classification LP failed.",
          "x" = conditionMessage(cnd)
        ),
        class = "ffp_error_entropy_classification_failure",
        call = call
      )
    }
  )

  if (result$status == 0L) {
    return(
      list(
        feasible = TRUE,
        solution = result$solution,
        objective = result$objval
      )
    )
  }

  if (result$status == 2L) {
    return(
      list(
        feasible = FALSE,
        solution = NULL,
        objective = NA_real_
      )
    )
  }

  ffp_abort(
    c(
      "The entropy-problem classification LP did not return a conclusive result.",
      "x" = paste0(
        "lpSolve returned status ",
        result$status,
        "."
      )
    ),
    class = "ffp_error_entropy_classification_failure",
    call = call
  )
}


# Feasibility -------------------------------------------------------------

entropy_full_feasibility <- function(
    problem,
    call = rlang::caller_env()
) {
  constraints <- entropy_lp_base_constraints(
    problem = problem
  )

  run_entropy_lp(
    direction = "min",
    objective = rep(0, problem$n_scenarios),
    constraints = constraints,
    call = call
  )
}


entropy_support_feasibility <- function(
    problem,
    call = rlang::caller_env()
) {
  base <- entropy_lp_base_constraints(
    problem = problem
  )

  support <- entropy_support_constraints(
    problem = problem,
    n_variables = problem$n_scenarios
  )

  constraints <- bind_entropy_lp_constraints(
    base,
    support
  )

  run_entropy_lp(
    direction = "min",
    objective = rep(0, problem$n_scenarios),
    constraints = constraints,
    call = call
  )
}


# Interior margin ---------------------------------------------------------

entropy_interior_margin <- function(
    problem,
    call = rlang::caller_env()
) {
  n_scenarios <- problem$n_scenarios
  n_variables <- n_scenarios + 1L
  margin_column <- n_variables

  base <- entropy_lp_base_constraints(
    problem = problem,
    extra_variables = 1L
  )

  support <- entropy_support_constraints(
    problem = problem,
    n_variables = n_variables
  )

  positive_prior <- which(
    problem$prior > 0
  )

  margin_matrix <- matrix(
    0,
    nrow = length(positive_prior),
    ncol = n_variables
  )

  margin_matrix[
    cbind(
      seq_along(positive_prior),
      positive_prior
    )
  ] <- 1

  margin_matrix[, margin_column] <- -1

  margin_constraints <- list(
    matrix = margin_matrix,
    direction = rep(">=", length(positive_prior)),
    rhs = rep(0, length(positive_prior))
  )

  constraints <- bind_entropy_lp_constraints(
    base,
    support,
    margin_constraints
  )

  objective <- c(
    rep(0, n_scenarios),
    1
  )

  result <- run_entropy_lp(
    direction = "max",
    objective = objective,
    constraints = constraints,
    call = call
  )

  if (!result$feasible) {
    return(
      list(
        feasible = FALSE,
        margin = NA_real_,
        witness = NULL
      )
    )
  }

  list(
    feasible = TRUE,
    margin = result$solution[[margin_column]],
    witness = result$solution[seq_len(n_scenarios)]
  )
}


# Classifier --------------------------------------------------------------

classify_entropy_problem <- function(
    problem,
    boundary_tolerance = 1e-10,
    call = rlang::caller_env()
) {
  validate_ffp_entropy_problem(
    problem,
    call = call
  )

  validate_entropy_boundary_tolerance(
    tolerance = boundary_tolerance,
    call = call
  )

  full <- entropy_full_feasibility(
    problem = problem,
    call = call
  )

  if (!full$feasible) {
    return(
      new_entropy_problem_classification(
        category = "infeasible",
        subtype = "constraints",
        full_feasible = FALSE,
        support_feasible = FALSE,
        interior_margin = NA_real_,
        witness = NULL
      )
    )
  }

  support <- entropy_support_feasibility(
    problem = problem,
    call = call
  )

  if (!support$feasible) {
    return(
      new_entropy_problem_classification(
        category = "boundary",
        subtype = "prior_support",
        full_feasible = TRUE,
        support_feasible = FALSE,
        interior_margin = NA_real_,
        witness = full$solution
      )
    )
  }

  interior <- entropy_interior_margin(
    problem = problem,
    call = call
  )

  if (!interior$feasible) {
    ffp_abort(
      paste0(
        "Entropy-problem classification produced inconsistent ",
        "feasibility results."
      ),
      class = "ffp_error_entropy_classification_failure",
      call = call
    )
  }

  if (interior$margin <= boundary_tolerance) {
    return(
      new_entropy_problem_classification(
        category = "boundary",
        subtype = "simplex_face",
        full_feasible = TRUE,
        support_feasible = TRUE,
        interior_margin = interior$margin,
        witness = interior$witness
      )
    )
  }

  new_entropy_problem_classification(
    category = "regular",
    subtype = "interior",
    full_feasible = TRUE,
    support_feasible = TRUE,
    interior_margin = interior$margin,
    witness = interior$witness
  )
}


# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_entropy_problem_classification <- function(x, ...) {
  cat("<ffp_entropy_problem_classification>\n")
  cat("Category:        ", x$category, "\n", sep = "")
  cat("Subtype:         ", x$subtype, "\n", sep = "")

  if (is.finite(x$interior_margin)) {
    cat(
      "Interior margin: ",
      format(
        x$interior_margin,
        digits = 6,
        scientific = TRUE
      ),
      "\n",
      sep = ""
    )
  }

  invisible(x)
}
