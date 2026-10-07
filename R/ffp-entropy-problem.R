# Entropy problem objects -------------------------------------------------

new_entropy_problem_metadata <- function(constraint_metadata, normalization_row) {
  list(
    constraints = constraint_metadata,
    normalization_row = as.integer(normalization_row)
  )
}


new_ffp_entropy_problem <- function(n_scenarios, prior, a_eq, b_eq, a_ineq, b_ineq, metadata) {
  problem <- structure(
    list(
      prior = prior,
      a_eq = a_eq,
      b_eq = b_eq,
      a_ineq = a_ineq,
      b_ineq = b_ineq,
      metadata = metadata,
      n_scenarios = as.integer(n_scenarios)
    ),
    class = "ffp_entropy_problem"
  )
  validate_ffp_entropy_problem(problem)
  problem
}


validate_ffp_entropy_problem <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "ffp_entropy_problem")) {
    ffp_abort(
      "The object must inherit from {.cls ffp_entropy_problem}.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }
  validate_entropy_scenario_count(n_scenarios = x$n_scenarios, call = call)
  validate_entropy_prior(prior = x$prior, n_scenarios = x$n_scenarios, call = call)
  validate_entropy_matrix(x = x$a_eq, n_scenarios = x$n_scenarios, arg = "a_eq", call = call)
  validate_entropy_rhs(x = x$b_eq, n_constraints = nrow(x$a_eq), arg = "b_eq", call = call)
  validate_entropy_matrix(x = x$a_ineq, n_scenarios = x$n_scenarios, arg = "a_ineq", call = call)
  validate_entropy_rhs(x = x$b_ineq, n_constraints = nrow(x$a_ineq), arg = "b_ineq", call = call)
  validate_entropy_problem_metadata(metadata = x$metadata, n_equalities = nrow(x$a_eq), n_inequalities = nrow(x$a_ineq), call = call)
  validate_entropy_normalization(x = x, call = call)
  invisible(x)
}


validate_entropy_scenario_count <- function(n_scenarios, call = rlang::caller_env()) {
  valid <- is.numeric(n_scenarios) && length(n_scenarios) == 1L && !is.na(n_scenarios) && is.finite(n_scenarios) && n_scenarios >= 1 && n_scenarios == floor(n_scenarios)

  if (!valid) {
    ffp_abort(
      "`n_scenarios` must be a positive whole number.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }
  invisible(n_scenarios)
}


validate_entropy_prior <- function(prior, n_scenarios, call = rlang::caller_env()) {
  valid <- is.numeric(prior) && is.null(dim(prior)) && length(prior) == n_scenarios && !anyNA(prior) && all(is.finite(prior)) && all(prior >= 0)

  if (!valid) {
    ffp_abort(
      c(
        "The entropy problem contains an invalid prior.",
        "x" = paste0("Expected ", n_scenarios, " finite, non-negative probability value(s).")
      ),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  prior_sum <- sum(prior)

  if (!is.finite(prior_sum) || abs(prior_sum - 1) > sqrt(.Machine$double.eps)) {
    ffp_abort(
      "`prior` must sum to one.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  invisible(prior)
}


validate_entropy_matrix <- function(x, n_scenarios, arg, call = rlang::caller_env()) {
  if (!is.matrix(x) || !is.numeric(x)) {
    ffp_abort(
      paste0("`", arg, "` must be a numeric matrix."),
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

  invisible(x)
}


validate_entropy_rhs <- function(x, n_constraints, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    ffp_abort(
      paste0("`", arg, "` must be a numeric vector."),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

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

  invisible(x)
}


validate_entropy_problem_metadata <- function(metadata, n_equalities, n_inequalities, call = rlang::caller_env()) {
  required <- c("constraints", "normalization_row")

  if (!is.list(metadata) || !all(required %in% names(metadata))) {
    ffp_abort(
      "`metadata` does not contain all required entropy-problem fields.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  normalization_row <- metadata$normalization_row

  valid_row <- is.integer(normalization_row) && length(normalization_row) == 1L && !is.na(normalization_row) && normalization_row >= 1L && normalization_row == n_equalities

  if (!valid_row) {
    ffp_abort(
      "`normalization_row` must identify the final equality constraint.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  view_equalities <- n_equalities - 1L

  if (view_equalities < 0L) {
    ffp_abort(
      "The entropy problem must contain a normalization equality.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  tryCatch(
    validate_constraint_metadata(metadata = metadata$constraints, n_equalities = view_equalities, n_inequalities = n_inequalities, call = call),
    ffp_error_invalid_constraints = function(cnd) {
      ffp_abort(
        c(
          "The entropy problem contains invalid constraint metadata.",
          "x" = conditionMessage(cnd)
        ),
        class = "ffp_error_invalid_entropy_problem",
        call = call
      )
    }
  )

  invisible(metadata)
}


validate_entropy_normalization <- function(x, call = rlang::caller_env()) {
  normalization_row <- x$metadata$normalization_row

  if (normalization_row != nrow(x$a_eq)) {
    ffp_abort(
      "Normalization must be the final equality constraint.",
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  normalization_lhs <- x$a_eq[normalization_row, ]
  normalization_rhs <- x$b_eq[[normalization_row]]

  if (length(normalization_lhs) != x$n_scenarios || any(normalization_lhs != 1) || normalization_rhs != 1) {
    ffp_abort(
      c(
        "The entropy problem contains an invalid normalization constraint.",
        "i" = "Normalization must impose `sum(p) = 1`."
      ),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  invisible(x)
}


# Problem builder ---------------------------------------------------------

build_entropy_problem <- function(model, constraints, call = rlang::caller_env()) {
  validate_ffp_model(model = model, call = call)

  if (is.null(model$prior)) {
    ffp_abort(
      c(
        "An FFP model must have a prior before building the entropy problem.",
        "i" = "Specify the prior with {.fn ffp_prior} before fitting the model."
      ),
      class = "ffp_error_missing_prior",
      call = call
    )
  }

  validate_ffp_constraints(constraints, call = call)

  n_scenarios <- scenario_count(model$scenarios)

  if (constraints$n_scenarios != n_scenarios) {
    ffp_abort(
      c(
        "The compiled constraints are incompatible with the model.",
        "x" = paste0("The model contains ", n_scenarios, " scenario(s), but the constraints contain ", constraints$n_scenarios, ".")
      ),
      class = "ffp_error_invalid_entropy_problem",
      call = call
    )
  }

  a_eq <- rbind(constraints$a_eq, rep(1, n_scenarios))

  storage.mode(a_eq) <- "double"
  dimnames(a_eq) <- NULL

  b_eq <- c(constraints$b_eq, 1)
  normalization_row <- nrow(a_eq)
  metadata <- new_entropy_problem_metadata(constraint_metadata = constraints$metadata, normalization_row = normalization_row)

  new_ffp_entropy_problem(
    n_scenarios = n_scenarios,
    prior = model$prior,
    a_eq = a_eq,
    b_eq = b_eq,
    a_ineq = constraints$a_ineq,
    b_ineq = constraints$b_ineq,
    metadata = metadata
  )
}


# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_entropy_problem <- function(x, ...) {
  validate_ffp_entropy_problem(x)

  n_view_equalities <- nrow(x$a_eq) - 1L
  n_prior_zeros <- sum(x$prior == 0)

  cat("<ffp_entropy_problem>\n")
  cat("Scenarios:       ", x$n_scenarios, "\n", sep = "")
  cat("Prior zeros:     ", n_prior_zeros, "\n", sep = "")
  cat("View equalities: ", n_view_equalities, "\n", sep = "")
  cat("Inequalities:    ", nrow(x$a_ineq), "\n", sep = "")
  cat("Normalization:   yes\n")

  invisible(x)
}
