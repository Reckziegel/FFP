# Fit objects -------------------------------------------------------------

new_ffp_fit <- function(
    posterior,
    prior,
    constraints,
    solver,
    model,
    n_scenarios,
    n_views
) {
  fit <- structure(
    list(
      posterior = posterior,
      prior = prior,
      constraints = constraints,
      solver = solver,
      model = model,
      n_scenarios = as.integer(n_scenarios),
      n_views = as.integer(n_views)
    ),
    class = "ffp_fit"
  )

  validate_ffp_fit(fit)

  fit
}


validate_ffp_fit <- function(
    x,
    call = rlang::caller_env()
) {
  if (!inherits(x, "ffp_fit")) {
    ffp_abort(
      "The object must inherit from {.cls ffp_fit}.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  if (!inherits(x$model, "ffp_model")) {
    ffp_abort(
      "`model` must inherit from {.cls ffp_model}.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  validate_ffp_model(
    model = x$model,
    call = call
  )

  validate_fit_scenario_count(
    n_scenarios = x$n_scenarios,
    call = call
  )

  validate_fit_view_count(
    n_views = x$n_views,
    call = call
  )

  if (scenario_count(x$model$scenarios) != x$n_scenarios) {
    ffp_abort(
      "`model` is incompatible with the fitted scenario support.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  if (length(x$model$views) != x$n_views) {
    ffp_abort(
      "`model` is incompatible with the fitted view count.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  validate_fit_probability_vector(
    x = x$prior,
    n_scenarios = x$n_scenarios,
    arg = "prior",
    call = call
  )

  validate_fit_probability_vector(
    x = x$posterior,
    n_scenarios = x$n_scenarios,
    arg = "posterior",
    call = call
  )

  if (!identical(x$prior, x$model$prior)) {
    ffp_abort(
      "`prior` must match the prior stored in `model`.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  if (!inherits(x$constraints, "ffp_constraints")) {
    ffp_abort(
      "`constraints` must inherit from {.cls ffp_constraints}.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  validate_ffp_constraints(
    x$constraints,
    call = call
  )

  if (x$constraints$n_scenarios != x$n_scenarios) {
    ffp_abort(
      "`constraints` are incompatible with the fitted scenario support.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  if (!inherits(x$solver, "ffp_solver_result")) {
    ffp_abort(
      "`solver` must inherit from {.cls ffp_solver_result}.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  validate_ffp_solver_result(
    x$solver,
    call = call
  )

  if (!identical(x$posterior, x$solver$posterior)) {
    ffp_abort(
      "`posterior` must match the posterior returned by the solver.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  invisible(x)
}


validate_fit_scenario_count <- function(
    n_scenarios,
    call = rlang::caller_env()
) {
  valid <- is.numeric(n_scenarios) &&
    length(n_scenarios) == 1L &&
    !is.na(n_scenarios) &&
    is.finite(n_scenarios) &&
    n_scenarios >= 1 &&
    n_scenarios == floor(n_scenarios)

  if (!valid) {
    ffp_abort(
      "`n_scenarios` must be a positive whole number.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  invisible(n_scenarios)
}


validate_fit_view_count <- function(
    n_views,
    call = rlang::caller_env()
) {
  valid <- is.numeric(n_views) &&
    length(n_views) == 1L &&
    !is.na(n_views) &&
    is.finite(n_views) &&
    n_views >= 0 &&
    n_views == floor(n_views)

  if (!valid) {
    ffp_abort(
      "`n_views` must be a non-negative whole number.",
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  invisible(n_views)
}


validate_fit_probability_vector <- function(
    x,
    n_scenarios,
    arg,
    call = rlang::caller_env()
) {
  valid <- is.numeric(x) &&
    is.null(dim(x)) &&
    length(x) == n_scenarios &&
    !anyNA(x) &&
    all(is.finite(x)) &&
    all(x >= 0)

  if (!valid) {
    ffp_abort(
      c(
        paste0("`", arg, "` is not a valid probability vector."),
        "x" = paste0(
          "Expected ",
          n_scenarios,
          " finite, non-negative probability value(s)."
        )
      ),
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  probability_sum <- sum(x)

  if (
    !is.finite(probability_sum) ||
    abs(probability_sum - 1) > 1e-8
  ) {
    ffp_abort(
      paste0("`", arg, "` must sum to one."),
      class = "ffp_error_invalid_fit",
      call = call
    )
  }

  invisible(x)
}


# Fitting -----------------------------------------------------------------

#' Fit a Fully Flexible Probabilities model
#'
#' `ffp_fit()` computes posterior scenario probabilities that incorporate the
#' prior distribution and all views attached to an [ffp_model] using
#' full-confidence Entropy Pooling.
#'
#' @param model An [ffp_model] object containing scenario data, a prior
#'   probability distribution, and optionally one or more views.
#' @param control Numerical solver controls created by
#'   [entropy_solver_control()].
#'
#' @return An object of class `ffp_fit` containing:
#'
#' - `posterior`: the fitted posterior scenario probabilities;
#' - `prior`: the prior scenario probabilities;
#' - `constraints`: the mathematical constraints compiled from the views;
#' - `solver`: numerical solver results, diagnostics, and the numerical
#'   controls used for fitting;
#' - `model`: the fitted [ffp_model], including scenarios, prior
#'   specification, views, and scenario metadata;
#' - `n_scenarios`: the number of joint scenarios;
#' - `n_views`: the number of views included in the model.
#'
#' @details
#' Fully Flexible Probabilities represent a distribution through a fixed set
#' of joint scenarios and a probability associated with each scenario.
#'
#' `ffp_fit()` keeps the scenarios unchanged and modifies their probabilities
#' to incorporate the user's views.
#'
#' For prior probabilities \eqn{q_t} and posterior probabilities \eqn{p_t},
#' the full-confidence Entropy Pooling problem minimizes the relative entropy
#'
#' \deqn{
#' \sum_t p_t \log\left(\frac{p_t}{q_t}\right)
#' }
#'
#' subject to the equality and inequality constraints implied by the views,
#' together with the probability normalization condition
#'
#' \deqn{
#' \sum_t p_t = 1.
#' }
#'
#' The resulting posterior is therefore the distribution satisfying the views
#' while introducing the minimum relative-entropy distortion from the prior.
#'
#' The normal workflow does not require the user to compile views or construct
#' the Entropy Pooling problem explicitly. `ffp_fit()` performs internally:
#'
#' 1. compilation of all bound views;
#' 2. construction of the Entropy Pooling problem;
#' 3. numerical solution of the full-confidence problem;
#' 4. validation of the posterior probabilities.
#'
#' If the prior already satisfies all views, the prior itself is the
#' minimum-distortion solution and is returned exactly as the posterior.
#'
#' If the model contains no views, the posterior is also identical to the
#' prior.
#'
#' @section Full confidence:
#'
#' The current implementation solves the **full-confidence** Entropy Pooling
#' problem. Every attached view is therefore imposed as a mathematical
#' constraint on the posterior distribution.
#'
#' Partial-confidence opinion pooling is conceptually distinct from numerical
#' solver tolerances and is not performed by `ffp_fit()` at this stage.
#'
#' @section Numerical controls:
#'
#' The default solver settings are appropriate for typical applications.
#' Advanced users can customize numerical tolerances and iteration limits with
#' [entropy_solver_control()].
#'
#' Solver tolerances govern numerical convergence and posterior verification;
#' they do not alter view targets or otherwise relax the specified views.
#'
#' @examples
#' # A simple set of joint scenarios
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   inflation = c(0.02, 0.03, 0.035, 0.045, 0.05)
#' )
#'
#' # Without views, the posterior equals the prior
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_uniform()
#'   )
#'
#' fit <- ffp_fit(model)
#' fit
#'
#' fit$posterior
#'
#' # Add a view on the posterior expected equity return
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_uniform()
#'   ) |>
#'   ffp_view(
#'     view_mean(
#'       equity,
#'       target = 0.02
#'     )
#'   )
#'
#' fit <- ffp_fit(model)
#' fit
#'
#' sum(fit$posterior * scenarios$equity)
#'
#' # Several views can be processed jointly
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   inflation = c(0.02, 0.03, 0.035, 0.045, 0.05),
#'   recession = c(TRUE, TRUE, FALSE, FALSE, FALSE)
#' )
#'
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_uniform()
#'   ) |>
#'   ffp_view(
#'     view_mean(
#'       equity,
#'       target = 0.02
#'     ),
#'     view_probability(
#'       recession,
#'       target = 0.30
#'     )
#'   )
#'
#' fit <- ffp_fit(model)
#' fit
#'
#' sum(fit$posterior * scenarios$equity)
#' sum(fit$posterior[scenarios$recession])
#'
#' # Advanced users can customize numerical controls
#' control <- entropy_solver_control(
#'   constraint_tolerance = 1e-9
#' )
#'
#' fit <- ffp_fit(
#'   model,
#'   control = control
#' )
#'
#' @seealso
#' [ffp_model()], [ffp_prior()], [ffp_view()],
#' [entropy_solver_control()]
#'
#' @export
ffp_fit <- function(
    model,
    control = entropy_solver_control()
) {
  call <- rlang::caller_env()

  validate_ffp_model(
    model = model,
    call = call
  )

  if (is.null(model$prior)) {
    ffp_abort(
      c(
        "An FFP model must have a prior before it can be fitted.",
        "i" = paste0(
          "Specify the prior with {.fn ffp_prior} before calling ",
          "{.fn ffp_fit}."
        )
      ),
      class = "ffp_error_missing_prior",
      call = call
    )
  }

  validate_entropy_solver_control(
    control = control,
    call = call
  )

  constraints <- compile_views(
    model = model,
    call = call
  )

  problem <- build_entropy_problem(
    model = model,
    constraints = constraints,
    call = call
  )

  solver <- solve_entropy_problem(
    problem = problem,
    control = control,
    call = call
  )

  new_ffp_fit(
    posterior = solver$posterior,
    prior = model$prior,
    constraints = constraints,
    solver = solver,
    model = model,
    n_scenarios = problem$n_scenarios,
    n_views = length(model$views)
  )
}

# Printing ----------------------------------------------------------------

#' @export
#' @noRd
print.ffp_fit <- function(x, ...) {

  validate_ffp_fit(x)

  max_residual <- fit_max_residual(x)

  cat("<ffp_fit>\n")
  cat("Scenarios:      ", x$n_scenarios, "\n", sep = "")
  cat("Views:          ", x$n_views, "\n", sep = "")
  cat("Status:         ", x$solver$status, "\n", sep = "")

  cat(
    "KL divergence:  ",
    format(x$solver$objective, digits = 6),
    "\n",
    sep = ""
  )

  cat(
    "Max residual:   ",
    format(max_residual, digits = 3, scientific = TRUE),
    "\n",
    sep = ""
  )

  invisible(x)
}
