# Fit summary -------------------------------------------------------------

new_ffp_fit_summary <- function(
    n_scenarios,
    n_views,
    status,
    kl_divergence,
    max_residual,
    n_views_satisfied,
    n_inequalities,
    n_binding
) {

  structure(
    list(
      n_scenarios = as.integer(n_scenarios),
      n_views = as.integer(n_views),
      status = status,
      kl_divergence = kl_divergence,
      max_residual = max_residual,
      n_views_satisfied = as.integer(n_views_satisfied),
      n_inequalities = as.integer(n_inequalities),
      n_binding = as.integer(n_binding)
    ),
    class = "summary_ffp_fit"
  )

}


fit_max_residual <- function(fit) {

  residuals <- fit$solver$residuals

  max(
    residuals$normalization_residual,
    residuals$max_equality_residual,
    residuals$max_inequality_violation
  )

}


#' Summarize a fitted FFP model
#'
#' `summary()` provides a compact overview of a fitted Fully Flexible
#' Probabilities model, including the fit status, relative-entropy distortion,
#' numerical residuals, and view diagnostics.
#'
#' @param object An [ffp_fit] object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return An object of class `summary_ffp_fit` containing:
#'
#' - `n_scenarios`: number of scenarios;
#' - `n_views`: number of semantic views;
#' - `status`: solver status;
#' - `kl_divergence`: Kullback-Leibler divergence from prior to posterior;
#' - `max_residual`: largest normalization residual, equality residual, or
#'   inequality violation;
#' - `n_views_satisfied`: number of views satisfied within the solver
#'   tolerance;
#' - `n_inequalities`: number of compiled inequality constraints;
#' - `n_binding`: number of inequality constraints that are numerically
#'   binding.
#'
#' @details
#' The summary is intentionally higher level than [ffp_diagnostics()].
#' It reports whether the fitted views are satisfied and how many inequality
#' constraints are binding without exposing the complete mathematical
#' representation of the Entropy Pooling problem.
#'
#' A view may generate more than one mathematical constraint. For example, a
#' quantile target generates two inequalities. Consequently, `n_views` and
#' `n_inequalities` describe different levels of the fitted model.
#'
#' `n_binding` applies only to inequality constraints. Equality constraints
#' are checked for satisfaction through their residuals but are not described
#' as binding.
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06),
#'   inflation = c(0.02, 0.03, 0.035, 0.045, 0.05)
#' )
#'
#' model <- ffp_model(scenarios) |>
#'   ffp_prior(
#'     prior_uniform()
#'   ) |>
#'   ffp_view(
#'     view_mean(equity, target = 0.02),
#'     view_quantile(inflation, target = 0.045, level = 0.80)
#'   )
#'
#' fit <- ffp_fit(model)
#'
#' summary(fit)
#'
#' @seealso [ffp_fit()], [ffp_probabilities()], [ffp_diagnostics()]
#'
#' @export
summary.ffp_fit <- function(object, ...) {

  validate_ffp_fit(object)

  diagnostics <- ffp_diagnostics(object)

  n_views_satisfied <- sum(diagnostics$status == "satisfied")
  n_inequalities    <- sum(diagnostics$n_inequalities)
  n_binding         <- sum(diagnostics$n_binding)

  new_ffp_fit_summary(
    n_scenarios       = object$n_scenarios,
    n_views           = object$n_views,
    status            = object$solver$status,
    kl_divergence     = object$solver$objective,
    max_residual      = fit_max_residual(object),
    n_views_satisfied = n_views_satisfied,
    n_inequalities    = n_inequalities,
    n_binding         = n_binding
  )

}


#' @export
#' @noRd
print.summary_ffp_fit <- function(x, ...) {

  cat("<ffp_fit summary>\n\n")

  cat("Scenarios:       ", x$n_scenarios, "\n", sep = "")
  cat("Views:           ", x$n_views, "\n", sep = "")
  cat("Status:          ", x$status, "\n", sep = "")

  cat("\n")

  cat("KL divergence:   ", format(x$kl_divergence, digits = 6), "\n", sep = "")
  cat("Max residual:    ", format(x$max_residual, digits = 3, scientific = TRUE), "\n", sep = "")

  cat("\n")

  cat("Views satisfied: ", x$n_views_satisfied, " / ", x$n_views, "\n", sep = "")
  cat("Inequalities:    ", x$n_inequalities, "\n", sep = "")
  cat("Binding:         ", x$n_binding, "\n", sep = "")

  invisible(x)
}
