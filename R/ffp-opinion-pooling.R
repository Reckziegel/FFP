# Opinion-pool objects ----------------------------------------------------

new_ffp_opinion_pool <- function(
    prior,
    full_confidence_posterior,
    posterior,
    confidence,
    fit,
    n_scenarios
) {
  pool <- structure(
    list(
      prior = prior,
      full_confidence_posterior = full_confidence_posterior,
      posterior = posterior,
      confidence = confidence,
      fit = fit,
      n_scenarios = as.integer(n_scenarios)
    ),
    class = "ffp_opinion_pool"
  )

  validate_ffp_opinion_pool(pool)

  pool
}


validate_ffp_opinion_pool <- function(
    x,
    call = rlang::caller_env()
) {
  if (!inherits(x, "ffp_opinion_pool")) {
    ffp_abort(
      "The object must inherit from {.cls ffp_opinion_pool}.",
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  validate_ffp_fit(
    x$fit,
    call = call
  )

  validate_opinion_pool_scenario_count(
    n_scenarios = x$n_scenarios,
    call = call
  )

  if (x$n_scenarios != x$fit$n_scenarios) {
    ffp_abort(
      "`n_scenarios` must match the underlying {.cls ffp_fit}.",
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  validate_opinion_pool_confidence(
    confidence = x$confidence,
    call = call
  )

  validate_opinion_pool_probability_vector(
    x = x$prior,
    n_scenarios = x$n_scenarios,
    arg = "prior",
    call = call
  )

  validate_opinion_pool_probability_vector(
    x = x$full_confidence_posterior,
    n_scenarios = x$n_scenarios,
    arg = "full_confidence_posterior",
    call = call
  )

  validate_opinion_pool_probability_vector(
    x = x$posterior,
    n_scenarios = x$n_scenarios,
    arg = "posterior",
    call = call
  )

  if (!identical(x$prior, x$fit$prior)) {
    ffp_abort(
      "`prior` must match the prior stored in the underlying {.cls ffp_fit}.",
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  if (!identical(x$full_confidence_posterior, x$fit$posterior)) {
    ffp_abort(
      paste0(
        "`full_confidence_posterior` must match the posterior stored in ",
        "the underlying {.cls ffp_fit}."
      ),
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  expected_posterior <- opinion_pool_probabilities(
    prior = x$prior,
    full_confidence_posterior = x$full_confidence_posterior,
    confidence = x$confidence
  )

  if (!identical(x$posterior, expected_posterior)) {
    ffp_abort(
      paste0(
        "`posterior` is inconsistent with the stored prior, full-confidence ",
        "posterior, and confidence."
      ),
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  invisible(x)
}


validate_opinion_pool_confidence <- function(
    confidence,
    call = rlang::caller_env()
) {
  valid <- is.numeric(confidence) &&
    is.null(dim(confidence)) &&
    length(confidence) == 1L &&
    !is.na(confidence) &&
    is.finite(confidence)

  if (!valid) {
    ffp_abort(
      c(
        "{.arg confidence} must be a single finite number between 0 and 1.",
        "i" = "Use decimal confidence, such as 0.75 for 75 percent."
      ),
      class = c(
        "ffp_error_invalid_confidence",
        "ffp_error_invalid_opinion_pool"
      ),
      call = call
    )
  }

  if (confidence < 0 || confidence > 1) {
    ffp_abort(
      c(
        "{.arg confidence} must be between 0 and 1.",
        "i" = "Use 0 for no confidence and 1 for full confidence."
      ),
      class = c(
        "ffp_error_invalid_confidence",
        "ffp_error_invalid_opinion_pool"
      ),
      call = call
    )
  }

  invisible(confidence)
}


validate_opinion_pool_scenario_count <- function(
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
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  invisible(n_scenarios)
}


validate_opinion_pool_probability_vector <- function(
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
      class = "ffp_error_invalid_opinion_pool",
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
      class = "ffp_error_invalid_opinion_pool",
      call = call
    )
  }

  invisible(x)
}


opinion_pool_probabilities <- function(
    prior,
    full_confidence_posterior,
    confidence
) {
  if (
    confidence == 0 ||
    identical(prior, full_confidence_posterior)
  ) {
    return(prior)
  }

  if (confidence == 1) {
    return(full_confidence_posterior)
  }

  (1 - confidence) * prior +
    confidence * full_confidence_posterior
}


# Opinion pooling ---------------------------------------------------------

#' Apply global confidence through opinion pooling
#'
#' `ffp_opinion_pooling()` combines the prior distribution of a fitted
#' Fully Flexible Probabilities model with its full-confidence posterior.
#'
#' The function implements global partial confidence through linear opinion
#' pooling:
#'
#' \deqn{
#' p_c = (1-c)q + c p_{\mathrm{full}},
#' }
#'
#' where \eqn{q} is the prior, \eqn{p_{\mathrm{full}}} is the
#' full-confidence posterior returned by [ffp_fit()], and \eqn{c} is the
#' confidence level.
#'
#' @param fit An [ffp_fit] object containing the full-confidence Entropy
#'   Pooling solution.
#' @param confidence A single finite number between 0 and 1 giving the global
#'   confidence assigned jointly to all views in `fit`.
#'
#' @return An object of class `ffp_opinion_pool` containing:
#'
#' - `prior`: the prior scenario probabilities;
#' - `full_confidence_posterior`: the full-confidence posterior from `fit`;
#' - `posterior`: the confidence-weighted posterior probabilities;
#' - `confidence`: the global confidence level;
#' - `fit`: the original [ffp_fit] object;
#' - `n_scenarios`: the number of joint scenarios.
#'
#' @details
#' `ffp_fit()` always solves the full-confidence Entropy Pooling problem.
#' `ffp_opinion_pooling()` is a separate subsequent operation and does not
#' recompile views, rerun the solver, relax constraints, or alter numerical
#' solver tolerances.
#'
#' `confidence = 0` returns the prior exactly, while `confidence = 1` returns
#' the full-confidence posterior exactly.
#'
#' For confidence levels strictly between 0 and 1, each scenario probability
#' moves the same fraction of the way from its prior value toward its
#' full-confidence value:
#'
#' \deqn{
#' p_c - q = c(p_{\mathrm{full}} - q).
#' }
#'
#' The confidence-weighted posterior does not, in general, satisfy the
#' original views exactly. This is an intentional consequence of partial
#' confidence and must not be interpreted as numerical failure of the
#' full-confidence Entropy Pooling fit.
#'
#' @references
#' Meucci, A. (2008). Fully Flexible Views: Theory and Practice.
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
#'     view_mean(
#'       equity,
#'       target = 0.02
#'     )
#'   )
#'
#' fit <- ffp_fit(model)
#'
#' pooled <- ffp_opinion_pooling(
#'   fit,
#'   confidence = 0.75
#' )
#'
#' pooled
#' pooled$prior
#' pooled$full_confidence_posterior
#' pooled$posterior
#'
#' @seealso [ffp_fit()]
#'
#' @export
ffp_opinion_pooling <- function(
    fit,
    confidence
) {
  call <- rlang::caller_env()

  validate_ffp_fit(
    fit,
    call = call
  )

  if (missing(confidence)) {
    ffp_abort(
      c(
        "{.fn ffp_opinion_pooling} requires {.arg confidence}.",
        "i" = "Supply one global confidence level between 0 and 1."
      ),
      class = c(
        "ffp_error_invalid_confidence",
        "ffp_error_invalid_opinion_pool"
      ),
      call = call
    )
  }

  validate_opinion_pool_confidence(
    confidence = confidence,
    call = call
  )

  confidence <- unname(
    as.double(confidence)
  )

  posterior <- opinion_pool_probabilities(
    prior = fit$prior,
    full_confidence_posterior = fit$posterior,
    confidence = confidence
  )

  new_ffp_opinion_pool(
    prior = fit$prior,
    full_confidence_posterior = fit$posterior,
    posterior = posterior,
    confidence = confidence,
    fit = fit,
    n_scenarios = fit$n_scenarios
  )
}


# Printing ----------------------------------------------------------------

format_opinion_pool_confidence <- function(confidence) {
  paste0(
    format(
      100 * confidence,
      digits = 6,
      trim = TRUE,
      scientific = FALSE
    ),
    "%"
  )
}


#' @export
#' @noRd
print.ffp_opinion_pool <- function(x, ...) {
  validate_ffp_opinion_pool(x)

  cat("<ffp_opinion_pool>\n")
  cat("Scenarios:   ", x$n_scenarios, "\n", sep = "")
  cat(
    "Confidence:  ",
    format_opinion_pool_confidence(x$confidence),
    "\n",
    sep = ""
  )

  invisible(x)
}
