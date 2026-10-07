# Opinion-pool summary ----------------------------------------------------

#' Summarize an FFP opinion pool
#'
#' `summary.ffp_opinion_pool()` summarizes the effect of global opinion
#' pooling on scenario probabilities.
#'
#' The summary compares the full-confidence Entropy Pooling posterior with
#' the final confidence-weighted posterior using two complementary measures:
#'
#' - Kullback-Leibler divergence from the prior;
#' - total probability mass reallocated relative to the prior.
#'
#' @param object An `ffp_opinion_pool` object.
#' @param ... Additional arguments, currently unused.
#'
#' @return An object of class `summary_ffp_opinion_pool`.
#'
#' @details
#' The probability mass reallocated by a distribution \eqn{p} relative to the
#' prior \eqn{q} is defined as
#'
#' \deqn{
#' \frac{1}{2} \sum_t |p_t - q_t|.
#' }
#'
#' This is the total variation distance between the two discrete probability
#' distributions.
#'
#' Under global opinion pooling,
#'
#' \deqn{
#' p_c = (1-c)q + c p_{\mathrm{full}},
#' }
#'
#' so the reallocated probability mass satisfies
#'
#' \deqn{
#' TV(p_c, q) = c TV(p_{\mathrm{full}}, q).
#' }
#'
#' The summary deliberately does not report solver convergence, residuals,
#' constraint satisfaction, or binding inequalities. Those quantities belong
#' to the underlying full-confidence [ffp_fit] object and can be inspected with
#' [summary()] and [ffp_diagnostics()].
#'
#' @examples
#' scenarios <- data.frame(
#'   equity = c(-0.08, -0.03, 0.01, 0.04, 0.06)
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
#' summary(pooled)
#'
#' @seealso
#' [ffp_opinion_pooling()], [ffp_probabilities()], [ffp_diagnostics()]
#'
#' @export
summary.ffp_opinion_pool <- function(object, ...) {

  validate_ffp_opinion_pool(object)

  structure(
    list(
      n_scenarios = object$n_scenarios,
      n_views     = object$fit$n_views,
      confidence  = object$confidence,
      kl_full_confidence = entropy_kl_divergence(object$full_confidence_posterior, object$prior),
      kl_posterior       = entropy_kl_divergence(object$posterior, object$prior),
      mass_reallocated_full_confidence = opinion_pool_mass_reallocated(object$full_confidence_posterior, object$prior),
      mass_reallocated_posterior       = opinion_pool_mass_reallocated(object$posterior, object$prior)
    ),
    class = "summary_ffp_opinion_pool"
  )
}


#' @export
#' @noRd
print.summary_ffp_opinion_pool <- function(x, ...) {
  cat("<ffp_opinion_pool summary>\n\n")

  cat(
    sprintf("%-16s%s\n", "Scenarios:", format(x$n_scenarios, trim = TRUE))
  )

  cat(
    sprintf("%-16s%s\n", "Views:", format(x$n_views, trim = TRUE))
  )

  cat(
    sprintf("%-16s%s\n", "Confidence:", format_opinion_pool_confidence(x$confidence))
  )

  cat("\nKL divergence:\n")

  cat(
    sprintf("  %-18s%s\n", "Full confidence:", format_opinion_pool_number(x$kl_full_confidence))
  )

  cat(
    sprintf("  %-18s%s\n", "Posterior:", format_opinion_pool_number(x$kl_posterior))
  )

  cat("\nProbability mass reallocated:\n")

  cat(
    sprintf("  %-18s%s\n", "Full confidence:", format_opinion_pool_percentage(x$mass_reallocated_full_confidence))
  )

  cat(
    sprintf("  %-18s%s\n", "Posterior:", format_opinion_pool_percentage(x$mass_reallocated_posterior))
  )

  invisible(x)
}


# Internal helpers --------------------------------------------------------

opinion_pool_mass_reallocated <- function(probabilities, prior) {
  0.5 * sum(abs(probabilities - prior))
}


format_opinion_pool_number <- function(x) {
  if (is.infinite(x)) {
    return("Inf")
  }
  format(x, digits = 4, trim = TRUE, scientific = FALSE)
}


format_opinion_pool_percentage <- function(x) {
  paste0(format(100 * x, digits = 4, trim = TRUE, scientific = FALSE), "%")
}
