# FFP probabilities -------------------------------------------------------

#' Extract FFP probabilities
#'
#' `ffp_probabilities()` returns scenario probabilities from fitted Fully
#' Flexible Probabilities objects and opinion-pooled distributions.
#'
#' For an [ffp_fit] object, the function compares the prior with the
#' full-confidence posterior obtained by Entropy Pooling.
#'
#' For an `ffp_opinion_pool` object, the function preserves the same columns
#' and appends the confidence-weighted posterior as the final column.
#'
#' @param fit An [ffp_fit] object or an `ffp_opinion_pool` object.
#'
#' @return
#' For an [ffp_fit] object, a tibble with one row per scenario and exactly
#' five columns:
#'
#' - `scenario`: one-based scenario position;
#' - `index`: the original scenario index, when available, or `NA` otherwise;
#' - `prior`: prior scenario probability;
#' - `full_confidence`: full-confidence posterior from Entropy Pooling;
#' - `difference`: full-confidence posterior minus prior probability.
#'
#' For an `ffp_opinion_pool` object, the same five columns are returned,
#' followed by:
#'
#' - `posterior`: confidence-weighted posterior probability.
#'
#' @details
#' Fully Flexible Probabilities keep the original joint scenarios fixed and
#' modify only the probabilities assigned to them.
#'
#' For an [ffp_fit] object, `full_confidence` is the posterior obtained by
#' solving the Entropy Pooling problem under full confidence in the views.
#'
#' For an `ffp_opinion_pool` object, `posterior` is obtained by global opinion
#' pooling:
#'
#' \deqn{
#' p_c = (1-c)q + c p_{\mathrm{full}},
#' }
#'
#' where \eqn{q} is the prior, \eqn{p_{\mathrm{full}}} is the full-confidence
#' posterior, and \eqn{c} is the global confidence level.
#'
#' The first five columns are identical for an [ffp_fit] object and an
#' `ffp_opinion_pool` derived from that fit. Opinion pooling therefore extends
#' the probability table by appending the effective posterior without changing
#' the existing probability path.
#'
#' `difference` always means
#'
#' \deqn{
#' p_{\mathrm{full}, t} - q_t.
#' }
#'
#' Its meaning does not change after opinion pooling.
#'
#' When an index was identified by [ffp_model()], its original values and class
#' are preserved in the `index` column. When no index is available, `index`
#' contains `NA_real_` for every scenario.
#'
#' The function deliberately does not report posterior-to-prior ratios.
#' Exact zero prior probabilities are valid in FFP models, so such ratios are
#' not generally defined.
#'
#' @examples
#' scenarios <- data.frame(
#'   date = as.Date("2026-01-01") + 0:4,
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
#' ffp_probabilities(fit)
#'
#' pooled <- ffp_opinion_pooling(
#'   fit,
#'   confidence = 0.75
#' )
#'
#' ffp_probabilities(pooled)
#'
#' @seealso
#' [ffp_fit()], [ffp_model()], [ffp_opinion_pooling()]
#'
#' @export
ffp_probabilities <- function(fit) {
  UseMethod("ffp_probabilities")
}


#' @export
#' @noRd
ffp_probabilities.ffp_fit <- function(fit) {
  validate_ffp_fit(fit)

  index <- ffp_probability_index(fit)

  tibble::tibble(
    scenario = seq_len(fit$n_scenarios),
    index = index,
    prior = fit$prior,
    full_confidence = fit$posterior,
    difference = fit$posterior - fit$prior
  )
}


#' @export
#' @noRd
ffp_probabilities.ffp_opinion_pool <- function(fit) {
  validate_ffp_opinion_pool(fit)

  index <- ffp_probability_index(fit$fit)

  tibble::tibble(
    scenario = seq_len(fit$n_scenarios),
    index = index,
    prior = fit$prior,
    full_confidence = fit$full_confidence_posterior,
    difference = fit$full_confidence_posterior - fit$prior,
    posterior = fit$posterior
  )
}


#' @export
#' @noRd
ffp_probabilities.default <- function(fit) {
  validate_ffp_fit(fit)
}


# Internal helpers --------------------------------------------------------

ffp_probability_index <- function(fit) {
  index <- fit$model$metadata$index

  if (is.null(index)) {
    return(
      rep(NA_real_, fit$n_scenarios)
    )
  }

  index
}
