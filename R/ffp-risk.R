#' Tail Risk Under Flexible Probabilities
#'
#' Computes loss-oriented tail-risk measures under flexible probabilities.
#'
#' @param x A set of scenarios or an object supported by an `ffp_risk()`
#'   method.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `ffp_risk`.
#'
#' @export
ffp_risk <- function(x, ...) {
  UseMethod("ffp_risk")
}

#' @rdname ffp_risk
#'
#' @param p An optional probability vector. It can be an `ffp` object or a
#'   numeric vector that can be converted to `ffp`. If `NULL`, equal
#'   probabilities are used.
#' @param confidence A single number strictly between 0 and 1 defining the
#'   confidence level of the tail-risk measures.
#'
#' @details
#' `ffp_risk()` assumes that lower scenario values represent worse outcomes,
#' as is customary for returns or profit-and-loss observations.
#'
#' For confidence level `confidence`, the corresponding lower-tail probability
#' is `1 - confidence`.
#'
#' `value_at_risk` and `expected_shortfall` are reported using a loss-oriented
#' sign convention. Therefore, negative lower-tail returns are reported as
#' positive risk values.
#'
#' Risk measures are not truncated at zero. Consequently, a negative
#' `value_at_risk` or `expected_shortfall` indicates that even the relevant
#' lower tail consists of positive outcomes.
#'
#' @export
ffp_risk.default <- function(x, p = NULL, confidence = 0.99, ...) {

  x <- ffp_stats_prepare_data(x)
  confidence <- ffp_risk_validate_confidence(confidence)

  p <- ffp_stats_prepare_probabilities(p = p, n_scenarios = nrow(x))

  new_ffp_risk_single(
    risk = ffp_risk_compute(x = x, p = p, confidence = confidence),
    p = p,
    confidence = confidence,
    n_scenarios = nrow(x),
    n_variables = ncol(x)
  )
}

#' @rdname ffp_risk
#'
#' @export
ffp_risk.ffp_opinion_pool <- function(x, confidence = 0.99, ...) {
  validate_ffp_opinion_pool(x)

  ffp_risk_opinion_pool(
    x = x$fit$model$scenarios,
    prior = x$prior,
    full_confidence = x$full_confidence_posterior,
    posterior = x$posterior,
    opinion_confidence = x$confidence,
    confidence = confidence
  )
}

ffp_risk_opinion_pool <- function(x, prior, full_confidence, posterior, opinion_confidence, confidence = 0.99) {
  x <- ffp_stats_prepare_data(x)

  confidence <- ffp_risk_validate_confidence(confidence)

  n_scenarios <- nrow(x)
  n_variables <- ncol(x)

  prior           <- ffp_stats_prepare_probabilities(p = prior,n_scenarios = n_scenarios)
  full_confidence <- ffp_stats_prepare_probabilities(p = full_confidence, n_scenarios = n_scenarios)
  posterior       <- ffp_stats_prepare_probabilities(p = posterior, n_scenarios = n_scenarios)

  prior_risk           <- ffp_risk_compute(x = x, p = prior, confidence = confidence)
  full_confidence_risk <- ffp_risk_compute(x = x, p = full_confidence, confidence = confidence)
  posterior_risk       <- ffp_risk_compute(x = x, p = posterior, confidence = confidence)

  new_ffp_risk_opinion_pool(
    prior = prior_risk,
    full_confidence = full_confidence_risk,
    posterior = posterior_risk,
    prior_p = prior,
    full_confidence_p = full_confidence,
    posterior_p = posterior,
    opinion_confidence = opinion_confidence,
    confidence = confidence,
    n_scenarios = n_scenarios,
    n_variables = n_variables
  )
}

new_ffp_risk_opinion_pool <- function(prior, full_confidence, posterior, prior_p, full_confidence_p, posterior_p, opinion_confidence, confidence, n_scenarios, n_variables) {

  prior_ens           <- ens(prior_p)
  full_confidence_ens <- ens(full_confidence_p)
  posterior_ens       <- ens(posterior_p)

  structure(
    list(
      type = "opinion_pool",
      prior = prior,
      full_confidence = full_confidence,
      posterior = posterior,
      full_confidence_change = ffp_risk_change(prior = prior, posterior = full_confidence),
      posterior_change = ffp_risk_change(prior = prior, posterior = posterior),
      effective_scenarios = tibble::tibble(
        distribution = c("prior", "full_confidence", "posterior"),
        ens = c(prior_ens, full_confidence_ens, posterior_ens),
        scenario_share = c(prior_ens, full_confidence_ens, posterior_ens) / n_scenarios),
      full_confidence_ens_retained = full_confidence_ens / prior_ens,
      ens_retained = posterior_ens / prior_ens,
      confidence = confidence,
      opinion_confidence = opinion_confidence,
      tail_probability = 1 - confidence,
      n_scenarios = n_scenarios,
      n_variables = n_variables
    ),
    class = "ffp_risk"
  )
}

ffp_risk_compute <- function(x, p, confidence) {

  p <- vctrs::vec_data(p)

  tail_probability <- 1 - confidence

  tail_stats <- vapply(
    seq_len(ncol(x)),
    function(column) {
      ffp_stats_lower_tail(x = x[, column], p = p, prob = tail_probability)
    },
    FUN.VALUE = c(quantile = 0, expected_shortfall = 0)
  )

  tibble::tibble(
    variable = colnames(x),
    value_at_risk = -unname(tail_stats["quantile", ]),
    expected_shortfall = -unname(tail_stats["expected_shortfall", ])
  )
}

ffp_risk_validate_confidence <- function(confidence) {
  valid <- is.numeric(confidence) && is.null(dim(confidence)) && length(confidence) == 1L && !is.na(confidence) && is.finite(confidence)

  if (!valid) {
    cli::cli_abort(
      "{.arg confidence} must be a single finite number."
    )
  }

  confidence <- as.double(confidence)

  if (confidence <= 0 || confidence >= 1) {
    cli::cli_abort(
      "{.arg confidence} must be strictly between 0 and 1."
    )
  }

  confidence
}

new_ffp_risk_single <- function(risk, p, confidence, n_scenarios, n_variables) {

  effective_scenarios <- ens(p)

  structure(
    list(
      type = "single",
      risk = risk,
      confidence = confidence,
      tail_probability = 1 - confidence,
      effective_scenarios = effective_scenarios,
      scenario_share = effective_scenarios / n_scenarios,
      n_scenarios = n_scenarios,
      n_variables = n_variables
    ),
    class = "ffp_risk"
  )
}

#' @rdname ffp_risk
#'
#' @export
ffp_risk.ffp_fit <- function(x, confidence = 0.99, ...) {

  validate_ffp_fit(x)

  ffp_risk_compare(
    x = x$model$scenarios,
    prior = x$prior,
    posterior = x$posterior,
    confidence = confidence
  )
}

ffp_risk_compare <- function(x, prior, posterior, confidence = 0.99) {

  x <- ffp_stats_prepare_data(x)

  confidence <- ffp_risk_validate_confidence(confidence)

  n_scenarios <- nrow(x)
  n_variables <- ncol(x)

  prior <- ffp_stats_prepare_probabilities(p = prior, n_scenarios = n_scenarios)
  posterior <- ffp_stats_prepare_probabilities(p = posterior, n_scenarios = n_scenarios)
  prior_risk <- ffp_risk_compute(x = x, p = prior, confidence = confidence)
  posterior_risk <- ffp_risk_compute(x = x, p = posterior, confidence = confidence)

  new_ffp_risk_comparison(
    prior = prior_risk,
    posterior = posterior_risk,
    prior_p = prior,
    posterior_p = posterior,
    confidence = confidence,
    n_scenarios = n_scenarios,
    n_variables = n_variables
  )
}

ffp_risk_change <- function(prior, posterior) {

  risk_names <- c("value_at_risk", "expected_shortfall")

  change             <- posterior
  change[risk_names] <- posterior[risk_names] - prior[risk_names]
  change

}

new_ffp_risk_comparison <- function(
    prior,
    posterior,
    prior_p,
    posterior_p,
    confidence,
    n_scenarios,
    n_variables
) {
  prior_ens <- ens(prior_p)
  posterior_ens <- ens(posterior_p)

  structure(
    list(
      type = "comparison",
      prior = prior,
      posterior = posterior,
      change = ffp_risk_change(prior = prior, posterior = posterior),
      effective_scenarios = tibble::tibble(
        distribution = c("prior", "posterior"),
        ens = c(prior_ens, posterior_ens),
        scenario_share = c(prior_ens / n_scenarios, posterior_ens / n_scenarios)
      ),
      ens_retained = posterior_ens / prior_ens,
      confidence = confidence,
      tail_probability = 1 - confidence,
      n_scenarios = n_scenarios,
      n_variables = n_variables
    ),
    class = "ffp_risk"
  )
}

#' @export
as_tibble.ffp_risk <- function(x, ...) {
  if (identical(x$type, "single")) {
    return(tibble::as_tibble(x$risk))
  }

  if (identical(x$type, "opinion_pool")) {
    prior <- dplyr::mutate(x$prior, distribution = "prior", .after = "variable")
    full_confidence <- dplyr::mutate(x$full_confidence, distribution = "full_confidence", .after = "variable")
    posterior <- dplyr::mutate(x$posterior, distribution = "posterior", .after = "variable")

    return(dplyr::bind_rows(prior, full_confidence, posterior))
  }

  prior     <- dplyr::mutate(x$prior, distribution = "prior", .after = "variable")
  posterior <- dplyr::mutate(x$posterior, distribution = "posterior", .after = "variable")

  dplyr::bind_rows(prior, posterior)
}

#' @export
print.ffp_risk <- function(x, ...) {
  if (identical(x$type, "opinion_pool")) {
    return(ffp_risk_print_opinion_pool(x))
  }

  if (identical(x$type, "comparison")) {
    return(ffp_risk_print_comparison(x))
  }

  ffp_risk_print_single(x)
}

ffp_risk_print_single <- function(x) {
  cli::cli_text("{.cls ffp_risk}")
  cli::cli_text("Tail risk at {sprintf('%.1f%%', 100 * x$confidence)} confidence")
  cli::cli_text("")

  print(x$risk)

  cli::cli_text("")
  cli::cli_dl(c(
    "ENS" = sprintf("%.1f", x$effective_scenarios),
    "Scenario share" = sprintf("%.1f%%", 100 * x$scenario_share)
    )
  )

  invisible(x)
}

ffp_risk_print_comparison <- function(x) {
  cli::cli_text("{.cls ffp_risk}")
  cli::cli_text(
    "Prior -> posterior tail risk at {sprintf('%.1f%%', 100 * x$confidence)} confidence"
  )
  cli::cli_text("")

  risk <- dplyr::left_join(x$prior, x$posterior, by = "variable", suffix = c("_prior", "_posterior"))

  print(risk)

  cli::cli_text("")
  cli::cli_dl(c(
    "Prior ENS" = sprintf(
      "%.1f", x$effective_scenarios$ens[x$effective_scenarios$distribution == "prior"]),
    "Posterior ENS" = sprintf("%.1f", x$effective_scenarios$ens[x$effective_scenarios$distribution == "posterior"]),
    "ENS retained" = sprintf("%.1f%%", 100 * x$ens_retained)
  ))

  invisible(x)
}

ffp_risk_print_opinion_pool <- function(x) {
  cli::cli_text("{.cls ffp_risk}")
  cli::cli_text(
    paste0(
      "Prior -> full confidence -> posterior tail risk at ",
      "{sprintf('%.1f%%', 100 * x$confidence)} confidence"
    )
  )
  cli::cli_text(
    "Opinion confidence: {sprintf('%.1f%%', 100 * x$opinion_confidence)}"
  )
  cli::cli_text("")

  print(
    tibble::as_tibble(x)
  )

  cli::cli_text("")
  cli::cli_dl(
    c(
      "Prior ENS" = sprintf(
        "%.1f",
        x$effective_scenarios$ens[
          x$effective_scenarios$distribution == "prior"
        ]
      ),
      "Full-confidence ENS" = sprintf(
        "%.1f",
        x$effective_scenarios$ens[
          x$effective_scenarios$distribution == "full_confidence"
        ]
      ),
      "Posterior ENS" = sprintf(
        "%.1f",
        x$effective_scenarios$ens[
          x$effective_scenarios$distribution == "posterior"
        ]
      ),
      "ENS retained" = sprintf(
        "%.1f%%",
        100 * x$ens_retained
      )
    )
  )

  invisible(x)
}
