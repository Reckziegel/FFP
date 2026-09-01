#' Statistics Under Flexible Probabilities
#'
#' Computes marginal distribution statistics under flexible probabilities.
#'
#' @param x An object containing the scenarios or an object supported by
#'   an `ffp_stats()` method.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `ffp_stats`.
#'
#' @export
ffp_stats <- function(x, ...) {
  UseMethod("ffp_stats")
}


#' @rdname ffp_stats
#'
#' @param p An optional probability vector. It can be an `ffp` object or a
#'   numeric vector that can be converted to `ffp`. If `NULL`, equal
#'   probabilities are used.
#' @param prob A single number strictly between 0 and 1 defining the lower-tail
#'   probability used to compute `quantile` and `expected_shortfall`.
#'
#' @export
ffp_stats.default <- function(x, p = NULL, prob = 0.01, ...) {

  x <- ffp_stats_prepare_data(x)
  prob <- ffp_stats_validate_prob(prob)

  p <- ffp_stats_prepare_probabilities(
    p = p,
    n_scenarios = nrow(x)
  )

  new_ffp_stats_single(
    statistics = ffp_stats_compute(
      x = x,
      p = p,
      prob = prob
    ),
    p = p,
    prob = prob,
    n_scenarios = nrow(x),
    n_variables = ncol(x)
  )
}

new_ffp_stats_single <- function(
    statistics,
    p,
    prob,
    n_scenarios,
    n_variables
) {
  effective_scenarios <- ens(p)

  structure(
    list(
      type = "single",
      statistics = statistics,
      effective_scenarios = effective_scenarios,
      scenario_share = effective_scenarios / n_scenarios,
      n_scenarios = n_scenarios,
      n_variables = n_variables,
      prob = prob
    ),
    class = "ffp_stats"
  )
}

new_ffp_stats_comparison <- function(
    prior,
    posterior,
    prior_p,
    posterior_p,
    prob,
    n_scenarios,
    n_variables
) {
  prior_ens <- ens(prior_p)
  posterior_ens <- ens(posterior_p)

  change <- ffp_stats_change(
    prior = prior,
    posterior = posterior
  )

  effective_scenarios <- tibble::tibble(
    distribution = c(
      "prior",
      "posterior"
    ),
    ens = c(
      prior_ens,
      posterior_ens
    ),
    scenario_share = c(
      prior_ens / n_scenarios,
      posterior_ens / n_scenarios
    )
  )

  structure(
    list(
      type = "comparison",
      prior = prior,
      posterior = posterior,
      change = change,
      effective_scenarios = effective_scenarios,
      ens_retained = posterior_ens / prior_ens,
      n_scenarios = n_scenarios,
      n_variables = n_variables,
      prob = prob
    ),
    class = "ffp_stats"
  )
}

ffp_stats_compare <- function(
    x,
    prior,
    posterior,
    prob = 0.01
) {
  x <- ffp_stats_prepare_data(x)
  prob <- ffp_stats_validate_prob(prob)

  n_scenarios <- nrow(x)
  n_variables <- ncol(x)

  prior <- ffp_stats_prepare_probabilities(
    p = prior,
    n_scenarios = n_scenarios
  )

  posterior <- ffp_stats_prepare_probabilities(
    p = posterior,
    n_scenarios = n_scenarios
  )

  prior_stats <- ffp_stats_compute(
    x = x,
    p = prior,
    prob = prob
  )

  posterior_stats <- ffp_stats_compute(
    x = x,
    p = posterior,
    prob = prob
  )

  new_ffp_stats_comparison(
    prior = prior_stats,
    posterior = posterior_stats,
    prior_p = prior,
    posterior_p = posterior,
    prob = prob,
    n_scenarios = n_scenarios,
    n_variables = n_variables
  )
}

ffp_stats_change <- function(prior, posterior) {
  statistic_names <- setdiff(names(prior), "variable")

  change <- posterior
  change[statistic_names] <- posterior[statistic_names] - prior[statistic_names]
  change
}

#' @export
print.ffp_stats <- function(x, ...) {
  if (identical(x$type, "comparison")) {
    return(
      ffp_stats_print_comparison(x)
    )
  }

  ffp_stats_print_single(x)
}

ffp_stats_print_single <- function(x) {
  cli::cli_text("<{.cls ffp_stats}>")
  cli::cli_text("Flexible empirical distribution")
  cli::cli_text("")

  cli::cli_dl(c(
    "Scenarios" = format(
      x$n_scenarios,
      big.mark = ",",
      scientific = FALSE
    ),
    "Variables" = format(
      x$n_variables,
      big.mark = ",",
      scientific = FALSE
    ),
    "Tail probability" = sprintf(
      "%.1f%%",
      100 * x$prob
    )
  ))

  cli::cli_text("")
  cli::cli_text("{.strong Effective scenarios}")

  cli::cli_dl(c(
    "ENS" = format(
      round(x$effective_scenarios, 1),
      nsmall = 1,
      big.mark = ",",
      scientific = FALSE
    ),
    "Scenario share" = sprintf(
      "%.1f%%",
      100 * x$scenario_share
    )
  ))

  invisible(x)
}

ffp_stats_print_comparison <- function(x) {
  prior <- x$effective_scenarios[
    x$effective_scenarios$distribution == "prior",
    ,
    drop = FALSE
  ]

  posterior <- x$effective_scenarios[
    x$effective_scenarios$distribution == "posterior",
    ,
    drop = FALSE
  ]

  cli::cli_text("<{.cls ffp_stats}>")
  cli::cli_text("Prior -> posterior empirical distribution")
  cli::cli_text("")

  cli::cli_dl(c(
    "Scenarios" = format(
      x$n_scenarios,
      big.mark = ",",
      scientific = FALSE
    ),
    "Variables" = format(
      x$n_variables,
      big.mark = ",",
      scientific = FALSE
    ),
    "Tail probability" = sprintf(
      "%.1f%%",
      100 * x$prob
    )
  ))

  cli::cli_text("")
  cli::cli_text("{.strong Effective scenarios}")

  cli::cli_dl(c(
    "Prior ENS" = sprintf(
      "%.1f (%.1f%%)",
      prior$ens,
      100 * prior$scenario_share
    ),
    "Posterior ENS" = sprintf(
      "%.1f (%.1f%%)",
      posterior$ens,
      100 * posterior$scenario_share
    ),
    "ENS retained" = sprintf(
      "%.1f%%",
      100 * x$ens_retained
    )
  ))

  invisible(x)
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.ffp_stats <- function(x, ...) {
  if (identical(x$type, "single")) {
    return(
      tibble::as_tibble(
        x$statistics
      )
    )
  }

  prior <- dplyr::mutate(
    x$prior,
    distribution = "prior",
    .after = "variable"
  )

  posterior <- dplyr::mutate(
    x$posterior,
    distribution = "posterior",
    .after = "variable"
  )

  dplyr::bind_rows(
    prior,
    posterior
  )
}


ffp_stats_prepare_data <- function(x) {
  if (is.data.frame(x)) {
    is_numeric <- vapply(
      x,
      is.numeric,
      logical(1)
    )

    if (!any(is_numeric)) {
      cli::cli_abort(
        "{.arg x} must contain at least one numeric column."
      )
    }

    x <- as.matrix(
      x[, is_numeric, drop = FALSE]
    )
  } else if (is.numeric(x) && is.null(dim(x))) {
    x <- matrix(
      as.double(x),
      ncol = 1,
      dimnames = list(NULL, "x")
    )
  } else {
    x <- as.matrix(x)
  }

  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} must contain numeric scenarios."
    )
  }

  if (nrow(x) == 0L) {
    cli::cli_abort(
      "{.arg x} must contain at least one scenario."
    )
  }

  if (ncol(x) == 0L) {
    cli::cli_abort(
      "{.arg x} must contain at least one variable."
    )
  }

  if (any(!is.finite(x))) {
    cli::cli_abort(
      "{.arg x} must contain only finite values."
    )
  }

  storage.mode(x) <- "double"

  variable_names <- colnames(x)

  if (is.null(variable_names)) {
    variable_names <- if (ncol(x) == 1L) {
      "x"
    } else {
      paste0(
        "variable_",
        seq_len(ncol(x))
      )
    }

    colnames(x) <- variable_names
  }

  if (
    anyNA(variable_names) ||
    any(!nzchar(variable_names)) ||
    anyDuplicated(variable_names)
  ) {
    cli::cli_abort(
      "Variables in {.arg x} must have unique, non-empty names."
    )
  }

  x
}


ffp_stats_prepare_probabilities <- function(p, n_scenarios) {
  if (is.null(p)) {
    p <- rep(1 / n_scenarios, n_scenarios)
    return(as_ffp(p))
  }

  if (vctrs::vec_size(p) != n_scenarios) {
    cli::cli_abort(c(
      "{.arg p} must have the same number of observations as {.arg x}.",
      "i" = "{.arg x} has {n_scenarios} scenarios.",
      "i" = "{.arg p} has {vctrs::vec_size(p)} probabilities."
    ))
  }

  if (is_ffp(p)) {
    return(p)
  }

  as_ffp(p)
}


ffp_stats_validate_prob <- function(prob) {
  if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob)) {
    cli::cli_abort(
      "{.arg prob} must be a single finite number."
    )
  }

  prob <- as.double(prob)

  if (prob <= 0 || prob >= 1) {
    cli::cli_abort(
      "{.arg prob} must be strictly between 0 and 1."
    )
  }

  prob
}


ffp_stats_compute <- function(x, p, prob) {
  p <- vctrs::vec_data(p)

  mean <- unname(
    colSums(x * p)
  )

  centered <- sweep(
    x,
    MARGIN = 2,
    STATS = mean,
    FUN = "-"
  )

  variance <- unname(
    colSums(
      centered^2 * p
    )
  )

  sd <- sqrt(variance)

  third_moment <- unname(
    colSums(
      centered^3 * p
    )
  )

  fourth_moment <- unname(
    colSums(
      centered^4 * p
    )
  )

  skewness <- rep(
    NA_real_,
    ncol(x)
  )

  kurtosis <- rep(
    NA_real_,
    ncol(x)
  )

  non_degenerate <- sd > 0

  skewness[non_degenerate] <-
    third_moment[non_degenerate] /
    sd[non_degenerate]^3

  kurtosis[non_degenerate] <-
    fourth_moment[non_degenerate] /
    sd[non_degenerate]^4

  tail_stats <- vapply(
    seq_len(ncol(x)),
    function(column) {
      ffp_stats_lower_tail(
        x = x[, column],
        p = p,
        prob = prob
      )
    },
    FUN.VALUE = c(
      quantile = 0,
      expected_shortfall = 0
    )
  )

  tibble::tibble(
    variable = colnames(x),
    mean = mean,
    sd = sd,
    skewness = skewness,
    kurtosis = kurtosis,
    quantile = unname(
      tail_stats["quantile", ]
    ),
    expected_shortfall = unname(
      tail_stats["expected_shortfall", ]
    )
  )
}

ffp_stats_lower_tail <- function(x, p, prob) {

  ordering <- order(x)

  x <- x[ordering]
  p <- p[ordering]

  cumulative_probability <- cumsum(p)

  quantile_index <- which(cumulative_probability >= prob)[1L]
  quantile <- x[[quantile_index]]

  below_quantile <- x < quantile
  probability_below <- sum(p[below_quantile])
  remaining_probability <- prob - probability_below

  expected_shortfall <- (sum(x[below_quantile] * p[below_quantile]) + quantile * remaining_probability) / prob

  c(quantile = quantile, expected_shortfall = expected_shortfall)
}

#' @export
summary.ffp_stats <- function(
    object,
    variable = NULL,
    statistic = NULL,
    ...
) {
  if (!is.null(variable) && !is.null(statistic)) {
    cli::cli_abort(
      "Supply only one of {.arg variable} or {.arg statistic}."
    )
  }

  if (is.null(variable) && is.null(statistic)) {
    cli::cli_abort(c(
      "Supply either {.arg variable} or {.arg statistic}.",
      "i" = "Use {.code summary(x, variable = \"asset_a\")} to inspect one variable.",
      "i" = "Use {.code summary(x, statistic = \"sd\")} to compare one statistic."
    ))
  }

  if (!is.null(variable)) {
    variable <- ffp_stats_validate_summary_variable(
      object,
      variable
    )

    return(
      new_summary_ffp_stats(
        data = ffp_stats_summary_variable(
          object,
          variable
        ),
        view = "variable",
        label = variable,
        prob = object$prob
      )
    )
  }

  statistic <- ffp_stats_validate_summary_statistic(
    statistic
  )

  new_summary_ffp_stats(
    data = ffp_stats_summary_statistic(
      object,
      statistic
    ),
    view = "statistic",
    label = statistic,
    prob = object$prob
  )
}


new_summary_ffp_stats <- function(
    data,
    view,
    label,
    prob
) {
  structure(
    list(
      data = data,
      view = view,
      label = label,
      prob = prob
    ),
    class = "summary_ffp_stats"
  )
}

ffp_stats_statistic_names <- function() {
  c(
    "mean",
    "sd",
    "skewness",
    "kurtosis",
    "quantile",
    "expected_shortfall"
  )
}

ffp_stats_validate_summary_variable <- function(
    object,
    variable
) {
  if (
    !is.character(variable) ||
    length(variable) != 1L ||
    is.na(variable) ||
    !nzchar(variable)
  ) {
    cli::cli_abort(
      "{.arg variable} must be a single variable name."
    )
  }

  variables <- if (identical(object$type, "single")) {
    object$statistics$variable
  } else {
    object$prior$variable
  }

  if (!variable %in% variables) {
    cli::cli_abort(c(
      "Unknown variable {.val {variable}}.",
      "i" = "Available variables are: {.val {variables}}."
    ))
  }

  variable
}


ffp_stats_validate_summary_statistic <- function(
    statistic
) {
  statistics <- ffp_stats_statistic_names()

  if (
    !is.character(statistic) ||
    length(statistic) != 1L ||
    is.na(statistic) ||
    !nzchar(statistic)
  ) {
    cli::cli_abort(
      "{.arg statistic} must be a single statistic name."
    )
  }

  if (!statistic %in% statistics) {
    cli::cli_abort(c(
      "Unknown statistic {.val {statistic}}.",
      "i" = "Available statistics are: {.val {statistics}}."
    ))
  }

  statistic
}

ffp_stats_summary_variable <- function(
    object,
    variable
) {
  statistic_names <- ffp_stats_statistic_names()

  if (identical(object$type, "single")) {
    values <- object$statistics[
      object$statistics$variable == variable,
      statistic_names,
      drop = FALSE
    ]

    return(
      tibble::tibble(
        statistic = statistic_names,
        value = unname(
          as.numeric(values[1, ])
        )
      )
    )
  }

  prior <- object$prior[
    object$prior$variable == variable,
    statistic_names,
    drop = FALSE
  ]

  posterior <- object$posterior[
    object$posterior$variable == variable,
    statistic_names,
    drop = FALSE
  ]

  change <- object$change[
    object$change$variable == variable,
    statistic_names,
    drop = FALSE
  ]

  tibble::tibble(
    statistic = statistic_names,
    prior = unname(
      as.numeric(prior[1, ])
    ),
    posterior = unname(
      as.numeric(posterior[1, ])
    ),
    change = unname(
      as.numeric(change[1, ])
    )
  )
}

ffp_stats_summary_statistic <- function(
    object,
    statistic
) {
  if (identical(object$type, "single")) {
    return(
      tibble::tibble(
        variable = object$statistics$variable,
        value = object$statistics[[statistic]]
      )
    )
  }

  tibble::tibble(
    variable = object$prior$variable,
    prior = object$prior[[statistic]],
    posterior = object$posterior[[statistic]],
    change = object$change[[statistic]]
  )
}

#' @export
print.summary_ffp_stats <- function(
    x,
    ...
) {
  cli::cli_text("{.cls summary_ffp_stats}")

  if (identical(x$view, "variable")) {
    cli::cli_text(
      "Variable: {.val {x$label}}"
    )
  } else {
    label <- ffp_stats_format_statistic(
      statistic = x$label,
      prob = x$prob
    )

    cli::cli_text(
      "Statistic: {label}"
    )
  }

  cli::cli_text("")

  print(
    x$data
  )

  invisible(x)
}

ffp_stats_format_statistic <- function(
    statistic,
    prob
) {
  if (identical(statistic, "quantile")) {
    return(
      sprintf(
        "%.1f%% quantile",
        100 * prob
      )
    )
  }

  if (identical(statistic, "expected_shortfall")) {
    return(
      sprintf(
        "%.1f%% expected shortfall",
        100 * prob
      )
    )
  }

  statistic
}

#' @export
as_tibble.summary_ffp_stats <- function(
    x,
    ...
) {
  tibble::as_tibble(
    x$data
  )
}

#' @rdname ffp_stats
#'
#' @param x For the `ffp_fit` method, a fitted Fully Flexible Probabilities
#'   model.
#'
#' @details
#' When `x` is an `ffp_fit` object, `ffp_stats()` compares the empirical
#' distribution implied by the prior probabilities with the distribution
#' implied by the fitted posterior probabilities.
#'
#' The scenario support is obtained directly from the fitted model and is kept
#' fixed. Therefore, differences between prior and posterior statistics arise
#' exclusively from changes in scenario probabilities.
#'
#' The returned object contains the marginal statistics under the prior and
#' posterior distributions, their differences, and effective-number-of-scenarios
#' diagnostics for both probability distributions.
#'
#' @export
ffp_stats.ffp_fit <- function(
    x,
    prob = 0.01,
    ...
) {
  validate_ffp_fit(x)

  ffp_stats_compare(
    x = x$model$scenarios,
    prior = x$prior,
    posterior = x$posterior,
    prob = prob
  )
}
