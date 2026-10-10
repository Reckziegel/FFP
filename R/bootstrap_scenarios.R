#' Bootstrap Scenarios with Flexible Probabilities
#'
#' Resamples joint scenarios according to flexible probabilities.
#'
#' @param x Scenario data, an `ffp_fit` object, or an
#'   `ffp_opinion_pool` object.
#' @param ... Arguments passed to methods.
#'
#' @return A tibble containing the resampled joint scenarios.
#'
#' @export
bootstrap_scenarios <- function(x, ...) {
  UseMethod("bootstrap_scenarios")
}


#' @export
bootstrap_scenarios.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "{.fn bootstrap_scenarios} does not support class {.cls {class(x)[[1L]]}}.",
      "i" = paste0(
        "Supply scenario data, an {.cls ffp_fit}, or an {.cls ffp_opinion_pool} object."
      )
    )
  )
}


#' @rdname bootstrap_scenarios
#' @param p An object of class `ffp`.
#' @param n A positive integer giving the number of bootstrap draws.
#' @export
bootstrap_scenarios.numeric <- function(x, p, n, ...) {
  bootstrap_scenarios_legacy(x = x, p = p, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.matrix <- function(x, p, n, ...) {
  bootstrap_scenarios_legacy(x = x, p = p, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.ts <- function(x, p, n, ...) {
  bootstrap_scenarios_legacy(x = x, p = p, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.xts <- function(x, p, n, ...) {
  bootstrap_scenarios_legacy(x = x, p = p, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.data.frame <- function(x, p, n, ...) {
  bootstrap_scenarios_legacy(x = x, p = p, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.tbl <- function(x, p, n, ...) {
  bootstrap_scenarios_legacy(x = x, p = p, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.ffp_fit <- function(x, n, p = NULL, ...) {
  validate_ffp_fit(x)

  if (!is.null(p)) {
    cli::cli_abort(
      c(
        "{.arg p} must not be supplied for an {.cls ffp_fit} object.",
        "i" = paste0(
          "The posterior probabilities are obtained directly from the fitted object."
        )
      )
    )
  }

  bootstrap_scenarios_core(x = x$model$scenarios,p = x$posterior, n = n)
}


#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.ffp_opinion_pool <- function(x, n, p = NULL, ...) {
  validate_ffp_opinion_pool(x)

  if (!is.null(p)) {
    cli::cli_abort(
      c(
        "{.arg p} must not be supplied for an {.cls ffp_opinion_pool} object.",
        "i" = paste0(
          "The confidence-weighted posterior probabilities are obtained ",
          "directly from the opinion-pool object."
        )
      )
    )
  }

  bootstrap_scenarios_core(x = x$fit$model$scenarios, p = x$posterior, n = n)
}


bootstrap_scenarios_legacy <- function(x, p, n) {
  if (!is_ffp(p)) {
    cli::cli_abort(
      "{.arg p} must be an {.cls ffp} object."
    )
  }
  bootstrap_scenarios_core(x = x, p = p, n = n)
}


bootstrap_scenarios_core <- function(x, p, n, call = rlang::caller_env()) {
  assert_is_positive_integer(n)

  prepared  <- prepare_scenarios(x, call = call)
  scenarios <- prepared$data

  validate_scenarios(scenarios, call = call)

  n_scenarios <- scenario_count(scenarios)

  if (vctrs::vec_size(p) != n_scenarios) {
    ffp_abort(
      c(
        "{.arg p} must have one probability per scenario.",
        "i" = "{.arg x} has {n_scenarios} scenarios.",
        "i" = "{.arg p} has {vctrs::vec_size(p)} probabilities."
      ),
      class = "ffp_error_invalid_probabilities",
      call = call
    )
  }

  p <- as_ffp(p)
  indices <- sample.int(n = n_scenarios, size = as.integer(n), replace = TRUE, prob = as.double(p))
  bootstrap_scenario_rows(scenarios = scenarios, indices = indices)
}


bootstrap_scenario_rows <- function(scenarios, indices) {
  if (is.numeric(scenarios) && is.null(dim(scenarios))) {
    return(tibble::tibble(V1 = scenarios[indices]))
  }

  if (inherits(scenarios, "xts") || inherits(scenarios, "ts")) {
    scenarios <- as.matrix(scenarios)
  }

  out <- scenarios[indices, , drop = FALSE]

  if (is.matrix(out)) {
    if (is.null(colnames(out))) {
      colnames(out) <- paste0("V", seq_len(ncol(out)))
    }
    #return(tibble::as_tibble(out, .name_repair = "minimal"))
  }

  tibble::as_tibble(out, .name_repair = "minimal")
}
