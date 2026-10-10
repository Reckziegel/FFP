#' Combine Flexible Probability Distributions
#'
#' Combines already-realized flexible probability distributions through
#' arithmetic or weighted averaging.
#'
#' `average_ffp()` assigns equal weight to each supplied distribution.
#'
#' `combine_ffp()` assigns user-supplied non-negative weights that sum to one.
#'
#' @param ... Objects of class `ffp` with the same number of scenarios.
#' @param weights A numeric vector containing one non-negative weight per
#'   probability distribution. The weights must sum to one.
#'
#' @return An object of class `ffp`.
#'
#' @export
#'
#' @examples
#' p1 <- as_ffp(c(0.2, 0.3, 0.5))
#' p2 <- as_ffp(c(0.1, 0.4, 0.5))
#'
#' average_ffp(p1, p2)
#' combine_ffp(p1, p2, weights = c(0.7, 0.3))
average_ffp <- function(...) {
  probabilities   <- collect_ffp_distributions(...)
  n_distributions <- length(probabilities)
  weights         <- rep(1 / n_distributions, n_distributions)

  combine_ffp_distributions(probabilities = probabilities,weights = weights)
}


#' @rdname average_ffp
#' @export
combine_ffp <- function(..., weights) {
  probabilities <- collect_ffp_distributions(...)

  if (missing(weights)) {
    cli::cli_abort(
      c(
        "{.fn combine_ffp} requires {.arg weights}.",
        "i" = paste0(
          "Supply one non-negative weight per probability distribution, ",
          "with the weights summing to 1."
        )
      )
    )
  }

  weights <- validate_ffp_combination_weights(weights = weights, n_distributions = length(probabilities))

  combine_ffp_distributions(probabilities = probabilities, weights = weights)
}


collect_ffp_distributions <- function(...) {
  probabilities <- unname(rlang::list2(...))

  if (length(probabilities) == 0L) {
    cli::cli_abort("At least one {.cls ffp} object must be supplied.")
  }

  valid <- vapply(probabilities, is_ffp, logical(1))

  if (!all(valid)) {
    invalid <- which(!valid)

    cli::cli_abort(
      c(
        "All supplied distributions must be {.cls ffp} objects.",
        "x" = "Invalid input position(s): {.val {invalid}}."
      )
    )
  }

  sizes <- vapply(probabilities, vctrs::vec_size, integer(1))

  if (length(unique(sizes)) != 1L) {
    cli::cli_abort(
      c(
        "All {.cls ffp} distributions must have the same number of scenarios.",
        "x" = "Received sizes: {.val {sizes}}."
      )
    )
  }

  probabilities
}


validate_ffp_combination_weights <- function(weights, n_distributions, call = rlang::caller_env()) {
  valid <- is.numeric(weights) && is.null(dim(weights)) && length(weights) == n_distributions && !anyNA(weights) && all(is.finite(weights))

  if (!valid) {
    ffp_abort(
      c(
        "{.arg weights} must be a finite numeric vector with one value per distribution.",
        "i" = paste0("Expected ", n_distributions, " weight(s).")
      ),
      class = "ffp_error_invalid_combination_weights",
      call = call
    )
  }

  if (any(weights < 0)) {
    ffp_abort(
      "{.arg weights} must be non-negative.",
      class = "ffp_error_invalid_combination_weights",
      call = call
    )
  }

  total <- sum(weights)

  if (!probability_sum_is_one(total)) {
    ffp_abort(
      c(
        "{.arg weights} must sum to 1.",
        "x" = paste0("Current sum is ", format(total, digits = 15, scientific = FALSE, trim = TRUE), ".")
      ),
      class = "ffp_error_invalid_combination_weights",
      call = call
    )
  }

  canonicalize_probabilities(weights)
}


combine_ffp_distributions <- function(probabilities, weights) {
  probability_matrix <- do.call(cbind, lapply(probabilities, vctrs::vec_data))

  combined <- as.vector(probability_matrix %*% weights)
  combined <- canonicalize_probabilities(combined)

  as_ffp(combined)
}
