#' Combine Flexible Probabilities
#'
#' Functions to combine multiple `ffp` vectors while preserving the probability
#' distribution invariant.
#'
#' @param ... For `average_ffp()`: ffp objects of the same length.
#'   For `combine_ffp()`: named arguments `p1`, `p2`, ... representing ffp vectors.
#' @param weights Numeric vector of weights (must be non-negative and sum to 1).
#'   For `average_ffp()`, weights are implicitly uniform.
#'
#' @return An `ffp` object representing the combined/averaged probability distribution.
#'
#' @details
#' These functions implement valid probability combinations:
#'
#' - **`average_ffp()`**: Computes the arithmetic mean of multiple probability distributions.
#'   Equivalent to `combine_ffp()` with equal weights. The result is always a valid
#'   probability distribution.
#'
#' - **`combine_ffp()`**: Computes a weighted average of probability distributions.
#'   Weights must sum to 1. Commonly used for:
#'   - Combining expert opinions (weights = expert confidence)
#'   - View mixing in entropy pooling
#'   - Scenario resampling with belief updates
#'
#' @export
#' @examples
#' # Two probability distributions
#' p1 <- ffp(c(0.2, 0.3, 0.5))
#' p2 <- ffp(c(0.1, 0.4, 0.5))
#'
#' # Equal-weight average
#' average_ffp(p1, p2)
#'
#' # Weighted combination (70% p1, 30% p2)
#' combine_ffp(p1 = p1, p2 = p2, weights = c(0.7, 0.3))
average_ffp <- function(...) {
  dots <- list(...)
  n <- length(dots)

  if (n == 0) {
    cli::cli_abort("At least one {.cls ffp} object must be provided.")
  }

  # Check all are ffp
  for (i in seq_along(dots)) {
    if (!is_ffp(dots[[i]])) {
      cli::cli_abort(c(
        "All arguments must be {.cls ffp} objects.",
        "x" = "Argument {i} is class {.cls {class(dots[[i]])}} instead."
      ))
    }
  }

  # Check all have same size
  sizes <- purrr::map_int(dots, vctrs::vec_size)
  if (!all(sizes == sizes[1])) {
    cli::cli_abort(c(
      "All {.cls ffp} objects must have the same length.",
      "x" = "Got sizes: {.val {unique(sizes)}}"
    ))
  }

  # Compute simple average directly
  weights <- rep(1 / n, n)
  result <- rep(0, sizes[1])
  for (i in seq_along(dots)) {
    result <- result + weights[i] * vctrs::vec_data(dots[[i]])
  }

  ffp(result)
}

#' @rdname average_ffp
#' @export
combine_ffp <- function(..., weights) {
  dots <- list(...)

  if (length(dots) == 0) {
    cli::cli_abort("At least one {.cls ffp} object must be provided.")
  }

  # Check all are ffp
  for (i in seq_along(dots)) {
    if (!is_ffp(dots[[i]])) {
      cli::cli_abort(c(
        "All arguments must be {.cls ffp} objects.",
        "x" = "Argument {i} is class {.cls {class(dots[[i]])}} instead."
      ))
    }
  }

  # Check weights
  if (!is.numeric(weights)) {
    cli::cli_abort("{.arg weights} must be a numeric vector, not {.cls {class(weights)}}")
  }

  if (length(weights) != length(dots)) {
    cli::cli_abort(c(
      "Number of weights must match number of {.cls ffp} objects.",
      "x" = "Got {length(weights)} weights but {length(dots)} objects."
    ))
  }

  if (any(weights < 0)) {
    cli::cli_abort(c(
      "{.arg weights} must be non-negative.",
      "x" = "Found {sum(weights < 0)} negative weight(s)."
    ))
  }

  if (!dplyr::near(sum(weights), 1, tol = 0.001)) {
    cli::cli_abort(c(
      "{.arg weights} must sum to 1 (within tolerance 0.001).",
      "i" = "Sum of weights: {.val {sum(weights)}}"
    ))
  }

  # Check all have same size
  sizes <- purrr::map_int(dots, vctrs::vec_size)
  if (!all(sizes == sizes[1])) {
    cli::cli_abort(c(
      "All {.cls ffp} objects must have the same length.",
      "x" = "Got sizes: {.val {unique(sizes)}}"
    ))
  }

  # Compute weighted combination
  result <- rep(0, sizes[1])
  for (i in seq_along(dots)) {
    result <- result + weights[i] * vctrs::vec_data(dots[[i]])
  }

  # Return as ffp (already validated to be valid probability)
  ffp(result)
}
