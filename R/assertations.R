# Internal helper to validate numeric vector as probability distribution
validate_probabilities <- function(x, tolerance) {
  if (!all(x >= 0)) {
    cli::cli_abort("Probabilities must be non-negative.")
  }

  if (!dplyr::near(sum(x), 1, tol = tolerance)) {
    cli::cli_abort(c(
      "Probabilities must sum to 1 (within tolerance).",
      "i" = "Sum: {.val {sum(x)}}, Tolerance: {.val {tolerance}}"
    ))
  }
}


assert_is_equal_size <- function(x, y) {
  assertthat::assert_that(
    vctrs::vec_size(x) == vctrs::vec_size(y)
  )
}


assert_is_logical <- function(x) {
  if (tibble::is_tibble(x)) {
    vctrs::vec_assert(x[[1]], logical())
  } else if (is.null(dim(x))) {
    vctrs::vec_assert(x, logical())
  } else {
    cli::cli_abort(
      "{.arg x} must be logical, not {.cls {class(x)}}"
    )
  }
}


assert_is_univariate <- function(x) {
  if (NCOL(x) > 1) {
    cli::cli_abort(c(
      "The conditioning variable {.arg x} should be univariate.",
      "x" = "Got {NCOL(x)} columns instead of 1."
    ))
  }
}


#' Assert that a vector is a valid probability distribution
#'
#' Checks that a numeric or `ffp` vector represents a valid probability
#' distribution. For numeric vectors, uses a tolerance of 0.001 for the
#' sum constraint.
#'
#' @param x A numeric or `ffp` vector.
#'
#' @return No return value, called for side effects (error raising).
#'
#' @keywords internal
assert_is_probability <- function(x) {
  if (is_ffp(x)) {
    return(invisible(NULL))
  }

  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "{.arg x} must be numeric or an object of class {.cls ffp}.",
      "x" = "Got class {.cls {class(x)}} instead."
    ))
  }

  validate_probabilities(
    as.double(x),
    tolerance = 0.001
  )

  invisible(NULL)
}


# Kernel-specific assertions ----------------------------------------------

assert_kernel_mean <- function(mean) {
  if (!(is.numeric(mean) && vctrs::vec_size(mean) == 1L)) {
    cli::cli_abort(
      "{.arg mean} must be a single numeric value, not {.cls {class(mean)}}"
    )
  }
}


assert_kernel_sigma <- function(sigma) {
  if (!(is.numeric(sigma) && vctrs::vec_size(sigma) == 1L)) {
    cli::cli_abort(
      "{.arg sigma} must be a single numeric value, not {.cls {class(sigma)}}"
    )
  }
}


#' Assert that a value is a positive integer scalar
#'
#' Validates that input is a single positive integer. Accepts numeric values
#' that can be converted to integer (e.g. `10.0`) but rejects non-integer
#' values (e.g. `10.5`).
#'
#' @param n A numeric or integer value to validate.
#'
#' @return The validated value as integer, invisibly.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' assert_is_positive_integer(10)
#' assert_is_positive_integer(10L)
#' assert_is_positive_integer(10.0)
#' assert_is_positive_integer(10.5)
#' assert_is_positive_integer(-5)
#' assert_is_positive_integer(c(1, 2))
#' }
assert_is_positive_integer <- function(n) {
  if (vctrs::vec_size(n) != 1L) {
    cli::cli_abort(c(
      "{.arg n} must be a scalar (single value).",
      "x" = "Got length {vctrs::vec_size(n)} instead of 1."
    ))
  }

  if (!is.numeric(n)) {
    cli::cli_abort(c(
      "{.arg n} must be numeric or integer.",
      "x" = "Got class {.cls {class(n)}} instead."
    ))
  }

  if (!dplyr::near(n, round(n))) {
    cli::cli_abort(c(
      "{.arg n} must be an integer (no fractional part).",
      "x" = "Got {.val {n}} instead."
    ))
  }

  if (n <= 0) {
    cli::cli_abort(c(
      "{.arg n} must be positive (> 0).",
      "x" = "Got {.val {n}} instead."
    ))
  }

  invisible(as.integer(round(n)))
}
