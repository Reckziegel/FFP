#' Flexible Probabilities Driven Bootstrap
#'
#' Resamples historical scenarios with flexible probabilities.
#'
#' The argument `x` is supposed to have the same size of `p`.
#'
#' @param x A time series defining the scenario-probability distribution.
#' @param p An object of the `ffp` class.
#' @param n An \code{integer} scalar with the number of scenarios to be generated.
#'
#' @return A \code{tibble} with the number of rows equal to `n`.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' ret <- diff(log(EuStockMarkets))
#' ew  <- rep(1 / nrow(ret), nrow(ret))
#'
#' bootstrap_scenarios(x = ret, p = as_ffp(ew), n = 10)
bootstrap_scenarios <- function(x, p, n) {
  UseMethod("bootstrap_scenarios", x)
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.numeric <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x)
  out <- make_scenarios(x, p, n)

  tibble::tibble(V1 = as.double(out), .name_repair = "minimal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.matrix <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  x <- as.matrix(x)


  out <- make_scenarios(x, p, n)
  if (is.null(colnames(x))) {
    colnames(out) <- paste0("V", 1:NCOL(x))
  } else {
    colnames(out) <- colnames(x)
  }
  tibble::as_tibble(out, name_repair = "minimal")

}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.ts <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x)

  out <- make_scenarios(x, p, n)

  if (is.null(colnames(x))) {
    colnames(out) <- paste0("V", 1:NCOL(out))
  } else {
    colnames(out) <- colnames(x)
  }
  tibble::as_tibble(out, name_repair = "minimal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.xts <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- matrix(x, nrow = NROW(x), ncol = NCOL(x))

  out <- make_scenarios(x, p, n)

  if (is.null(colnames(x))) {
    colnames(out) <- paste0("V", 1:NCOL(out))
  } else {
    colnames(out) <- colnames(x)
  }
  tibble::as_tibble(out, name_repair = "minimal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.tbl <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])

  out <- make_scenarios(x, p, n)

  if (is.null(colnames(x))) {
    colnames(x) <- tibble::set_tidy_names(x)
  } else {
    colnames(out) <- colnames(x)
  }
  tibble::as_tibble(out, name_repair = "minimal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.data.frame <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as.matrix(x[purrr::map_lgl(x, is.numeric)])

  out <- make_scenarios(x, p, n)

  if (is.null(colnames(x))) {
    colnames(x) <- tibble::set_tidy_names(x)
  } else {
    colnames(out) <- colnames(x)
  }
  tibble::as_tibble(out, name_repair = "minimal")
}



