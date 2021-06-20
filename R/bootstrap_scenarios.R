#' Flexible Probabilities Driven Bootstrap
#'
#' This function resamples scenarios given by the flexible probabilities
#' approach.
#'
#' @param x A time-series defining the scenario-probability distribution.
#' @param p A numeric vector or an object of the `ffp` class.
#' @param n An \code{integer} scalar with the number of scenarios to be generated.
#'
#' @return The argument `x` is supposed to have the same size of `p`.
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
  assert_is_probability(p)
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)
  out <- make_scenarios(x, p, n)

  # FIXME
  tibble::as_tibble(out, name_repair = "universal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.matrix <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assert_is_probability(p)
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )
  x <- as_ffp_mat(x)

  out <- make_scenarios(x, p, n)
  # FIXME
  tibble::as_tibble(out, name_repair = "universal")

}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.ts <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)

  out <- make_scenarios(x, p, n)

  # FIXME
  tibble::as_tibble(out, name_repair = "universal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.xts <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(x)

  out <- make_scenarios(x, p, n)

  # FIXME
  tibble::as_tibble(out, name_repair = "universal")
}

#' @rdname bootstrap_scenarios
#' @export
bootstrap_scenarios.tbl <- function(x, p, n) {
  vctrs::vec_assert(n, double(), 1)
  stopifnot(inherits(p, "ffp"))
  assertthat::assert_that(
    assertthat::are_equal(vctrs::vec_size(x), vctrs::vec_size(p))
  )

  x <- as_ffp_mat(purrr::keep(x, is.double))

  out <- make_scenarios(x, p, n)

  # FIXME
  tibble::as_tibble(out, name_repair = "universal")
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

  x <- as_ffp_mat(purrr::keep(x, is.double))

  out <- make_scenarios(x, p, n)

  # FIXME
  tibble::as_tibble(out, name_repair = "universal")
}



