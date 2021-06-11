#' Compute the Half-Life
#'
#' This function computes the implied half-life of a decay parameter.
#'
#' @param lambda A number.
#'
#' @return A number with the half-life in days.
#'
#' @export
#'
#' @examples
#' half_life(0.0166)
#' half_life(0.01)
half_life <- function(lambda) {
  vctrs::vec_assert(lambda, numeric(), 1)
  round(log(2) / lambda)
}
