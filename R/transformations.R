#' Half-Life Calculation
#'
#' Compute the implied half-life of a decay parameter.
#'
#' @param lambda A number.
#'
#' @return A single number with the half-life in days.
#'
#' @seealso \code{\link{exp_decay}} \code{\link{double_decay}}
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
