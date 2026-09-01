#' Effective Number of Scenarios
#'
#' Computes the effective number of scenarios implied by a flexible probability
#' distribution.
#'
#' The effective number of scenarios is defined as the exponential of the
#' Shannon entropy:
#'
#' \deqn{
#' ENS = \exp\left(-\sum_t p_t \log p_t\right).
#' }
#'
#' Probabilities equal to zero make no contribution to the entropy, following
#' the convention \eqn{0 \log(0) = 0}.
#'
#' @param p An object of class `ffp`.
#'
#' @return A single `double`.
#'
#' @export
#'
#' @examples
#' p <- as_ffp(rep(0.01, 100))
#' ens(p)
#'
#' p <- as_ffp(c(0.5, 0.5, 0))
#' ens(p)
ens <- function(p) {
  if (!is_ffp(p)) {
    cli::cli_abort("{.arg p} must be an {.cls ffp} object.")
  }
  p <- vctrs::vec_data(p)
  positive_probability <- p > 0
  exp(-sum(p[positive_probability] * log(p[positive_probability])))
}
