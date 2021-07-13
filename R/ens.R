#' Effective Number of Scenarios
#'
#' Shows how many scenarios are effectively been considered when using flexible
#' probabilities.
#'
#' @param p An object of the `ffp` class.
#'
#' @return A single `double`.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- exp_decay(stats::rnorm(100), 0.01)
#'
#' # ens is smaller than 100
#' ens(p)
ens <- function(p) {
  stopifnot(inherits(p, "ffp"))
  exp(-sum(p * log(p)))
}
