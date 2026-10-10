#' Convert an Exponential Decay Rate to Half-Life
#'
#' Converts an exponential decay rate into its implied half-life.
#'
#' The relationship between the decay rate \eqn{\lambda} and half-life is
#'
#' \deqn{
#'   HL = \frac{\log(2)}{\lambda}.
#' }
#'
#' The result is expressed in observations. Its interpretation therefore
#' depends on the frequency of the scenario data. For daily data, for example,
#' a half-life of 42 corresponds to approximately 42 trading observations.
#'
#' @param lambda A single finite positive number giving the exponential decay
#'   rate.
#'
#' @return A single positive number containing the implied half-life in
#'   observations.
#'
#' @seealso [exp_decay()], [prior_exp_decay()], [double_decay()]
#'
#' @export
#'
#' @examples
#' half_life(0.0166)
half_life <- function(lambda) {

  valid <- is.numeric(lambda) && is.null(dim(lambda)) && length(lambda) == 1L && !is.na(lambda) && is.finite(lambda)

  if (!valid) cli::cli_abort("{.arg lambda} must be a single finite number.")
  if (lambda <= 0) cli::cli_abort("{.arg lambda} must be strictly positive.")

  log(2) / as.double(lambda)
}

