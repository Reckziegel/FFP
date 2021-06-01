#' #' Call Option Pricing
#' #'
#' #' Computes the price of an European-Call Option.
#' #'
#' #' @param p A \code{numeric} with the stock prices.
#' #' @param k A \code{numeric} with the different strike values.
#' #' @param r A \code{numeric} with the risk-free rates (in years).
#' #' @param t A \code{numeric} with the time to expire (in years).
#' #' @param s A \code{numeric} with the volatility (in years).
#' #'
#' #' @return A \code{numeric} vector.
#' #'
#' #' @examples
#' #' call_price(p = 100, k = 98, r = 0.02 / 252, t = 21 / 252, s = 0.2)
#' #'
#' #' # works on vectors as well
#' #' call_price(p = c(100, 101, 102), k = 98, r = 0.02 / 252, t = 21 / 252, s = 0.2)
#' call_price <- function(p, k, r, t, s) {
#'   d_1 <- log(p / k) + (r + s * s / 2) * t
#'   d_2 <- d_1 - s * sqrt(t)
#'   c <- p * stats::pnorm(d_1) - k * exp(-r * t) * stats::pnorm(d_2)
#'   c
#' }
