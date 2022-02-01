#' Input Relative Views
#'
#' Compute the relative over/under performance of assets.
#'
#' @param .invariant A time series defining the scenario-probability distribution.
#' @param .p An object of the `ffp` class.
#' @param .rank A \code{double} with the asset indexes.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' #x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' #colnames(x) <- colnames(EuStockMarkets)
#' #prior <- as.matrix(rep(1/nrow(x), nrow(x)))
#' # asset in the first col will outperform the asset in the second col.
#' #view_by_rank(.invariant = x, .p = prior, .rank = c(2, 1))
# view_by_rank <- function(.invariant, .p , .rank) {
#
#   #assertthat::assert_that(is.numeric(.rank), msg = "`.rank` must be a numeric vector.")
#
#   j <- NROW(.invariant)
#   #n <- NCOL(.invariant)
#   rank_size <- vctrs::vec_size(.rank)
#
#   # constrain probabilities to sum to one across all scenarios...
#   Aeq <- matrix(1, nrow = 1, ncol = j)
#   beq <- matrix(1, nrow = 1, ncol = 1)
#
#   # ...constrain the expectations... A*x <= 0
#   view <- .invariant[ , .rank[1:(rank_size - 1)]] - .invariant[ , .rank[2:rank_size]]
#   # Jx1 vector. Expectation is assigned to each scenario
#
#   # The expectation is that (Lower - Upper)x <= 0.
#   # (i.e. The returns of upper are greater than zero for each scenario)
#   A <- t(view)
#   b <- matrix(rep(0, nrow(A)), ncol = 1)
#
#   # ...compute posterior probabilities
#   entropy_pooling(p = .p, A = A, b = b, Aeq = Aeq, beq = beq)
#
# }
