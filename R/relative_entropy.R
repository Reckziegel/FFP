#' Relative Entropy
#'
#' Computes the relative entropy of two distributions.
#'
#' @param prior A prior probability distribution.
#' @param posterior A posterior probability distribution.
#'
#' @return A \code{double} with the relative entropy.
#' @export
#'
#' @examples
#' set.seed(222)
#'
#' prior <- rep(1 / 100, 100)
#'
#' posterior <- runif(100)
#' posterior <- posterior / sum(posterior)
#'
#' relative_entropy(prior, posterior)
relative_entropy <- function(prior, posterior) {

  if (vctrs::vec_size(prior) != vctrs::vec_size(posterior)) {
    cli::cli_abort(
      c("x" = "The {.arg prior} and {.arg posterior} must have the same length.",
        "i" = "Provided lengths: prior = {vctrs::vec_size(prior)}, posterior = {vctrs::vec_size(posterior)}")
    )
  }

  prior     <- as_ffp(prior)
  posterior <- as_ffp(posterior)

  prior     <- vctrs::vec_data(prior)
  posterior <- vctrs::vec_data(posterior)

  sum(posterior * (log(posterior) - log(prior)))

}
