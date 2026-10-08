#' Relative Entropy
#'
#' Computes the Kullback-Leibler divergence from a prior probability
#' distribution to a posterior probability distribution.
#'
#' @param prior A prior probability distribution.
#' @param posterior A posterior probability distribution.
#'
#' @return A non-negative numeric scalar containing the relative entropy.
#'
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

  prior     <- as.double(as_ffp(prior))
  posterior <- as.double(as_ffp(posterior))

  entropy_kl_divergence(posterior = posterior,prior = prior)

}
