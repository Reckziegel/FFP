# Full Information --------------------------------------------------------

#' @keywords internal
make_crisp <- function(x, condition) {
  if (anyNA(condition)) {
    ffp_abort(
      c(
        "Crisp conditioning must not contain missing values.",
        "i" = "Each scenario must be either selected or excluded."
      ),
      class = c("ffp_error_invalid_prior_condition", "ffp_error_invalid_prior")
    )
  }

  if (!any(condition)) {
    ffp_abort(
      c(
        "The crisp conditioning set is empty.",
        "x" = "No scenario satisfies the conditioning condition.",
        "i" = paste0(
          "Adjust the condition so that at least one scenario receives ",
          "positive probability."
        )
      ),
      class = c("ffp_error_empty_conditioning_set", "ffp_error_invalid_prior")
    )
  }

  crisp_conditioning_probabilities(condition)
}

#' @keywords internal
make_decay <- function(x, lambda) {
  n_scenarios <- vctrs::vec_size(x)
  half_life <- log(2) / lambda

  exp_decay_probabilities(n_scenarios = n_scenarios, half_life = half_life)
}

#' @keywords internal
make_kernel_normal <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    return(
      kernel_conditioning_probabilities(
        values = as.double(x),
        target = mean,
        bandwidth = sqrt(sigma)
      )
    )
  }

  p <- mvtnorm::dmvnorm(x = x, mean = mean, sigma = sigma)

  if (any(p == 0)) {
    p[p == 0] <- 1e-30
  }

  p <- p / sum(p)

  as.double(p)
}


# Partial Information -----------------------------------------------------

#' @keywords internal
make_kernel_entropy <- function(x, mean, sigma) {
  least_info_kernel(Y = x, y = mean, h2 = sigma) |>
    as.double()
}

#' @keywords internal
make_double_decay <- function(x, decay_low, decay_high) {
  moments <- DoubleDecay(x = x, decay_low = decay_low, decay_high = decay_high)
  fit_to_moments(X = x, m = moments$m, S = moments$s) |>
    as.double()
}


