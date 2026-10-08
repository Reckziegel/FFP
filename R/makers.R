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


# Empirical Stats ---------------------------------------------------------

#' @keywords internal
make_empirical_stats <- function(x, p, level) {

  T_ <- nrow(x)
  N <- ncol(x)

  p_mat <- matrix(
    p,
    nrow = T_,
    ncol = N
  )

  # mean
  mu <- as.vector(t(p) %*% x)

  # sd
  if (N == 1) {
    sd <- sqrt(sum(((x - mu)^2) * p))
  } else {
    sd <- sqrt(colSums(((x - mu)^2) * p_mat))
  }

  # covariance
  # mu_shift <- x - mu
  # if (N == 1) {
  #   cov <- t(
  #     mu_shift * matrix(p, nrow = T_, ncol = N)
  #   ) %*% mu_shift
  #   cov <- as.vector(cov)
  # } else {
  #   cov <- t(mu_shift * p_mat) %*% mu_shift
  #   cov <- (cov + t(cov)) / 2
  # }

  # skew
  if (N == 1) {
    sk <- sum(p * ((x - mu)^3)) / (sd^3)
  } else {
    sk <- colSums(p_mat * ((x - mu)^3)) / (sd^3)
  }

  # kurtosis
  if (N == 1) {
    kurt <- sum(p * ((x - mu)^4)) / (sd^4)
  } else {
    kurt <- colSums(p_mat * ((x - mu)^4)) / (sd^4)
  }

  # VaR & CVaR
  if (N == 1) {
    tmp <- sort(
      as.vector(x),
      index.return = TRUE
    )

    SortedEps <- tmp$x
    idx <- tmp$ix
    SortedP <- p[idx]
    VarPos <- which(cumsum(SortedP) <= level)
    VaR <- min(-SortedEps[VarPos])

    # Conditional VaR (Expected-Shortfall)
    CVaR <- -sum(
      SortedEps[VarPos] * SortedP[VarPos]
    ) / sum(SortedP[VarPos])
  } else {
    VaR <- NULL
    CVaR <- NULL

    for (n in 1:N) {
      tmp <- sort(
        x[, n, drop = FALSE],
        index.return = TRUE
      )

      SortedEps <- tmp$x
      idx <- tmp$ix
      SortedP <- p[idx]
      VarPos <- which(cumsum(SortedP) <= level)

      new_VaR <- min(-SortedEps[VarPos])
      new_CVaR <- -sum(
        SortedEps[VarPos] * SortedP[VarPos]
      ) / sum(SortedP[VarPos])

      VaR <- c(VaR, new_VaR)
      CVaR <- c(CVaR, new_CVaR)
    }
  }

  out <- rbind(
    mu,
    sd,
    sk,
    kurt,
    VaR,
    CVaR
  )

  out_name <- colnames(x)

  if (is.null(out_name)) {
    colnames(out) <- paste0(
      "V",
      1:NCOL(x)
    )
  } else {
    colnames(out) <- out_name
  }

  tibble::as_tibble(out) |>
    dplyr::mutate(
      stat = c(
        "Mu",
        "Std",
        "Skew",
        "Kurt",
        "VaR",
        "CVaR"
      )
    ) |>
    dplyr::mutate(
      stat = as.factor(stat)
    ) |>
    dplyr::select(
      stat,
      dplyr::everything()
    )
}


# make_scenarios ----------------------------------------------------------

#' @keywords internal
make_scenarios <- function(x, p, n) {
  empirical_cdf <- vctrs::vec_c(
    0,
    cumsum(vctrs::vec_data(p))
  )

  rand_uniform <- stats::runif(n)

  tmp <- histc(
    rand_uniform,
    empirical_cdf
  )

  ind <- tmp$bin
  x[ind, , drop = FALSE]
}
