
# Full Information --------------------------------------------------------

#' @keywords internal
make_crisp <- function(x, condition) {
  p <- vector("double", vctrs::vec_size(x))
  p[condition] <- 1
  if (any(p == 0)) {
    p[p == 0] <- 1e-30
  }
  p <- p / sum(p)
  as.double(p)
}

#' @keywords internal
make_decay <- function(x, lambda) {
  T_ <- vctrs::vec_size(x)
  p <- exp(-lambda * (T_ - (1:T_)))
  if (any(p == 0)) {
    p[p == 0] <- 1e-30
  }
  p  <- p / sum(p)
  as.double(p)
}

#' @keywords internal
make_kernel_normal <- function(x, mean, sigma) {
  if (NCOL(x) == 1) {
    p <- stats::dnorm(x = x, mean = mean, sd = sqrt(sigma))
  } else {
    p <- mvtnorm::dmvnorm(x = x, mean = mean, sigma = sigma)
  }
  if (any(p == 0)) {
    p[p == 0] <- 1e-30
  }
  p <- p / sum(p)
  as.double(p)
}


# Partial Information -----------------------------------------------------

#' @keywords internal
make_kernel_entropy <- function(x, mean, sigma) {
  p <- LeastInfoKernel(x, mean, sigma)
  if (any(p == 0)) {
    p[p == 0] <- 1e-30
  }
  as.double(p)
}

#' @keywords internal
make_double_decay <- function(x, decay_low, decay_high) {
  dd <- DoubleDecay(x, decay_low, decay_high)
  p  <- Fit2Moms(x, dd$m, dd$s)
  if (any(p == 0)) {
    p[p == 0] <- 1e-30
  }
  as.double(p)
}


# Empirical Stats ---------------------------------------------------------

#' @keywords internal
make_empirical_stats <- function(x, p, level) {

  T_ <- nrow(x)
  N  <- ncol(x)

  p_mat <- matrix(p, nrow = T_, ncol = N)

  # mean
  mu <- as.vector(t(p) %*% x)

  # sd
  if (N == 1) {
    sd <- sqrt(sum(((x  - mu) ^ 2) * p))
  } else {
    sd <- sqrt(colSums(((x - mu) ^ 2) * p_mat))
  }

  # covariance
  # mu_shift <- x - mu
  # if (N == 1) {
  #   cov <- t(mu_shift * matrix(p, nrow = T_, ncol = N)) %*% mu_shift
  #   cov <- as.vector(cov)
  # } else {
  #   cov <- t(mu_shift * p_mat) %*% mu_shift
  #   cov <- (cov + t(cov)) / 2 # ensure true symmetric outcome
  # }

  # skew
  if (N == 1) {
    sk <- sum(p * ((x - mu) ^ 3)) / (sd ^ 3)
  } else {
    sk <- colSums(p_mat * ((x - mu) ^ 3)) / (sd ^ 3)
  }

  # kurtosis
  if (N == 1) {
    kurt <- sum(p * ((x - mu) ^ 4)) / (sd ^ 4)
  } else {
    kurt <- colSums(p_mat * ((x - mu) ^ 4)) / (sd ^ 4)
  }

  # VaR & CVaR
  if (N == 1) {
    tmp <- sort(as.vector(x), index.return = TRUE)
    SortedEps <- tmp$x
    idx <- tmp$ix
    SortedP <- p[idx]
    VarPos <- which(cumsum(SortedP) <= level)
    VaR <- min(-SortedEps[VarPos])
    # Conditional VaR (Expected-Shortfall)
    CVaR <- -sum(SortedEps[VarPos] * SortedP[VarPos]) / sum(SortedP[VarPos])
  } else {
    # initialize the series
    VaR  <- NULL
    CVaR <- NULL
    for (n in 1:N) {
      tmp       <- sort(x[ , n, drop = FALSE], index.return = TRUE)
      SortedEps <- tmp$x
      idx       <- tmp$ix
      SortedP <- p[idx]
      VarPos <- which(cumsum(SortedP) <= level)
      new_VaR  <- min(-SortedEps[VarPos])
      new_CVaR <- -sum(SortedEps[VarPos] * SortedP[VarPos]) / sum(SortedP[VarPos])
      VaR <- c(VaR, new_VaR)
      CVaR <- c(CVaR, new_CVaR)
    }
  }

  out <- rbind(mu, sd, sk, kurt, VaR, CVaR)
  out_name <- colnames(x)
  if (is.null(out_name)) {
    colnames(out) <- paste0("V", 1:NCOL(x))
  } else {
    colnames(out) <- out_name
  }

  tibble::as_tibble(out) %>%
    dplyr::mutate(stat = c("Mu", "Std", "Skew", "Kurt", "VaR", "CVaR")) %>%
    dplyr::mutate(stat = as.factor(.data$stat)) %>%
    dplyr::select(.data$stat, dplyr::everything())

}

# make_scenarios ----------------------------------------------------------

#' @keywords internal
make_scenarios <- function(x, p, n) {
  # empirical cdf
  empirical_cdf <- vctrs::vec_c(0, cumsum(p))
  # random matrix
  rand_uniform <- stats::runif(n)
  # scenarios
  tmp <- pracma::histc(rand_uniform, empirical_cdf)
  ind <- tmp$bin
  X_sample <- x[ind, , drop = FALSE]
  X_sample
}
