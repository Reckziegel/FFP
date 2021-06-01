
# Full Information --------------------------------------------------------

#' @keywords internal
make_crisp <- function(x, condition) {
  p <- vector("double", vctrs::vec_size(x))
  p[condition] <- 1
  p <- p / sum(p)
  as.double(p)
}

#' @keywords internal
make_smoothing <- function(x, lambda) {
  T_ <- vctrs::vec_size(x)
  p <- exp(-lambda * (T_ - (1:T_)))
  p  <- p / sum(p)
  as.double(p)
}

#' @keywords internal
make_kernel_normal <- function(x, mean, sigma) {
  p <- stats::dnorm(x = x, mean = mean, sd = sqrt(sigma))
  p <- p / sum(p)
  as.double(p)
}


# Partial Information -----------------------------------------------------

#' @keywords internal
make_kernel_entropy <- function(x, mean, sigma) {
  p <- LeastInfoKernel(x, mean, sigma)
  as.double(p)
}

#' @keywords internal
make_double_decay <- function(x, decay_low, decay_high) {
  dd <- DoubleDecay(x, decay_low, decay_high)
  p  <- Fit2Moms(x, dd$m, dd$s)
  as.double(p)
}


# Empirical Stats ---------------------------------------------------------

#' @keywords internal
make_empirical_stats <- function(x, p, level = 0.01) {

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
    tmp <- sort(x, index.return = TRUE)
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

  out <- cbind(mu, sd, sk, kurt, -VaR, -CVaR)
  colnames(out) <- c("mu", "sd", "skewness", "kurtosis", "VaR", "CVaR")
  if (!is.null(colnames(x))) {
    rownames(out) <- colnames(x)
  }

  out

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
