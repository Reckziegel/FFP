
# PARTIAL INFORMATION -----------------------------------------------------

# Views on Means ----------------------------------------------------------

#' Views on Expected Returns
#'
#' Helper to construct views on expected returns.
#'
#' @param x An univariate or a multivariate distribution.
#' @param mean A \code{double} for the target location parameter of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Invariant
#' ret <- diff(log(EuStockMarkets))
#' n   <- nrow(ret)
#'
#' # Prior belief for expected returns (here is 2% for each asset)
#' mean <- rep(0.02, 4)
#'
#' # Prior probabilities (usually equal weight scheme)
#' prior <- rep(1 / n, n)
#'
#' # View
#' views <- view_on_mean(x = ret, mean = mean)
#' views
#'
#' # Optimization
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nloptr")
#' autoplot(ep)
#'
#' # Probabilities are twisted in such a way that the posterior
#' # `mu` match's exactly with previously stated beliefs
#' ffp_moments(x = ret, p = ep)$mu
view_on_mean <- function(x, mean) {
  UseMethod("view_on_mean", x)
}

#' @rdname view_on_mean
#' @export
view_on_mean.default <- function(x, mean) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_mean
#' @export
view_on_mean.matrix <- function(x, mean) {
  construct_view_on_mean(x, mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.xts <- function(x, mean) {
  construct_view_on_mean(as.matrix(x), mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.tbl_df <- function(x, mean) {
  construct_view_on_mean(tbl_to_mtx(x), mean)
}

#' @keywords internal
construct_view_on_mean <- function(x, mean) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), vctrs::vec_size(mean)))
  vctrs::vec_assert(mean, double())

  # ...constrain the first moments...
  Aeq <- t(x)
  beq <- mean

  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_mean")

}


# Views on Covariance Matrix ---------------------------------------------------

#' Views on the Covariance Matrix
#'
#' Helper to construct views on variance-covariance matrix.
#'
#' @param x An univariate or a multivariate distribution.
#' @param mean A \code{double} for the location parameter of the series in `x`.
#' @param sigma A \code{matrix} for the target variance-covariance parameter
#' of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Invariant (stationarity)
#' ret <- diff(log(EuStockMarkets))
#'
#' # Expectations for location and dispersion parameters
#' mean <- colMeans(ret) # No active expectations for returns
#' covs <- matrix(0, ncol = 4, nrow = 4) # assuming all assets are uncorrelated
#'                                       # very strong view!
#'
#' # prior probabilities (usually equal weight scheme)
#' prior <- rep(1 / nrow(ret), nrow(ret))
#'
#' # Views
#' views <- view_on_covariance(x = ret, mean = mean, sigma = covs)
#' views
#'
#' # Optimization
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
#' autoplot(ep)
#'
#' # original covariance matrix
#' stats::cov(ret)
#'
#' # Posterior covariance matrix
#' ffp_moments(x = ret, p = ep)$sigma
view_on_covariance <- function(x, mean, sigma) {
  UseMethod("view_on_covariance", x)
}

#' @keywords internal
#' @rdname view_on_covariance
view_on_covariance.default <- function(x, mean, sigma) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.matrix <- function(x, mean, sigma) {
  construct_view_on_covariance(x, mean, sigma)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.xts <- function(x, mean, sigma) {
  construct_view_on_covariance(as.matrix(x), mean, sigma)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.tbl_df <- function(x, mean, sigma) {
  construct_view_on_covariance(tbl_to_mtx(x), mean, sigma)
}

#' @keywords internal
construct_view_on_covariance <- function(x, mean, sigma) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), vctrs::vec_size(mean)))
  assertthat::assert_that(assertthat::are_equal(NCOL(sigma), NROW(sigma)))
  vctrs::vec_assert(mean, double())

  N  <- NCOL(x)

  SecMom <- sigma + mean %*% t(mean)   #...constrain the second moments...

  Aeq <- NULL
  beq <- NULL

  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq, t(x[ , k] * x[ , l]))
      beq <- rbind(beq, SecMom[k, l])
    }
  }

  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_covariance")

}


# Views on Correlation ----------------------------------------------------

#' Views on Correlation Structure
#'
#' Helper to construct views  on the correlation matrix.
#'
#' @param x An univariate or a multivariate distribution.
#' @param cor A \code{matrix} for the target correlation structure of
#' the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Invariant
#' ret <- diff(log(EuStockMarkets))
#'
#' # Assume that a panic event throws all correlations to the roof!
#' co <- matrix(0.95, 4, 4)
#' diag(co) <- 1
#' co
#'
#' # Prior probability (usually the equal-weight setting)
#' prior <- rep(1 / nrow(ret), nrow(ret))
#'
#' # View
#' views <- view_on_correlation(x = ret, cor = co)
#' views
#'
#' # Optimization
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
#' autoplot(ep)
#'
#' # prior correlation structure
#' stats::cor(ret)
#'
#' # posterior correlation structure matches the initial view very closely
#' stats::cov2cor(ffp_moments(x = ret, p = ep)$sigma)
view_on_correlation <- function(x, cor) {
  UseMethod("view_on_correlation", x)
}

#' @rdname view_on_correlation
#' @export
view_on_correlation.default <- function(x, cor) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_correlation
#' @export
view_on_correlation.matrix <- function(x, cor) {
  construct_view_on_correlation(x, cor)
}

#' @rdname view_on_correlation
#' @export
view_on_correlation.xts <- function(x, cor) {
  construct_view_on_correlation(as.matrix(x), cor)
}

#' @rdname view_on_correlation
#' @export
view_on_correlation.tbl_df <- function(x, cor) {
  construct_view_on_correlation(tbl_to_mtx(x), cor)
}

#' @keywords internal
construct_view_on_correlation <- function(x, cor) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), NROW(cor)))
  assertthat::assert_that(assertthat::are_equal(NCOL(cor), NROW(cor)))

  N <- NCOL(cor)
  mu <- colMeans(x)
  sd <- apply(x, 2, stats::sd)

  Aeq <- NULL
  beq <- NULL

  # Attach the view on cor
  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq ,t(x[ , k] * x[ , l]))
      beq <- rbind(beq, mu[[k]] * mu[[l]] + sd[[k]] * sd[[l]] * cor[k, l])
    }
  }

  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_correlation")

}


# Views on the Volatility -------------------------------------------------

#' Views on Volatility
#'
#' Helper to construct views on volatility.
#'
#' @param x An univariate or a multivariate distribution.
#' @param vol A \code{double} for the target volatility structure
#' of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Invariant
#' ret <- diff(log(EuStockMarkets))
#' n   <- nrow(ret)
#'
#' # Expected a volatility 30% higher than historical average
#' vol <- apply(ret, 2, stats::sd) * 1.3
#'
#' # Prior Probabilities
#' prior <- rep(1 / n, n)
#'
#' # Views
#' views <- view_on_volatility(x = ret, vol = vol)
#' views
#'
#' # Optimization
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
#' autoplot(ep)
#'
#' # Desired volatility
#' vol
#'
#' # Posterior volatility matches very closely with the desired volatility
#' sqrt(diag(ffp_moments(x = ret, p = ep)$sigma))
view_on_volatility <- function(x, vol) {
  UseMethod("view_on_volatility", x)
}

#' @rdname view_on_volatility
#' @export
view_on_volatility.default <- function(x, vol) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_volatility
#' @export
view_on_volatility.matrix <- function(x, vol) {
  construct_view_on_volatility(x, vol)
}

#' @rdname view_on_volatility
#' @export
view_on_volatility.xts <- function(x, vol) {
  construct_view_on_volatility(as.matrix(x), vol)
}

#' @rdname view_on_volatility
#' @export
view_on_volatility.tbl_df <- function(x, vol) {
  construct_view_on_volatility(tbl_to_mtx(x), vol)
}

#' @keywords internal
construct_view_on_volatility <- function(x, vol) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), vctrs::vec_size(vol)))
  vctrs::vec_assert(vol, double())

  Aeq <- NULL
  #beq <- NULL

  Aeq <- t(x) ^ 2
  beq <- colMeans(x) ^ 2 + vol ^ 2

  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_volatility")

}


# Views on Ranking --------------------------------------------------------

#' Views on Relative Performance
#'
#' Helper to construct views on relative performance of assets.
#'
#' @param x An univariate or a multivariate distribution.
#' @param rank A \code{double} with the asset indexes.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' # Invariants
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' n <- ncol(x)
#'
#' # asset in the first col will outperform the asset in the second col.
#' views <- view_on_rank(x = x, rank = c(2, 1))
#' views
view_on_rank <- function(x, rank) {
  UseMethod("view_on_rank", x)
}

#' @rdname view_on_rank
#' @export
view_on_rank.default <- function(x, rank) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_rank
#' @export
view_on_rank.matrix <- function(x, rank) {
  construct_view_on_rank(x = x, rank = rank)
}

#' @rdname view_on_rank
#' @export
view_on_rank.xts <- function(x, rank) {
  construct_view_on_rank(x = as.matrix(x), rank = rank)
}

#' @rdname view_on_rank
#' @export
view_on_rank.tbl_df <- function(x, rank) {
  construct_view_on_rank(x = tbl_to_mtx(x), rank = rank)
}

#' @keywords internal
construct_view_on_rank <- function(x, rank) {

  #assertthat::assert_that(assertthat::are_equal(NROW(x), NROW(p)))
  assertthat::assert_that(is.numeric(rank), msg = "`.rank` must be a numeric vector.")

  rank_size <- vctrs::vec_size(rank)

  # ...constrain the expectations... A*x <= 0
  view <- x[ , rank[1:(rank_size - 1)]] - x[ , rank[2:rank_size]]
  # Jx1 vector. Expectation is assigned to each scenario

  # The expectation is that (Lower - Upper)x <= 0.
  # (i.e. The returns of upper are greater than zero for each scenario)
  A <- t(view)
  b <- matrix(rep(0, nrow(A)), ncol = 1)

  vctrs::new_list_of(
    x      = list(A = A, b = b),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_rank")

}

# Views on tail codependence ----------------------------------------------

#' Views on Tail Dependence
#'
#' Helper to construct views on tail dependence.
#'
#' @param x A multivariate copula.
#' @param tail A \code{double} with tail index of each asset.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(1)
#'
#' # Invariants
#' x <- diff(log(EuStockMarkets))
#' u <- apply(x, 2, stats::pnorm) # normal copula (assumption, it could be anything)
#' n <- nrow(x)
#' tail <- c(0.49, 0.5, 0.5002, 0.5) # tail index
#'
#' # Prior Probabilities
#' prior <- rep(1 / n, n)
#'
#' # Views
#' views <- view_on_tail_dependence(x = u, tail = tail)
#' views
#'
#' # Optimization
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
#' autoplot(ep)
view_on_tail_dependence <- function(x, tail) {
  UseMethod("view_on_tail_dependence", x)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.default <- function(x, tail) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.matrix <- function(x, tail) {
  construct_view_on_tail_dependence(x = x, tail = tail)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.xts <- function(x, tail) {
  construct_view_on_tail_dependence(x = as.matrix(x), tail = tail)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.tbl_df <- function(x, tail) {
  construct_view_on_tail_dependence(x = tbl_to_mtx(x), tail = tail)
}

#' @keywords internal
construct_view_on_tail_dependence <- function(x, tail) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), vctrs::vec_size(tail)))
  vctrs::vec_assert(tail, double())

  cop <- matrix(NA_real_, nrow = nrow(x), ncol = ncol(x))
  for (i in 1:NCOL(x)) {
    cop[ , i] <- as.double(x[ , i] <= tail[[i]])
  }
  Aeq <- t(cop)
  beq <- tail

  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_tail_depedence"
  )

}

# FULL INFORMATION --------------------------------------------------------

# Views on copula

#' Views on Copulas
#'
#' Helper to construct constraints on copulas for entropy programming.
#'
#' @param x A multivariate copula.
#' @param simul A simulated target copula.
#' @param p An object of the `ffp` class.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' set.seed(1)
#' library(ggplot2)
#'
#' # Invariants
#' ret <- diff(log(EuStockMarkets))
#' u <- apply(ret, 2, stats::pnorm) # assuming normal copula
#' n <- nrow(u)
#'
#' #' Prior probability distribution
#' prior <- rep(1 / n, n)
#'
#' # Simulated marginals
#' simul_marg <- bootstrap_scenarios(ret, as_ffp(prior), as.double(n))
#'
#' # Copulas derived from the simulated margins
#' simul_cop <- apply(simul_marg, 2, stats::pnorm) # assuming normal copula
#'
#' views <- view_on_copula(x = u, simul = simul_cop, p = prior)
#' views
#'
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nloptr")
#' autoplot(ep)
view_on_copula <- function(x, simul, p) {
  UseMethod("view_on_copula", x)
}

#' @rdname view_on_copula
#' @export
view_on_copula.default <- function(x, simul, p) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_copula
#' @export
view_on_copula.matrix <- function(x, simul, p) {
  construct_view_on_copula(x = x, simul = check_input(simul), p = as_ffp(p))
}

#' @rdname view_on_copula
#' @export
view_on_copula.xts <- function(x, simul, p) {
  construct_view_on_copula(x = as.matrix(x), simul = check_input(simul), p = as_ffp(p))
}

#' @rdname view_on_copula
#' @export
view_on_copula.tbl_df <- function(x, simul, p) {
  construct_view_on_copula(x = tbl_to_mtx(x), simul = check_input(simul), p = as_ffp(p))
}

#' @keywords internal
construct_view_on_copula <- function(x, simul, p) {

  assertthat::assert_that(assertthat::are_equal(NROW(simul), NROW(p)))

  N <- NCOL(simul)

  Aeq <- NULL
  beq <- NULL

  Aeq <- rbind(Aeq, t(x))
  beq <- as.matrix(c(beq, rep(1 / 2, NCOL(x))))

  # order 2
  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq, t(x[ , k] * x[ , l]))
      beq <- rbind(beq, t(simul[ , k] * simul[ , l]) %*% p)
    }
  }

  # order 3
  for (k in 1:N) {
    for (l in k:N) {
      for (i in l:k) {
        Aeq <- rbind(Aeq, t(x[ , k] * x[ , l] * x[ , i]))
        beq <- rbind(beq, t(simul[ , k] * simul[ , l] * simul[ , i]) %*% p)
      }
    }
  }

  vctrs::new_list_of(
    x = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_copula"
  )

}

# Views on Marginal Distributions -----------------------------------------

#' Views on Marginal Distribution
#'
#' Helper to construct constraints on the marginal distribution.
#'
#' \itemize{
#'   \item `simul` must have the same number of columns than `x`
#'   \item `p` should have the same number of rows that `simul`.
#' }
#'
#' @param x An univariate or a multivariate distribution.
#' @param simul An univariate or multivariate simulated panel.
#' @param p An object of the `ffp` class.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' set.seed(1)
#' library(ggplot2)
#'
#' # Invariants
#' ret <- diff(log(EuStockMarkets))
#' n <- nrow(ret)
#'
#' #' Prior probability distribution
#' prior <- rep(1 / n, n)
#'
#' # Simulated marginals
#' simul <- bootstrap_scenarios(ret, as_ffp(prior), as.double(n))
#'
#' views <- view_on_marginal_distribution(x = ret, simul = simul, p = prior)
#' views
#'
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
#' autoplot(ep)
#'
#' # location matches
#' colMeans(simul)
#' ffp_moments(x = ret, p = ep)$mu
#'
#' # dispersion matches
#' cov(simul)
#' ffp_moments(x = ret, p = ep)$sigma
view_on_marginal_distribution <- function(x, simul, p) {
  UseMethod("view_on_marginal_distribution", x)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.default <- function(x, simul, p) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.matrix <- function(x, simul, p) {
  construct_view_on_marginal_distribution(x = x, simul = check_input(simul), p = as_ffp(p))
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.xts <- function(x, simul, p) {
  construct_view_on_marginal_distribution(x = as.matrix(x), simul = check_input(simul), p = as_ffp(p))
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.tbl_df <- function(x, simul, p) {
  construct_view_on_marginal_distribution(x = tbl_to_mtx(x), simul = check_input(simul), p = as_ffp(p))
}

#' @keywords internal
construct_view_on_marginal_distribution <- function(x, simul, p) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), NCOL(simul)))
  assertthat::assert_that(assertthat::are_equal(NROW(p), NROW(simul)))

  #N <- NCOL(x)

  Aeq <- NULL
  beq <- NULL

  # Location
  Aeq <- rbind(Aeq, t(x))
  beq <- rbind(beq, t(simul) %*% p)

  # Dispersion
  Aeq <- rbind(Aeq, t(x) ^ 2)
  beq <- rbind(beq, t(simul ^ 2) %*% p)

  # Skewness
  Aeq <- rbind(Aeq, t(x) ^ 3)
  beq <- rbind(beq, (t(simul) ^ 3) %*% p)

  # Kurtosis
  Aeq <- rbind(Aeq, t(x) ^ 4)
  beq <- rbind(beq, (t(simul) ^ 4) %*% p)


  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_marginal_distribution"
  )

}



# Views on Joint Distribution ---------------------------------------------

#' Views on Joint Distribution
#'
#' Helper to construct constraints on the entire distribution.
#'
#' \itemize{
#'   \item `simul` must have the same number of columns than `x`
#'   \item `p` should have the same number of rows that `simul`.
#' }
#'
#' @param x An univariate or a multivariate distribution.
#' @param simul An univariate or multivariate simulated panel.
#' @param p An object of the `ffp` class.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' set.seed(1)
#' library(ggplot2)
#'
#' # Invariants
#' ret <- diff(log(EuStockMarkets))
#' n <- nrow(ret)
#'
#' #' Prior probability distribution
#' prior <- rep(1 / n, n)
#'
#' # Simulated marginals
#' simul <- bootstrap_scenarios(ret, as_ffp(prior), as.double(n))
#'
#' views <- view_on_joint_distribution(x = ret, simul = simul, p = prior)
#' views
#'
#' ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
#' autoplot(ep)
#'
#' # location matches
#' colMeans(simul)
#' ffp_moments(x = ret, p = ep)$mu
#'
#' # dispersion matches
#' cov(simul)
#' ffp_moments(x = ret, p = ep)$sigma
view_on_joint_distribution <- function(x, simul, p) {
  UseMethod("view_on_joint_distribution", x)
}

#' @rdname view_on_joint_distribution
#' @export
view_on_joint_distribution.default <- function(x, simul, p) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_joint_distribution
#' @export
view_on_joint_distribution.matrix <- function(x, simul, p) {
  construct_view_on_joint_distribution(x = x, simul = check_input(simul), p = as_ffp(p))
}

#' @rdname view_on_joint_distribution
#' @export
view_on_joint_distribution.xts <- function(x, simul, p) {
  construct_view_on_joint_distribution(x = as.matrix(x), simul = check_input(simul), p = as_ffp(p))
}

#' @rdname view_on_joint_distribution
#' @export
view_on_joint_distribution.tbl_df <- function(x, simul, p) {
  construct_view_on_joint_distribution(x = tbl_to_mtx(x), simul = check_input(simul), p = as_ffp(p))
}

#' @keywords internal
construct_view_on_joint_distribution <- function(x, simul, p) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), NCOL(simul)))

  N <- NCOL(x)

  Aeq <- NULL
  beq <- NULL

  # order 1
  Aeq <- rbind(Aeq , t(x))
  beq <- rbind(beq, t(simul) %*% p)

  # order 2
  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq, t(x[ , k] * x[ , l]))
      beq <- rbind(beq, t(simul[ , k] * simul[ , l]) %*% p)
    }
  }

  # order 3
  for (k in 1:N) {
    for (l in k:N) {
      for (i in l:k) {
        Aeq <- rbind(Aeq, t(x[ , k] * x[ , l] * x[ , i]))
        beq <- rbind(beq, t(simul[ , k] * simul[ , l] * simul[ , i]) %*% p)
      }
    }
  }

  vctrs::new_list_of(
    x      = list(Aeq = Aeq, beq = beq),
    .ptype = double(),
    class  = "ffp_views",
    type   = "view_on_joint_distribution"
  )

}



# Bind Views --------------------------------------------------------------

#' Stack Different Views
#'
#' Bind views for entropy programming.
#'
#' @param ... Objects of the class \code{ffp_views} to combine.
#'
#' @return  A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Invariant
#' ret <- diff(log(EuStockMarkets))
#' n   <- nrow(ret)
#'
#' # Prior probabilities (usually equal weight scheme)
#' prior <- rep(1 / n, n)
#'
#' # Prior belief for expected returns (here is 0% for each asset)
#' view_mean <- view_on_mean(x = ret, mean = rep(0, 4))
#'
#' #' view on volatility
#' vol <- apply(ret, 2, stats::sd) * 1.1 # volatility 10% higher than average
#' view_volatility <- view_on_volatility(x = ret, vol = vol)
#'
#' views_comb <- bind_views(view_mean, view_volatility)
#' views_comb
#'
#' ep <- entropy_pooling(p      = prior,
#'                       Aeq    = views_comb$Aeq,
#'                       beq    = views_comb$beq,
#'                       A      = views_comb$A,
#'                       b      = views_comb$b,
#'                       solver = "nlminb")
#' autoplot(ep)
bind_views <- function(...) {

  dots <- rlang::list2(...)
  dots <- purrr::keep(dots, inherits, "ffp_views")

  .Aeq <- do.call(rbind, purrr::map(dots, "Aeq"))
  .beq <- do.call(rbind, purrr::map(purrr::map(dots, "beq"), ~ if (is.null(.x)) .x else as.matrix(.x)))
  .A   <- do.call(rbind, purrr::map(dots, "A"))
  .b   <- do.call(rbind, purrr::map(purrr::map(dots, "b"),   ~ if (is.null(.x)) .x else as.matrix(.x)))

  vctrs::new_list_of(
    x      = list(Aeq = .Aeq, beq = .beq, A = .A, b = .b),
    .ptype = double(),
    class  = "ffp_views",
    type   = "multiple_views")

}

# Printing methods --------------------------------------------------------

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.ffp_views <- function(x, ...) {
  cat(crayon::cyan("# ffp view"))
  #cat("\n")
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.ffp_views <- function(x, ...) {
  cat("\n")
  cat("Type: ", crayon::blurred(stringr::str_to_title(stringr::str_replace_all(attributes(x)$type, "_", " "))))
  cat("\n")
  nms <- names(x)
  purrr::walk2(.x = x, .y = nms, .f = ~ cat(.y, ": ", "Dim", NROW(.x), "x", NCOL(.x), "\n"))
  #cat("\n")
}
