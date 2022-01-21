
# PARTIAL INFORMATION -----------------------------------------------------

# Views on Means ----------------------------------------------------------

#' Views on the Location
#'
#' Helper to construct constraints on the means for entropy programming.
#'
#' @param x An univariate or a multivariate dataset.
#' @param mean A \code{double} for the target location parameter of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
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
view_on_mean.numeric <- function(x, mean) {
  construct_view_on_mean(x, mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.matrix <- function(x, mean) {
  construct_view_on_mean(x, mean)
}

#' @rdname view_on_mean
#' @export
view_on_mean.ts <- function(x, mean) {
  construct_view_on_mean(as.matrix(x), mean)
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

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}


# Views on Covariance Matrix ---------------------------------------------------

#' Views on the Covariance Matrix
#'
#' Helper to construct constraints on variance-covariance matrix for
#' entropy programming.
#'
#' @param x An univariate or a multivariate dataset.
#' @param mean A \code{double} for the location parameter of the series in `x`.
#' @param sigma A \code{matrix} for the target variance-covariance parameter
#' of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
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
view_on_covariance.numeric <- function(x, mean, sigma) {
  construct_view_on_covariance(x, mean, sigma)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.matrix <- function(x, mean, sigma) {
  construct_view_on_covariance(x, mean, sigma)
}

#' @rdname view_on_covariance
#' @export
view_on_covariance.ts <- function(x, mean, sigma) {
  construct_view_on_covariance(as.matrix(x), mean, sigma)
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

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}


# Views on Correlation ----------------------------------------------------

#' Views on the Correlation Matrix
#'
#' Helper to construct constraints on the correlation matrix for
#' entropy programming.
#'
#' @param x An univariate or a multivariate dataset.
#' @param cor A \code{matrix} for the target correlation structure of
#' the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
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
view_on_correlation.numeric <- function(x, cor) {
  construct_view_on_correlation(x, cor)
}

#' @rdname view_on_correlation
#' @export
view_on_correlation.matrix <- function(x, cor) {
  construct_view_on_correlation(x, cor)
}

#' @rdname view_on_correlation
#' @export
view_on_correlation.ts <- function(x, cor) {
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

  mu <- colMeans(x)
  sd <- apply(x, 2, stats::sd)

  Aeq <- NULL
  beq <- NULL

  # Attach the view on cor
  for (k in 1:ncol(cor)) {
    for (l in k:ncol(cor)) {
      Aeq <- rbind(Aeq ,t(x[ , k] * x[ , l]))
      beq <- rbind(beq, mu[[k]] * mu[[l]] * sd[[k]] * sd[[l]] * cor[k, l])
    }
  }

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}


# Views on the Volatility -------------------------------------------------

#' Views on the Volatility
#'
#' Helper to construct constraints on volatility for entropy programming.
#'
#' @param x An univariate or a multivariate dataset.
#' @param vol A \code{double} for the target volatility structure
#' of the series in `x`.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
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
view_on_volatility.numeric <- function(x, vol) {
  construct_view_on_volatility(x, vol)
}

#' @rdname view_on_volatility
#' @export
view_on_volatility.matrix <- function(x, vol) {
  construct_view_on_volatility(x, vol)
}

#' @rdname view_on_volatility
#' @export
view_on_volatility.ts <- function(x, vol) {
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
  beq <- NULL

  Aeq <- rbind(Aeq, t(x ^ 2))
  beq <- rbind(beq, colMeans(x) ^ 2 + vol ^ 2)

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}


# Views on Ranking --------------------------------------------------------

#' Views on Relative Performance
#'
#' Helper to construct constraints on ranking assets for entropy programming.
#'
#' @param x An univariate ou multivariate dataset.
#' @param p An object of the `ffp` class.
#' @param rank A \code{double} with the asset indexes.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' colnames(x) <- colnames(EuStockMarkets)
#' prior <- as.matrix(rep(1/nrow(x), nrow(x)))
#' # asset in the first col will outperform the asset in the second col.
#' view_on_rank(x = x, p = prior, rank = c(2, 1))
view_on_rank <- function(x, p, rank) {
  UseMethod("view_on_rank", x)
}

#' @rdname view_on_rank
#' @export
view_on_rank.default <- function(x, p, rank) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_rank
#' @export
view_on_rank.matrix <- function(x, p, rank) {
  construct_view_on_rank(x = x, p = p, rank = rank)
}

#' @rdname view_on_rank
#' @export
view_on_rank.ts <- function(x, p, rank) {
  construct_view_on_rank(x = as.matrix(x), p = p, rank = rank)
}

#' @rdname view_on_rank
#' @export
view_on_rank.xts <- function(x, p, rank) {
  construct_view_on_rank(x = as.matrix(x), p = p, rank = rank)
}

#' @rdname view_on_rank
#' @export
view_on_rank.tbl_df <- function(x, p, rank) {
  construct_view_on_rank(x = tbl_to_mtx(x), p = p, rank = rank)
}

#' @keywords internal
construct_view_on_rank <- function(x, p, rank) {

  assertthat::assert_that(assertthat::are_equal(NROW(x), NROW(p)))
  assertthat::assert_that(is.numeric(rank), msg = "`.rank` must be a numeric vector.")

  rank_size <- vctrs::vec_size(rank)

  Aeq <- NULL
  beq <- NULL

  # ...constrain the expectations... A*x <= 0
  view <- x[ , rank[1:(rank_size - 1)]] - x[ , rank[2:rank_size]]
  # Jx1 vector. Expectation is assigned to each scenario

  # The expectation is that (Lower - Upper)x <= 0.
  # (i.e. The returns of upper are greater than zero for each scenario)
  A <- t(view)
  b <- matrix(rep(0, nrow(A)), ncol = 1)

  vctrs::new_list_of(list(A = A, b = b), .ptype = double())

}

# Views on tail codependence ----------------------------------------------

#' Views on Tail Dependence
#'
#' Helper to construct constraints on tail dependence for entropy programming.
#'
#' @param u A multivariate copula.
#' @param tail A \code{double} with tail index of each asset.
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
view_on_tail_dependence <- function(u, tail) {
  UseMethod("view_on_tail_dependence", u)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.default <- function(u, tail) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.matrix <- function(u, tail) {
  construct_view_on_tail_dependence(u = u, tail = tail)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.xts <- function(u, tail) {
  construct_view_on_tail_dependence(u = as.matrix(u), tail = tail)
}

#' @rdname view_on_tail_dependence
#' @export
view_on_tail_dependence.tbl_df <- function(u, tail) {
  construct_view_on_tail_dependence(u = tbl_to_mtx(u), tail = tail)
}

#' @keywords internal
construct_view_on_tail_dependence <- function(u, tail) {

  assertthat::assert_that(assertthat::are_equal(NROW(u), NROW(tail)))
  vctrs::vec_assert(tail, double())

  Aeq <- NULL
  beq <- NULL

  Aeq <- t(u)
  beq <- tail

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}

# FULL INFORMATION --------------------------------------------------------

# Views on copula ---------------------------------------------------------

# TODO

# Views on Marginal Distributions -----------------------------------------

#' Views on the Marginal Distribution
#'
#' Helper to construct constraints on the entire marginal distribution for
#' entropy programming.
#'
#' \itemize{
#'   \item `simul` must have the same number of columns than `x`
#'   \item `p` should have the same number of rows that `simul`.
#' }
#'
#' @param x An univariate ou multivariate dataset.
#' @param simul An univariate ou multivariate dataset.
#' @param p An object of the `ffp` class.
#' @param on_mean A \code{flag}. Should the constraints be added on the mean?
#' @param on_sigma A \code{flag}. Should the constraints be added on the covariance
#' matrix?
#'
#' @return A \code{list} of the `view` class.
#' @export
#'
#' @examples
#' #
view_on_marginal_distribution <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  UseMethod("view_on_marginal_distribution", x)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.default <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  stop("Method not implemented for class `", class(x), "` yet.", call. = FALSE)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.numeric <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  construct_view_on_marginal_distribution(x = x, simul = simul, p = p, on_mean = on_mean, on_sigma = on_sigma)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.matrix <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  construct_view_on_marginal_distribution(x = x, simul = simul, p = p, on_mean = on_mean, on_sigma = on_sigma)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.ts <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  construct_view_on_marginal_distribution(x = x, simul = simul, p = p, on_mean = on_mean, on_sigma = on_sigma)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.xts <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  construct_view_on_marginal_distribution(x = as.matrix(x), simul = simul, p = p, on_mean = on_mean, on_sigma = on_sigma)
}

#' @rdname view_on_marginal_distribution
#' @export
view_on_marginal_distribution.tbl_df <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {
  construct_view_on_marginal_distribution(x = tbl_to_mtx(x), simul = simul, p = p, on_mean = on_mean, on_sigma = on_sigma)
}

#' @keywords internal
construct_view_on_marginal_distribution <- function(x, simul, p, on_mean = TRUE, on_sigma = TRUE) {

  assertthat::assert_that(assertthat::are_equal(NCOL(x), NCOL(simul)))

  N <- NCOL(x)

  Aeq <- NULL
  beq <- NULL

  # ...constrain the first moments...
  if (on_mean) {
    Aeq <- rbind(Aeq, t(x))
    beq <- rbind(beq, t(simul) %*% p)
  }
  if (on_sigma) {
    Aeq <- rbind(Aeq, t(simul) ^ 2)
    beq <- rbind(beq, (t(simul) ^ 2) %*% p)
  }
  # if (on_skew) {
  #   Aeq <- rbind(Aeq, t(simul) ^ 3)
  #   beq <- rbind(beq, (t(simul) ^ 3) %*% p)
  # }

  vctrs::new_list_of(list(Aeq = Aeq, beq = beq), .ptype = double())

}



# Views on Joint Distribution ---------------------------------------------

construct_view_on_joint_distribution <- function() {

  Aeq <- NULL
  beq <- NULL

  Aeq <- rbind(Aeq , t(X))
  beq <- rbind(beq, m)

  SecMom <- S + m %*% t(m)

  for (k in 1:N) {
    for (l in k:N) {
      Aeq <- rbind(Aeq , t(X[ , k] * X[ , l]))
      beq <- rbind(beq, SecMom[k, l])
    }
  }
}

