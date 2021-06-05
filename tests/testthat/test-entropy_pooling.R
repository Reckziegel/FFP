set.seed(123)

# Optimization using ffp ------------------------------------------------------

# Prior Probabilities
p <- rep(1/100, 100)
# An arbitrary View
Aeq_p <- matrix(rnorm(100), ncol = 100)
# Constrain probabilities to sum 1
Aeq_c <- matrix(rep(1, 100), ncol = 100)
Aeq <- rbind(Aeq_p, Aeq_c)
# right-hand side
beq <- as.matrix(c(1, 1))

opt_ffp <- entropy_pooling(p, Aeq = Aeq, beq = beq)


# Optimization using optim ----------------------------------------------------

objective <- function(v, p, Aeq, beq){
  x <- exp(log(p) - 1 - t(Aeq) %*% v)
  x <- apply(cbind(x, 10 ^ -32), 1, max)
  L <- t(x) %*% (log(x) - log(p) + t(Aeq) %*% v) - t(beq) %*% v
  -L
}
gradient <- function(v, p, Aeq, beq){
  x <- exp(log(p) - 1 - t(Aeq) %*% v)
  beq - Aeq %*% x
}

entropy_optim <- function(p, Aeq, beq) {
  vopt <- stats::optim(
    par = rep(0, NROW(Aeq)),
    fn  = objective,
    gr  = gradient,
    Aeq = Aeq,
    beq = beq,
    p = p,
    method = "L-BFGS-B"
  )
  v <- vopt$par
  p <- exp(log(p) - 1 - t(Aeq) %*% v)
  as.double(p / sum(p))
}

opt_optim <- entropy_optim(p = p, Aeq = Aeq, beq = beq)


test_that("ffp and optim results converge", {
  expect_type(opt_ffp, "double")
  expect_type(opt_optim, "double")
  expect_length(opt_ffp, vctrs::vec_size(p))
  expect_length(opt_optim, vctrs::vec_size(p))
  expect_true(all(dplyr::near(opt_ffp, opt_optim, tol = 0.00001)))
})
