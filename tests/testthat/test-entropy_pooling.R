
# Optimization using ffp ------------------------------------------------------
set.seed(1)

# Equality Constraints

# Prior Probabilities
p <- rep(1/100, 100)
# An arbitrary View
Aeq_p <- matrix(rnorm(100), ncol = 100)
# Constrain probabilities to sum 1
Aeq_c <- matrix(rep(1, 100), ncol = 100)
Aeq <- rbind(Aeq_p, Aeq_c)
# right-hand side
beq <- as.matrix(c(1, 1))

opt_nlminb <- entropy_pooling(p = p, Aeq = Aeq, beq = beq, solver = "nlminb")
opt_solnl  <- entropy_pooling(p = p, Aeq = Aeq, beq = beq, solver = "solnl")
opt_nloptr <- entropy_pooling(p = p, Aeq = Aeq, beq = beq, solver = "nloptr")

# Test --------------------------------------------------------------------

test_that("nlminb, solnl and nloptr results converge for equality constrains", {
  expect_type(opt_nlminb, "double")
  expect_type(opt_solnl, "double")
  expect_type(opt_nloptr, "double")
  expect_length(opt_nlminb, vctrs::vec_size(p))
  expect_length(opt_solnl, vctrs::vec_size(p))
  expect_length(opt_nloptr, vctrs::vec_size(p))
  expect_true(all(dplyr::near(opt_nlminb, opt_solnl, tol = 0.00001)))
  expect_true(all(dplyr::near(opt_solnl, opt_nloptr, tol = 0.00001)))
  expect_true(all(dplyr::near(opt_nlminb, opt_nloptr, tol = 0.00001)))
})

# Inequality Constraints

ret <- matrix(diff(log(EuStockMarkets)), ncol = 4)
prior <- rep(1 / nrow(ret), nrow(ret))
views_single   <- view_on_rank(x = ret, rank = c(1, 2))
views_multiple <- view_on_rank(x = ret, rank = c(1, 2, 3, 4))

opt_single_view_solnl  <- entropy_pooling(p = prior, A = views_single$A, views_single$b, solver = "solnl")
opt_single_view_nloptr <- entropy_pooling(p = prior, A = views_single$A, views_single$b, solver = "nloptr")

opt_multiple_view_solnl  <- entropy_pooling(p = prior, A = views_single$A, views_single$b, solver = "solnl")
opt_multiple_view_nloptr <- entropy_pooling(p = prior, A = views_single$A, views_single$b, solver = "nloptr")

test_that("solnl and nloptr results converge for inequality constrains", {
  expect_type(opt_single_view_solnl, "double")
  expect_type(opt_single_view_nloptr, "double")
  expect_type(opt_multiple_view_solnl, "double")
  expect_type(opt_multiple_view_nloptr, "double")
  expect_length(opt_single_view_solnl, vctrs::vec_size(prior))
  expect_length(opt_single_view_nloptr, vctrs::vec_size(prior))
  expect_length(opt_multiple_view_solnl, vctrs::vec_size(prior))
  expect_length(opt_multiple_view_nloptr, vctrs::vec_size(prior))
  expect_true(all(dplyr::near(opt_single_view_solnl, opt_single_view_nloptr, tol = 0.0001)))
  expect_true(all(dplyr::near(opt_multiple_view_solnl, opt_multiple_view_nloptr, tol = 0.0001)))
})

