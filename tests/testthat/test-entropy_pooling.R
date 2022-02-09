
# Optimization using ffp ------------------------------------------------------
set.seed(1)

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

test_that("ffp, optim and nlminb results converge", {
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

