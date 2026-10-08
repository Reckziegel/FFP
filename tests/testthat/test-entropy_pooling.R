
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
  expect_true(all(dplyr::near(
    vctrs::vec_data(opt_nlminb),
    vctrs::vec_data(opt_solnl),
    tol = 0.00001))
    )
  expect_true(all(dplyr::near(
    vctrs::vec_data(opt_solnl),
    vctrs::vec_data(opt_nloptr),
    tol = 0.00001))
    )
  expect_true(all(dplyr::near(
    vctrs::vec_data(opt_nlminb),
    vctrs::vec_data(opt_nloptr),
    tol = 0.00001))
    )
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
  expect_true(all(dplyr::near(
    vctrs::vec_data(opt_single_view_solnl),
    vctrs::vec_data(opt_single_view_nloptr),
    tol = 0.0001))
    )
  expect_true(all(dplyr::near(
    vctrs::vec_data(opt_multiple_view_solnl),
    vctrs::vec_data(opt_multiple_view_nloptr),
    tol = 0.0001))
    )
})

test_that("legacy inputs build a canonical entropy problem", {
  prior <- c(
    0.10,
    0.20,
    0.30,
    0.40
  )

  Aeq <- matrix(
    c(
      -1,
      0,
      1,
      2
    ),
    nrow = 1
  )

  beq <- 0.75

  A <- matrix(
    c(
      1,
      1,
      0,
      0
    ),
    nrow = 1
  )

  b <- 0.60

  problem <- build_legacy_entropy_problem(
    p = prior,
    A = A,
    b = b,
    Aeq = Aeq,
    beq = beq
  )

  expect_s3_class(
    problem,
    "ffp_entropy_problem"
  )

  expect_identical(
    problem$prior,
    prior
  )

  expect_equal(
    problem$a_eq[1, ],
    Aeq[1, ]
  )

  expect_identical(
    problem$b_eq[[1]],
    beq
  )

  expect_equal(
    problem$a_ineq,
    A
  )

  expect_identical(
    problem$b_ineq,
    b
  )

  expect_equal(
    problem$a_eq[nrow(problem$a_eq), ],
    rep(1, length(prior))
  )

  expect_identical(
    problem$b_eq[[nrow(problem$a_eq)]],
    1
  )
})


test_that("entropy_pooling uses nlminb by default", {
  prior <- rep(
    1 / 5,
    5
  )

  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  expected <- entropy_pooling(
    p = prior,
    Aeq = x,
    beq = 0.5,
    solver = "nlminb"
  )

  result <- entropy_pooling(
    p = prior,
    Aeq = x,
    beq = 0.5
  )

  expect_equal(
    as.double(result),
    as.double(expected),
    tolerance = 1e-10
  )
})


test_that("entropy_pooling returns the prior without active views", {
  prior <- c(
    0.10,
    0.20,
    0.30,
    0.40
  )

  result <- entropy_pooling(
    p = prior
  )

  expect_equal(
    as.double(result),
    prior,
    tolerance = 1e-8
  )
})


test_that("legacy entropy inputs validate constraint dimensions", {
  prior <- rep(
    0.25,
    4
  )

  expect_error(
    build_legacy_entropy_problem(
      p = prior,
      Aeq = matrix(
        1,
        nrow = 1,
        ncol = 3
      ),
      beq = 1
    ),
    class = "ffp_error_invalid_entropy_problem"
  )

  expect_error(
    build_legacy_entropy_problem(
      p = prior,
      Aeq = matrix(
        1,
        nrow = 1,
        ncol = 4
      ),
      beq = c(
        1,
        2
      )
    ),
    class = "ffp_error_invalid_entropy_problem"
  )
})


test_that("legacy entropy metadata supports equality-only problems", {
  problem <- build_legacy_entropy_problem(
    p = rep(0.25, 4),
    Aeq = matrix(
      c(-1, 0, 1, 2),
      nrow = 1
    ),
    beq = 0.5
  )

  expect_identical(
    nrow(problem$a_ineq),
    0L
  )

  expect_identical(
    problem$metadata$constraints$constraint_type,
    "equality"
  )
})


test_that("legacy entropy metadata supports inequality-only problems", {
  problem <- build_legacy_entropy_problem(
    p = rep(0.25, 4),
    A = matrix(
      c(1, 1, 0, 0),
      nrow = 1
    ),
    b = 0.60
  )

  expect_identical(
    nrow(problem$a_eq),
    1L
  )

  expect_identical(
    problem$metadata$constraints$constraint_type,
    "inequality"
  )
})


# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

test_that("core solver matches legacy equality-only entropy pooling", {
  prior <- rep(
    1 / 5,
    5
  )

  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  Aeq <- matrix(
    x,
    nrow = 1
  )

  beq <- 0.50

  problem <- build_legacy_entropy_problem(
    p = prior,
    Aeq = Aeq,
    beq = beq
  )

  core <- solve_entropy_problem(
    problem
  )$posterior

  legacy_nlminb <- entropy_pooling(
    p = prior,
    Aeq = Aeq,
    beq = beq,
    solver = "nlminb"
  )

  legacy_solnl <- entropy_pooling(
    p = prior,
    Aeq = Aeq,
    beq = beq,
    solver = "solnl"
  )

  legacy_nloptr <- entropy_pooling(
    p = prior,
    Aeq = Aeq,
    beq = beq,
    solver = "nloptr"
  )

  expect_equal(
    as.double(legacy_nlminb),
    core,
    tolerance = 1e-6
  )

  expect_equal(
    as.double(legacy_solnl),
    core,
    tolerance = 1e-6
  )

  expect_equal(
    as.double(legacy_nloptr),
    core,
    tolerance = 1e-6
  )
})

test_that("core solver matches legacy inequality entropy pooling", {
  prior <- rep(
    0.25,
    4
  )

  A <- matrix(
    c(
      1,
      1,
      0,
      0
    ),
    nrow = 1
  )

  b <- 0.25

  problem <- build_legacy_entropy_problem(
    p = prior,
    A = A,
    b = b
  )

  core <- solve_entropy_problem(
    problem
  )$posterior

  legacy_solnl <- entropy_pooling(
    p = prior,
    A = A,
    b = b,
    solver = "solnl"
  )

  legacy_nloptr <- entropy_pooling(
    p = prior,
    A = A,
    b = b,
    solver = "nloptr"
  )

  expected <- c(
    0.125,
    0.125,
    0.375,
    0.375
  )

  expect_equal(
    core,
    expected,
    tolerance = 1e-6
  )

  expect_equal(
    as.double(legacy_solnl),
    core,
    tolerance = 1e-6
  )

  expect_equal(
    as.double(legacy_nloptr),
    core,
    tolerance = 1e-6
  )
})

test_that("core solver matches legacy mixed entropy pooling", {
  prior <- rep(
    1 / 6,
    6
  )

  x <- c(
    -2,
    -1,
    0,
    1,
    2,
    3
  )

  Aeq <- matrix(
    x,
    nrow = 1
  )

  beq <- 0.75

  A <- matrix(
    c(
      1,
      1,
      0,
      0,
      0,
      0
    ),
    nrow = 1
  )

  b <- 0.20

  problem <- build_legacy_entropy_problem(
    p = prior,
    A = A,
    b = b,
    Aeq = Aeq,
    beq = beq
  )

  core <- solve_entropy_problem(
    problem
  )$posterior

  legacy_solnl <- entropy_pooling(
    p = prior,
    A = A,
    b = b,
    Aeq = Aeq,
    beq = beq,
    solver = "solnl"
  )

  legacy_nloptr <- entropy_pooling(
    p = prior,
    A = A,
    b = b,
    Aeq = Aeq,
    beq = beq,
    solver = "nloptr"
  )

  control <- entropy_solver_control()

  expect_equal(
    sum(core),
    1,
    tolerance = control$constraint_tolerance
  )

  expect_lte(
    abs(
      sum(core * x) - beq
    ),
    control$constraint_tolerance
  )

  expect_lte(
    sum(core[1:2]) - b,
    control$constraint_tolerance
  )

  expect_equal(
    as.double(legacy_solnl),
    core,
    tolerance = 1e-5
  )

  expect_equal(
    as.double(legacy_nloptr),
    core,
    tolerance = 1e-5
  )
})

test_that("core solver preserves zero prior support in legacy problems", {
  prior <- c(
    0,
    0.25,
    0.25,
    0.25,
    0.25
  )

  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  problem <- build_legacy_entropy_problem(
    p = prior,
    Aeq = matrix(
      x,
      nrow = 1
    ),
    beq = 0.50
  )

  core <- solve_entropy_problem(
    problem
  )$posterior

  expect_identical(
    core[[1]],
    0
  )

  expect_equal(
    sum(core),
    1,
    tolerance = 1e-8
  )

  expect_equal(
    sum(core * x),
    0.50,
    tolerance = 1e-8
  )
})

test_that("legacy solver identifiers use the unified entropy solver", {
  prior <- rep(
    1 / 5,
    5
  )

  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  Aeq <- matrix(
    x,
    nrow = 1
  )

  beq <- 0.50

  nlminb_result <- entropy_pooling(
    p = prior,
    Aeq = Aeq,
    beq = beq,
    solver = "nlminb"
  )

  solnl_result <- entropy_pooling(
    p = prior,
    Aeq = Aeq,
    beq = beq,
    solver = "solnl"
  )

  nloptr_result <- entropy_pooling(
    p = prior,
    Aeq = Aeq,
    beq = beq,
    solver = "nloptr"
  )

  expect_identical(
    as.double(nlminb_result),
    as.double(solnl_result)
  )

  expect_identical(
    as.double(solnl_result),
    as.double(nloptr_result)
  )
})

test_that("entropy_pooling preserves structural zeros in the prior", {
  prior <- c(
    0,
    0.25,
    0.25,
    0.25,
    0.25
  )

  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  posterior <- entropy_pooling(
    p = prior,
    Aeq = matrix(
      x,
      nrow = 1
    ),
    beq = 0.50
  )

  posterior_values <- as.double(
    posterior
  )

  expect_identical(
    posterior_values[[1]],
    0
  )

  expect_equal(
    sum(posterior_values),
    1,
    tolerance = 1e-8
  )

  expect_equal(
    sum(posterior_values * x),
    0.50,
    tolerance = 1e-8
  )
})
