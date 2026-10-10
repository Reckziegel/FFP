ret_ts  <- diff(log(EuStockMarkets))
ret_mtx <- matrix(ret_ts, ncol = 4)
ret_xts <- xts::xts(ret_mtx, order.by = Sys.Date() + 1:nrow(ret_mtx))
ret_tbl <- tibble::as_tibble(ret_ts)

p <- rep(1 / nrow(ret_ts), nrow(ret_ts))
simul_marg <- bootstrap_scenarios(ret_mtx, as_ffp(p), as.double(NROW(p)))
simul_cop <- apply(simul_marg, 2, stats::pnorm) # assuming normal copula

vmean_ts <- view_on_copula(x = ret_ts, simul = simul_cop, p = p)
test_that("`view_on_copula` works for ts", {
  expect_type(vmean_ts, "list")
  expect_s3_class(vmean_ts, "ffp_views")
  expect_length(vmean_ts, 2L)
  expect_named(vmean_ts, c("Aeq", "beq"))
})

vmean_mtx <- view_on_copula(x = ret_mtx, simul = simul_cop, p = p)
test_that("`view_on_copula` works for matrix", {
  expect_type(vmean_mtx, "list")
  expect_s3_class(vmean_mtx, "ffp_views")
  expect_length(vmean_mtx, 2L)
  expect_named(vmean_mtx, c("Aeq", "beq"))
})

vmean_xts <- view_on_copula(x = ret_xts, simul = simul_cop, p = p)
test_that("`view_on_copula` works for xts", {
  expect_type(vmean_xts, "list")
  expect_s3_class(vmean_xts, "ffp_views")
  expect_length(vmean_xts, 2L)
  expect_named(vmean_xts, c("Aeq", "beq"))
})

vmean_tbl <- view_on_copula(x = ret_tbl, simul = simul_cop, p = p)
test_that("`view_on_copula` works for tbl_df", {
  expect_type(vmean_tbl, "list")
  expect_s3_class(vmean_tbl, "ffp_views")
  expect_length(vmean_tbl, 2L)
  expect_named(vmean_tbl, c("Aeq", "beq"))
})

test_that("view_on_copula preserves legacy moment constraints", {
  x <- matrix(
    c(
      0.10, 0.20, 0.30,
      0.25, 0.40, 0.50,
      0.45, 0.60, 0.70,
      0.70, 0.80, 0.85,
      0.90, 0.95, 0.98
    ),
    ncol = 3,
    byrow = TRUE
  )

  simul <- matrix(
    c(
      0.15, 0.10, 0.25,
      0.30, 0.35, 0.45,
      0.50, 0.55, 0.65,
      0.75, 0.70, 0.80,
      0.95, 0.90, 0.97
    ),
    ncol = 3,
    byrow = TRUE
  )

  probabilities <- c(
    0.10,
    0.15,
    0.20,
    0.25,
    0.30
  )

  result <- view_on_copula(
    x = x,
    simul = simul,
    p = probabilities
  )

  pairs <- matrix_constraint_pairs(3)

  second_a <- second_moment_constraint_matrix(
    feature_values = x,
    pairs = pairs
  )

  second_simul <- second_moment_constraint_matrix(
    feature_values = simul,
    pairs = pairs
  )

  triples <- matrix(
    c(
      1, 1, 1,
      1, 2, 2,
      1, 2, 1,
      1, 3, 3,
      1, 3, 2,
      1, 3, 1,
      2, 2, 2,
      2, 3, 3,
      2, 3, 2,
      3, 3, 3
    ),
    ncol = 3,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c(
        "first",
        "second",
        "third"
      )
    )
  )

  third_a <- third_moment_constraint_matrix(
    feature_values = x,
    indices = triples
  )

  third_simul <- third_moment_constraint_matrix(
    feature_values = simul,
    indices = triples
  )

  expected_a_eq <- rbind(
    t(x),
    second_a,
    third_a
  )

  expected_b_eq <- c(
    rep(0.5, 3),
    drop(
      second_simul %*%
        probabilities
    ),
    drop(
      third_simul %*%
        probabilities
    )
  )

  expect_equal(
    result$Aeq,
    expected_a_eq
  )

  expect_equal(
    as.double(result$beq),
    expected_b_eq
  )
})

test_that("view_on_copula preserves legacy two-dimensional behavior", {
  x <- matrix(
    c(
      0.10, 0.20,
      0.30, 0.40,
      0.50, 0.60,
      0.70, 0.80
    ),
    ncol = 2,
    byrow = TRUE
  )

  probabilities <- rep(
    0.25,
    4
  )

  result <- view_on_copula(
    x = x,
    simul = x,
    p = probabilities
  )

  # 2 first moments + 3 second moments.
  # The legacy implementation does not add third-order
  # moments when only two features are present.
  expect_equal(
    nrow(result$Aeq),
    5L
  )
})

test_that("view_on_copula requires compatible feature dimensions", {
  x <- matrix(
    seq_len(10),
    ncol = 2
  )

  simul <- matrix(
    seq_len(15),
    ncol = 3
  )

  probabilities <- rep(
    1 / 5,
    5
  )

  expect_error(
    view_on_copula(
      x = x,
      simul = simul,
      p = probabilities
    )
  )
})
