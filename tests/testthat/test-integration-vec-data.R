# Integration tests for vctrs::vec_data() extraction pattern
# Core pattern: ffp results can be extracted to numeric when needed for base R functions

library(dplyr, warn.conflicts = FALSE)

# Test data
ret <- diff(log(EuStockMarkets))
set.seed(42)

# Type conversion and vec_data extraction -----

test_that("vec_data extraction preserves numeric precision", {
  p_ffp <- ffp(c(0.1, 0.2, 0.3, 0.4))
  p_numeric <- vctrs::vec_data(p_ffp)

  # Round-trip should preserve values
  p_back <- as_ffp(p_numeric)
  expect_equal(vctrs::vec_data(p_ffp), vctrs::vec_data(p_back), tolerance = 1e-15)
})

test_that("functions handle both ffp and extracted numeric interchangeably", {
  x <- as.matrix(ret[ , 1:2])

  # Via ffp
  p_ffp <- exp_decay(x, 0.001)
  stats_ffp <- ffp_moments(x, p_ffp)

  # Via extracted numeric
  p_numeric <- vctrs::vec_data(p_ffp)
  p_converted <- as_ffp(p_numeric)
  stats_converted <- ffp_moments(x, p_converted)

  expect_equal(
    stats_ffp$value,
    stats_converted$value,
    tolerance = 1e-10
  )
})

# Integration with ggplot2 -----------

test_that("autoplot.ffp() works with entropy_pooling result", {
  prior <- rep(1 / 100, 100)
  Aeq <- matrix(rnorm(100), ncol = 100)
  Aeq <- rbind(Aeq, rep(1, 100))
  beq <- c(1, 1)

  p <- entropy_pooling(prior, Aeq, beq, solver = "solnl")
  plot <- autoplot(p)

  expect_s3_class(plot, "ggplot")
})

test_that("autoplot.ffp() works with exp_decay result", {
  x <- as.matrix(ret[ , 1:2])
  p_ffp <- exp_decay(x, 0.001)

  plot <- autoplot(p_ffp)
  expect_s3_class(plot, "ggplot")
})

# Integration with empirical_stats ------

test_that("empirical_stats works with exp_decay result", {
  x <- as.matrix(ret[ , 1:2])
  p <- exp_decay(x, 0.001)

  stats <- empirical_stats(x, p)
  expect_s3_class(stats, "tbl_df")
  expect_true(nrow(stats) > 0)
})

test_that("empirical_stats works with entropy_pooling result", {
  prior <- rep(1 / nrow(ret), nrow(ret))
  Aeq <- matrix(rnorm(nrow(ret)), ncol = nrow(ret))
  Aeq <- rbind(Aeq, rep(1, nrow(ret)))
  beq <- c(1, 1)

  p <- entropy_pooling(prior, Aeq, beq, solver = "solnl")
  x <- as.matrix(ret[ , 1:2])
  stats <- empirical_stats(x, p)

  expect_s3_class(stats, "tbl_df")
})

# Combination functions --------

test_that("average_ffp works with entropy_pooling results", {
  prior <- rep(1 / 50, 50)
  Aeq <- matrix(rnorm(50), ncol = 50)
  Aeq <- rbind(Aeq, rep(1, 50))
  beq <- c(0.5, 1)

  p1 <- entropy_pooling(prior, Aeq, beq, solver = "solnl")
  beq[1] <- -0.5
  p2 <- entropy_pooling(prior, Aeq, beq, solver = "solnl")

  p_combined <- average_ffp(p1, p2)
  expect_s3_class(p_combined, "ffp")
  expect_equal(sum(vctrs::vec_data(p_combined)), 1, tolerance = 1e-7)
})

test_that("combine_ffp works with exp_decay results", {
  x <- as.matrix(ret[ , 1:2])
  p1_ffp <- exp_decay(x, 0.001)
  p2_ffp <- exp_decay(x, 0.002)

  p_combined <- combine_ffp(p1 = p1_ffp, p2 = p2_ffp, weights = c(0.6, 0.4))
  expect_s3_class(p_combined, "ffp")
})

# Probability validation ---------

test_that("entropy_pooling returns valid probabilities", {
  prior <- rep(1 / 100, 100)
  Aeq <- matrix(rnorm(100), ncol = 100)
  Aeq <- rbind(Aeq, rep(1, 100))
  beq <- c(1, 1)

  p <- entropy_pooling(prior, Aeq, beq, solver = "solnl")
  p_data <- vctrs::vec_data(p)

  expect_true(all(p_data >= 0))
  expect_true(all(p_data <= 1))
  expect_equal(sum(p_data), 1, tolerance = 1e-7)
})

test_that("exp_decay returns valid probabilities", {
  x <- as.matrix(ret[ , 1:3])
  p <- exp_decay(x, 0.001)
  p_data <- vctrs::vec_data(p)

  expect_true(all(p_data >= 0))
  expect_true(all(p_data <= 1))
  expect_equal(sum(p_data), 1, tolerance = 1e-10)
})

# Error handling -----------

test_that("mismatched lengths caught in combine_ffp", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.3, 0.2))

  # Should error because lengths don't match
  expect_error(combine_ffp(p1 = p1, p2 = p2, weights = c(0.5, 0.5)))
})
