# tests/testthat/test-ffp-combinations.R


# Test setup ---------------------------------------------------------------

p1 <- ffp(c(0.2, 0.3, 0.5))
p2 <- ffp(c(0.1, 0.4, 0.5))
p3 <- ffp(c(0.3, 0.3, 0.4))


# average_ffp() ------------------------------------------------------------

test_that("average_ffp() returns valid ffp for two inputs", {
  result <- average_ffp(p1, p2)

  expect_s3_class(result, "ffp")
  expect_true(is_ffp(result))
  expect_length(result, 3L)
  expect_type(vctrs::vec_data(result), "double")
})


test_that("average_ffp() computes correct equal-weight average", {
  result <- average_ffp(p1, p2)

  expected <- (
    vctrs::vec_data(p1) +
      vctrs::vec_data(p2)
  ) / 2

  expect_equal(
    vctrs::vec_data(result),
    expected,
    tolerance = 1e-10
  )
})


test_that("average_ffp() sums to 1", {
  result <- average_ffp(p1, p2)

  expect_equal(
    sum(vctrs::vec_data(result)),
    1,
    tolerance = 1e-10
  )
})


test_that("average_ffp() works with multiple inputs", {
  result <- average_ffp(p1, p2, p3)

  expected <- (
    vctrs::vec_data(p1) +
      vctrs::vec_data(p2) +
      vctrs::vec_data(p3)
  ) / 3

  expect_s3_class(result, "ffp")
  expect_length(result, 3L)

  expect_equal(
    vctrs::vec_data(result),
    expected,
    tolerance = 1e-10
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1,
    tolerance = 1e-10
  )
})


test_that("average_ffp() preserves vector properties", {
  result <- average_ffp(p1, p2)
  result_data <- vctrs::vec_data(result)

  expect_true(all(result_data >= 0))
  expect_true(all(result_data <= 1))
})


test_that("average_ffp() errors on empty input", {
  expect_error(
    average_ffp(),
    "At least one"
  )
})


test_that("average_ffp() errors on non-ffp objects", {
  expect_error(
    average_ffp(p1, c(0.2, 0.3, 0.5)),
    "All arguments must be"
  )

  expect_error(
    average_ffp(c(0.2, 0.3, 0.5), p2),
    "All arguments must be"
  )
})


test_that("average_ffp() errors on different length inputs", {
  p_short <- ffp(c(0.5, 0.5))

  expect_error(
    average_ffp(p1, p_short),
    "same length"
  )
})


test_that("average_ffp() handles single input", {
  result <- average_ffp(p1)

  expect_s3_class(result, "ffp")

  expect_equal(
    vctrs::vec_data(result),
    vctrs::vec_data(p1),
    tolerance = 1e-10
  )
})


# combine_ffp() ------------------------------------------------------------

test_that("combine_ffp() returns valid ffp with equal weights", {
  result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.5, 0.5)
  )

  expect_s3_class(result, "ffp")
  expect_true(is_ffp(result))
  expect_length(result, 3L)
  expect_type(vctrs::vec_data(result), "double")
})


test_that("combine_ffp() equals average_ffp() with equal weights", {
  avg_result <- average_ffp(p1, p2)

  comb_result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.5, 0.5)
  )

  expect_equal(
    vctrs::vec_data(avg_result),
    vctrs::vec_data(comb_result),
    tolerance = 1e-10
  )
})


test_that("combine_ffp() computes weighted average correctly", {
  result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.7, 0.3)
  )

  expected <- (
    0.7 * vctrs::vec_data(p1) +
      0.3 * vctrs::vec_data(p2)
  )

  expect_equal(
    vctrs::vec_data(result),
    expected,
    tolerance = 1e-10
  )
})


test_that("combine_ffp() sums to 1", {
  result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.7, 0.3)
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1,
    tolerance = 1e-10
  )
})


test_that("combine_ffp() works with three distributions", {
  result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    p3 = p3,
    weights = c(0.5, 0.3, 0.2)
  )

  expected <- (
    0.5 * vctrs::vec_data(p1) +
      0.3 * vctrs::vec_data(p2) +
      0.2 * vctrs::vec_data(p3)
  )

  expect_equal(
    vctrs::vec_data(result),
    expected,
    tolerance = 1e-10
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1,
    tolerance = 1e-10
  )
})


test_that("combine_ffp() preserves vector properties", {
  result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.6, 0.4)
  )

  result_data <- vctrs::vec_data(result)

  expect_true(all(result_data >= 0))
  expect_true(all(result_data <= 1))
})


test_that("combine_ffp() errors on empty input", {
  expect_error(
    combine_ffp(weights = numeric()),
    "At least one"
  )
})


test_that("combine_ffp() errors on weight mismatch", {
  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = 0.7
    ),
    "Number of weights must match"
  )

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(0.7, 0.2, 0.1)
    ),
    "Number of weights must match"
  )
})


test_that("combine_ffp() errors on non-numeric weights", {
  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c("0.5", "0.5")
    ),
    "must be a numeric vector"
  )
})


test_that("combine_ffp() errors on weights not summing to 1", {
  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(0.6, 0.3)
    ),
    "must sum to 1"
  )
})


test_that("combine_ffp() errors on negative weights", {
  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(-0.2, 1.2)
    ),
    "must be non-negative"
  )
})


test_that("combine_ffp() errors on different length inputs", {
  p_short <- ffp(c(0.5, 0.5))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p_short,
      weights = c(0.5, 0.5)
    ),
    "same length"
  )
})


test_that("combine_ffp() errors on non-ffp objects", {
  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = c(0.1, 0.4, 0.5),
      weights = c(0.5, 0.5)
    ),
    "All arguments must be"
  )
})


# Edge cases ---------------------------------------------------------------

test_that("average_ffp() handles extreme probabilities", {
  p_extreme1 <- ffp(c(1, 0, 0))
  p_extreme2 <- ffp(c(0, 1, 0))

  result <- average_ffp(
    p_extreme1,
    p_extreme2
  )

  expect_equal(
    vctrs::vec_data(result),
    c(0.5, 0.5, 0),
    tolerance = 1e-10
  )
})


test_that("combine_ffp() handles extreme weights", {
  result_p1 <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(1, 0)
  )

  result_p2 <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0, 1)
  )

  expect_equal(
    vctrs::vec_data(result_p1),
    vctrs::vec_data(p1),
    tolerance = 1e-10
  )

  expect_equal(
    vctrs::vec_data(result_p2),
    vctrs::vec_data(p2),
    tolerance = 1e-10
  )
})


test_that("average_ffp() works with many distributions", {
  probabilities <- rep(
    list(p1, p2, p3),
    times = 3
  )

  result <- do.call(
    average_ffp,
    probabilities
  )

  expect_s3_class(result, "ffp")

  expect_equal(
    sum(vctrs::vec_data(result)),
    1,
    tolerance = 1e-10
  )
})


test_that("combine_ffp() is invariant to coordinated input reordering", {
  result1 <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.7, 0.3)
  )

  result2 <- combine_ffp(
    p1 = p2,
    p2 = p1,
    weights = c(0.3, 0.7)
  )

  expect_equal(
    vctrs::vec_data(result1),
    vctrs::vec_data(result2),
    tolerance = 1e-10
  )
})


test_that("averaging uniform probabilities yields uniform probabilities", {
  p_uniform1 <- ffp(rep(1 / 3, 3))
  p_uniform2 <- ffp(rep(1 / 3, 3))

  result <- average_ffp(
    p_uniform1,
    p_uniform2
  )

  expect_equal(
    vctrs::vec_data(result),
    rep(1 / 3, 3),
    tolerance = 1e-10
  )
})
