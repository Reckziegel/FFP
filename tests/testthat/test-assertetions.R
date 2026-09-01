# tests/testthat/test-assertetions.R


# Test setup ---------------------------------------------------------------

p1 <- ffp(c(0.2, 0.3, 0.5))
p2 <- ffp(c(0.1, 0.4, 0.5))


# validate_probabilities() ------------------------------------------------

test_that("validate_probabilities() accepts valid probabilities", {
  p <- c(0.2, 0.3, 0.5)

  expect_no_error(
    validate_probabilities(p, tolerance = 0.001)
  )
})


test_that("validate_probabilities() rejects negative values", {
  p <- c(-0.1, 0.6, 0.5)

  expect_error(
    validate_probabilities(p, tolerance = 0.001),
    "must be non-negative"
  )
})


test_that("validate_probabilities() rejects probabilities not summing to 1", {
  p <- c(0.2, 0.3, 0.4)

  expect_error(
    validate_probabilities(p, tolerance = 0.001),
    "must sum to 1"
  )
})


test_that("validate_probabilities() respects tolerance", {
  p <- c(0.5, 0.5005)

  expect_no_error(
    validate_probabilities(p, tolerance = 0.001)
  )

  expect_error(
    validate_probabilities(p, tolerance = 1e-8),
    "must sum to 1"
  )
})


test_that("validate_probabilities() accepts a unit probability", {
  expect_no_error(
    validate_probabilities(1, tolerance = 0.001)
  )
})


test_that("validate_probabilities() rejects invalid scalar probability", {
  expect_error(
    validate_probabilities(0.5, tolerance = 0.001),
    "must sum to 1"
  )
})


# assert_is_equal_size() --------------------------------------------------

test_that("assert_is_equal_size() accepts equal-length ffp vectors", {
  expect_no_error(
    assert_is_equal_size(p1, p2)
  )
})


test_that("assert_is_equal_size() rejects different lengths", {
  p_short <- ffp(c(0.5, 0.5))

  expect_error(
    assert_is_equal_size(p1, p_short)
  )
})


test_that("assert_is_equal_size() works with numeric vectors", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)

  expect_no_error(
    assert_is_equal_size(x, y)
  )
})


# assert_is_probability() -------------------------------------------------

test_that("assert_is_probability() accepts valid numeric probabilities", {
  p <- c(0.2, 0.3, 0.5)

  expect_no_error(
    assert_is_probability(p)
  )
})


test_that("assert_is_probability() accepts valid integer probabilities", {
  p <- c(0L, 0L, 1L)

  expect_no_error(
    assert_is_probability(p)
  )
})


test_that("assert_is_probability() accepts ffp objects", {
  expect_no_error(
    assert_is_probability(p1)
  )
})


test_that("assert_is_probability() rejects unsupported types", {
  expect_error(
    assert_is_probability("not a probability"),
    "must be numeric or an object of class"
  )
})


test_that("assert_is_probability() rejects invalid numeric probabilities", {
  x <- c(0.2, 0.3, 0.4)

  expect_error(
    assert_is_probability(x),
    "must sum to 1"
  )
})


test_that("assert_is_probability() rejects negative probabilities", {
  x <- c(-0.1, 0.4, 0.7)

  expect_error(
    assert_is_probability(x),
    "must be non-negative"
  )
})


# Integration with ffp constructors ---------------------------------------

test_that("ffp() uses probability validation", {
  p_valid <- c(0.2, 0.3, 0.5)
  p_invalid <- c(0.2, 0.3, 0.3)

  result <- ffp(p_valid)

  expect_s3_class(result, "ffp")

  expect_error(
    ffp(p_invalid),
    "must sum to 1"
  )
})


test_that("as_ffp() uses strict probability validation", {
  p <- c(0.5, 0.5005)

  expect_s3_class(
    ffp(p),
    "ffp"
  )

  expect_error(
    as_ffp(p),
    "must sum to 1"
  )
})


# Integration with ffp combinations ---------------------------------------

test_that("average_ffp() validates its inputs", {
  p1_valid <- ffp(c(0.2, 0.3, 0.5))
  p2_valid <- ffp(c(0.1, 0.4, 0.5))

  result <- average_ffp(
    p1_valid,
    p2_valid
  )

  expect_s3_class(result, "ffp")

  expect_error(
    average_ffp(
      p1_valid,
      c(0.1, 0.4, 0.5)
    ),
    "All arguments must be"
  )

  p_short <- ffp(c(0.5, 0.5))

  expect_error(
    average_ffp(
      p1_valid,
      p_short
    ),
    "same length"
  )
})


test_that("combine_ffp() validates weights", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(0.6, 0.3)
    ),
    "must sum to 1"
  )

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(-0.2, 1.2)
    ),
    "must be non-negative"
  )
})


# assert_is_logical() -----------------------------------------------------

test_that("assert_is_logical() accepts logical vectors", {
  x <- c(TRUE, FALSE, TRUE)

  expect_no_error(
    assert_is_logical(x)
  )
})


test_that("assert_is_logical() rejects non-logical vectors", {
  x <- c(1, 0, 1)

  expect_error(
    assert_is_logical(x)
  )
})


test_that("assert_is_logical() accepts one-column logical tibbles", {
  x <- tibble::tibble(value = c(TRUE, FALSE, TRUE))

  expect_no_error(
    assert_is_logical(x)
  )
})


# assert_is_univariate() --------------------------------------------------

test_that("assert_is_univariate() accepts vectors", {
  x <- c(1, 2, 3, 4, 5)

  expect_no_error(
    assert_is_univariate(x)
  )
})


test_that("assert_is_univariate() accepts one-column matrices", {
  x <- matrix(1:5, ncol = 1)

  expect_no_error(
    assert_is_univariate(x)
  )
})


test_that("assert_is_univariate() rejects multivariate data", {
  x <- matrix(1:10, ncol = 2)

  expect_error(
    assert_is_univariate(x),
    "should be univariate"
  )
})


# Kernel-specific assertions ----------------------------------------------

test_that("assert_kernel_mean() accepts numeric scalars", {
  expect_no_error(
    assert_kernel_mean(0.5)
  )

  expect_no_error(
    assert_kernel_mean(1L)
  )
})


test_that("assert_kernel_mean() rejects non-numeric input", {
  expect_error(
    assert_kernel_mean("0.5")
  )
})


test_that("assert_kernel_mean() rejects non-scalar input", {
  expect_error(
    assert_kernel_mean(c(0.5, 1))
  )
})


test_that("assert_kernel_sigma() accepts numeric scalars", {
  expect_no_error(
    assert_kernel_sigma(0.5)
  )

  expect_no_error(
    assert_kernel_sigma(1L)
  )
})


test_that("assert_kernel_sigma() rejects non-numeric input", {
  expect_error(
    assert_kernel_sigma("0.5")
  )
})


test_that("assert_kernel_sigma() rejects non-scalar input", {
  expect_error(
    assert_kernel_sigma(c(0.5, 1))
  )
})


# assert_is_positive_integer() --------------------------------------------

test_that("assert_is_positive_integer() accepts integer-like values", {
  expect_identical(
    assert_is_positive_integer(10),
    10L
  )

  expect_identical(
    assert_is_positive_integer(10L),
    10L
  )

  expect_identical(
    assert_is_positive_integer(10.0),
    10L
  )
})


test_that("assert_is_positive_integer() rejects fractional values", {
  expect_error(
    assert_is_positive_integer(10.5),
    "must be an integer"
  )
})


test_that("assert_is_positive_integer() rejects non-positive values", {
  expect_error(
    assert_is_positive_integer(0),
    "must be positive"
  )

  expect_error(
    assert_is_positive_integer(-5),
    "must be positive"
  )
})


test_that("assert_is_positive_integer() rejects non-scalar values", {
  expect_error(
    assert_is_positive_integer(c(1, 2)),
    "must be a scalar"
  )
})


test_that("assert_is_positive_integer() rejects non-numeric values", {
  expect_error(
    assert_is_positive_integer("10"),
    "must be numeric or integer"
  )
})


# Edge cases ---------------------------------------------------------------

test_that("probability assertions handle long vectors", {
  set.seed(123)

  p_long <- runif(100000)
  p_long <- p_long / sum(p_long)

  expect_no_error(
    assert_is_probability(p_long)
  )
})


test_that("probability assertions handle single-element ffp", {
  p_single <- ffp(1)

  expect_no_error(
    assert_is_probability(p_single)
  )
})


test_that("assertions compose in a typical workflow", {
  x <- c(1, 2, 3, 4, 5)
  p <- rep(0.2, 5)

  expect_no_error(
    assert_is_equal_size(x, p)
  )

  expect_no_error(
    assert_is_probability(p)
  )

  expect_no_error(
    assert_is_univariate(x)
  )
})
