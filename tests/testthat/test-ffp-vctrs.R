# tests/testthat/test-ffp-vctrs.R


# Constructors -------------------------------------------------------------

test_that("ffp() creates an ffp vector", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_s3_class(x, "ffp")
  expect_true(is_ffp(x))
  expect_equal(as.double(x), c(0.2, 0.3, 0.5))
})


test_that("ffp() preserves valid probabilities", {
  x <- c(0.1, 0.2, 0.3, 0.4)

  result <- ffp(x)

  expect_s3_class(result, "ffp")
  expect_equal(as.double(result), x)
  expect_equal(sum(result), 1)
})


test_that("ffp() accepts probabilities within its practical tolerance", {
  x <- c(0.5, 0.5005)

  result <- ffp(x)

  expect_s3_class(result, "ffp")
  expect_true(is_ffp(result))
})


test_that("ffp() rejects probabilities that do not sum to one", {
  expect_error(
    ffp(c(0.5, 0.6, 0.5)),
    "sum to 1"
  )
})


test_that("ffp() rejects negative probabilities", {
  expect_error(
    ffp(c(-0.1, 0.4, 0.7)),
    "non-negative"
  )
})


test_that("ffp() rejects invalid numeric probability vectors", {
  expect_error(
    ffp(c(0.2, 0.3, 0.4)),
    "sum to 1"
  )
})


# is_ffp() -----------------------------------------------------------------

test_that("is_ffp() correctly identifies ffp objects", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_true(is_ffp(x))
  expect_false(is_ffp(c(0.2, 0.3, 0.5)))
  expect_false(is_ffp(1))
  expect_false(is_ffp(NULL))
})


# as_ffp() -----------------------------------------------------------------

test_that("as_ffp() converts valid doubles to ffp", {
  x <- c(0.2, 0.3, 0.5)

  result <- as_ffp(x)

  expect_s3_class(result, "ffp")
  expect_equal(as.double(result), x)
})


test_that("as_ffp() converts valid integers to ffp", {
  result <- as_ffp(c(0L, 0L, 1L))

  expect_s3_class(result, "ffp")
  expect_equal(as.double(result), c(0, 0, 1))
})


test_that("as_ffp() returns ffp objects unchanged", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- as_ffp(x)

  expect_identical(result, x)
})


test_that("as_ffp() uses stricter validation than ffp()", {
  x <- c(0.5, 0.5005)

  expect_s3_class(
    ffp(x),
    "ffp"
  )

  expect_error(
    as_ffp(x),
    "sum to 1"
  )
})


test_that("as_ffp() rejects negative probabilities", {
  expect_error(
    as_ffp(c(-0.1, 0.4, 0.7)),
    "non-negative"
  )
})


test_that("as_ffp() rejects probabilities that do not sum to one", {
  expect_error(
    as_ffp(c(0.2, 0.3, 0.4)),
    "sum to 1"
  )
})


# new_ffp() ----------------------------------------------------------------

test_that("new_ffp() creates the low-level ffp representation", {
  x <- new_ffp(c(0.2, 0.3, 0.5))

  expect_s3_class(x, "ffp")
  expect_true(is_ffp(x))
  expect_equal(vctrs::vec_data(x), c(0.2, 0.3, 0.5))
})


test_that("new_ffp() requires a double vector", {
  expect_error(
    new_ffp(c(0L, 0L, 1L))
  )
})


# vctrs type information --------------------------------------------------

test_that("ffp has the expected abbreviated type", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_identical(
    vctrs::vec_ptype_abbr(x),
    "ffp"
  )
})


test_that("common type of ffp and ffp is double", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- ffp(c(0.1, 0.4, 0.5))

  result <- vctrs::vec_ptype2(x, y)

  expect_type(result, "double")
  expect_false(is_ffp(result))
})


test_that("common type of ffp and double is double", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- c(0.1, 0.4, 0.5)

  expect_type(
    vctrs::vec_ptype2(x, y),
    "double"
  )

  expect_type(
    vctrs::vec_ptype2(y, x),
    "double"
  )
})


# Casting ------------------------------------------------------------------

test_that("casting ffp to ffp preserves the object", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- vctrs::vec_cast(x, new_ffp())

  expect_identical(result, x)
})


test_that("casting ffp to double removes the ffp class", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- vctrs::vec_cast(x, double())

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_equal(result, c(0.2, 0.3, 0.5))
})


test_that("casting valid double to ffp validates probabilities", {
  x <- c(0.2, 0.3, 0.5)

  result <- vctrs::vec_cast(x, new_ffp())

  expect_s3_class(result, "ffp")
  expect_equal(as.double(result), x)
})


test_that("casting invalid double to ffp is rejected", {
  expect_error(
    vctrs::vec_cast(c(0.2, 0.3, 0.4), new_ffp()),
    "sum to 1"
  )
})


# Concatenation ------------------------------------------------------------

test_that("c(ffp, ffp) returns double", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- ffp(c(0.1, 0.4, 0.5))

  result <- c(x, y)

  expect_type(result, "double")
  expect_false(is_ffp(result))

  expect_equal(
    result,
    c(0.2, 0.3, 0.5, 0.1, 0.4, 0.5)
  )
})


test_that("c(ffp, double) returns double", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- c(0.1, 0.4, 0.5)

  result <- c(x, y)

  expect_type(result, "double")
  expect_false(is_ffp(result))

  expect_equal(
    result,
    c(0.2, 0.3, 0.5, 0.1, 0.4, 0.5)
  )
})


test_that("c(double, ffp) returns double", {
  x <- c(0.1, 0.4, 0.5)
  y <- ffp(c(0.2, 0.3, 0.5))

  result <- c(x, y)

  expect_type(result, "double")
  expect_false(is_ffp(result))

  expect_equal(
    result,
    c(0.1, 0.4, 0.5, 0.2, 0.3, 0.5)
  )
})


# Subsetting ---------------------------------------------------------------

test_that("single element extraction with [[ returns double", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- x[[1]]

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_identical(result, 0.2)
})


test_that("subsetting with [ returns double", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- x[1:2]

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_equal(result, c(0.2, 0.3))
})


test_that("logical subsetting returns double", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- x[c(TRUE, FALSE, TRUE)]

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_equal(result, c(0.2, 0.5))
})


test_that("reordering an ffp vector with [ returns double", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- x[c(3, 2, 1)]

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_equal(result, c(0.5, 0.3, 0.2))
})


# Repetition ---------------------------------------------------------------

test_that("rep() drops the ffp class", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- rep(x, 2)

  expect_type(result, "double")
  expect_false(is_ffp(result))

  expect_equal(
    result,
    c(0.2, 0.3, 0.5, 0.2, 0.3, 0.5)
  )
})


# Assignment ---------------------------------------------------------------

test_that("direct assignment with [ is rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    x[1] <- 0.4,
    class = "ffp_error_assignment"
  )
})


test_that("direct assignment with [[ is rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    x[[1]] <- 0.4,
    class = "ffp_error_assignment"
  )
})


# Arithmetic ---------------------------------------------------------------

test_that("ffp + ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    x + y,
    class = "ffp_error_arithmetic"
  )
})


test_that("ffp - ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    x - y,
    class = "ffp_error_arithmetic"
  )
})


test_that("ffp * scalar is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    x * 0.5,
    class = "ffp_error_arithmetic"
  )
})


test_that("scalar * ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    0.5 * x,
    class = "ffp_error_arithmetic"
  )
})


test_that("ffp / ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))
  y <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    x / y,
    class = "ffp_error_arithmetic"
  )
})


test_that("ffp / scalar is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    x / 2,
    class = "ffp_error_arithmetic"
  )
})


test_that("scalar / ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    1 / x,
    class = "ffp_error_arithmetic"
  )
})

test_that("integer * ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    2L * x,
    class = "ffp_error_arithmetic"
  )
})


test_that("integer / ffp is explicitly rejected", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    1L / x,
    class = "ffp_error_arithmetic"
  )
})


# Mathematical summaries --------------------------------------------------

test_that("sum() works on ffp and returns a double", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- sum(x)

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_equal(result, 1)
})


test_that("mean() works on ffp and returns a double", {
  x <- ffp(c(0.2, 0.3, 0.5))

  result <- mean(x)

  expect_type(result, "double")
  expect_false(is_ffp(result))
  expect_equal(result, 1 / 3)
})


test_that("min() and max() work on ffp", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_equal(min(x), 0.2)
  expect_equal(max(x), 0.5)

  expect_false(is_ffp(min(x)))
  expect_false(is_ffp(max(x)))
})


# Printing -----------------------------------------------------------------

test_that("short ffp vectors print without error", {
  x <- ffp(c(0.2, 0.3, 0.5))

  expect_output(
    print(x)
  )
})


test_that("long ffp vectors print without error", {
  x <- ffp(rep(0.1, 10))

  expect_output(
    print(x)
  )
})


# Probability-preserving combinations -------------------------------------

test_that("average_ffp() preserves the probability invariant", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  result <- average_ffp(p1, p2)

  expect_s3_class(result, "ffp")
  expect_true(is_ffp(result))
  expect_equal(sum(result), 1)

  expect_equal(
    as.double(result),
    c(0.15, 0.35, 0.50)
  )
})


test_that("combine_ffp() preserves the probability invariant", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  result <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.7, 0.3)
  )

  expect_s3_class(result, "ffp")
  expect_true(is_ffp(result))
  expect_equal(sum(result), 1)

  expect_equal(
    as.double(result),
    c(0.17, 0.33, 0.50)
  )
})


test_that("average_ffp() is equivalent to equal-weight combine_ffp()", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  averaged <- average_ffp(p1, p2)

  combined <- combine_ffp(
    p1 = p1,
    p2 = p2,
    weights = c(0.5, 0.5)
  )

  expect_equal(
    as.double(averaged),
    as.double(combined)
  )

  expect_s3_class(averaged, "ffp")
  expect_s3_class(combined, "ffp")
})


test_that("average_ffp() works with a single distribution", {
  p <- ffp(c(0.2, 0.3, 0.5))

  result <- average_ffp(p)

  expect_s3_class(result, "ffp")
  expect_equal(as.double(result), as.double(p))
})


test_that("average_ffp() requires at least one ffp object", {
  expect_error(
    average_ffp(),
    "At least one"
  )
})


test_that("average_ffp() rejects non-ffp inputs", {
  p <- ffp(c(0.2, 0.3, 0.5))

  expect_error(
    average_ffp(p, c(0.1, 0.4, 0.5)),
    "must be.*ffp"
  )
})


test_that("average_ffp() requires distributions of equal length", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.4, 0.6))

  expect_error(
    average_ffp(p1, p2),
    "same length"
  )
})


test_that("combine_ffp() requires at least one ffp object", {
  expect_error(
    combine_ffp(weights = numeric()),
    "At least one"
  )
})


test_that("combine_ffp() rejects non-ffp inputs", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- c(0.1, 0.4, 0.5)

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(0.5, 0.5)
    ),
    "must be.*ffp"
  )
})


test_that("combine_ffp() requires numeric weights", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c("0.5", "0.5")
    ),
    "numeric"
  )
})


test_that("combine_ffp() requires one weight per distribution", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = 1
    ),
    "Number of weights"
  )
})


test_that("combine_ffp() rejects negative weights", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(1.2, -0.2)
    ),
    "non-negative"
  )
})


test_that("combine_ffp() requires weights that sum to one", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.1, 0.4, 0.5))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(0.6, 0.6)
    ),
    "sum to 1"
  )
})


test_that("combine_ffp() requires distributions of equal length", {
  p1 <- ffp(c(0.2, 0.3, 0.5))
  p2 <- ffp(c(0.4, 0.6))

  expect_error(
    combine_ffp(
      p1 = p1,
      p2 = p2,
      weights = c(0.5, 0.5)
    ),
    "same length"
  )
})

