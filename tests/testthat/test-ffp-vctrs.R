set.seed(123)
p <- runif(10)
p <- p / sum(p)
x <- ffp(p)

# less than 5 obs
p_short <- runif(4)
p_short <- p_short / sum(p_short)
p_short <- as_ffp(p_short)

# ffp() -------------------------------------------------------------------

test_that("testing ffp()", {
  expect_type(x, "double")
  expect_s3_class(x, "ffp")
  expect_length(x, 10L)
  expect_type(p_short, "double")
  expect_s3_class(p_short, "ffp")
  expect_length(p_short, 4L)
})


# is_ffp() ----------------------------------------------------------------

test_that("testing is_ffp()", {
  expect_type(is_ffp(x), "logical")
  expect_length(is_ffp(x), 1L)
  expect_true(is_ffp(x))
  expect_false(is_ffp(p))
})


# as_ffp() ----------------------------------------------------------------

test_that("testing as_ffp() works on double's", {
  expect_type(as_ffp(p), "double")
  expect_s3_class(as_ffp(p), "ffp")
  expect_length(as_ffp(p), 10L)
  expect_true(is_ffp(as_ffp(p)))
})

int <- as.integer(round(stats::runif(100), 0))
int <- int / sum(int)
test_that("testing as_ffp() works on integers's", {
  expect_type(as_ffp(int), "double")
  expect_s3_class(as_ffp(int), "ffp")
  expect_length(as_ffp(int), 100L)
  expect_true(is_ffp(as_ffp(int)))
})




# Concatenation -----------------------------------------------------------

p2 <- runif(10)
p2 <- p2 / sum(p2)
y <- ffp(p2)

test_that("concatenation of `ffp`s yields `ffp`", {
  expect_type(c(x, y), "double")
  expect_s3_class(c(x, y), "ffp")
  expect_length(c(x, y), 20L)
})

test_that("concatenation of double + ffp yields double", {
  expect_type(c(x, runif(10)), "double")
  expect_type(c(runif(10, x)), "double") # swipe order
  expect_false(is_ffp(c(x, runif(10))))
  expect_false(is_ffp(c(runif(10), x)))  # swipe order
  expect_length(c(x, runif(10)), 20L)
  expect_length(c(runif(10), x), 20L)    # swipe order
})


# Math --------------------------------------------------------------------

test_that("`ffp` supports arith operations yielding double's", {
  expect_equal(x + y, p + p2)
  expect_equal(x - y, p - p2)
  expect_type(x + y, "double")
  expect_type(x - y, "double")
  expect_length(x + y, 10L)
  expect_length(x - y, 10L)
})

test_that("`ffp` supports math operations yielding double's", {
  expect_equal(sign(x), sign(p))
  expect_equal(sqrt(x) , sqrt(p))
  expect_type(sign(x), "double")
  expect_type(sqrt(x), "double")
  expect_length(sign(x), 10L)
  expect_length(sqrt(x), 10L)
})


test_that("`ffp` prints correctly", {
  expect_snapshot_output(x)
  expect_snapshot_output(p_short)
})

