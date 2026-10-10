# Test setup ---------------------------------------------------------------

p1 <- as_ffp(
  c(
    0.2,
    0.3,
    0.5
  )
)

p2 <- as_ffp(
  c(
    0.1,
    0.4,
    0.5
  )
)

p3 <- as_ffp(
  c(
    0.3,
    0.3,
    0.4
  )
)


# average_ffp() ------------------------------------------------------------

test_that("average_ffp computes an equal-weight probability mixture", {
  result <- average_ffp(
    p1,
    p2
  )

  expected <- (
    vctrs::vec_data(p1) +
      vctrs::vec_data(p2)
  ) / 2

  expect_s3_class(
    result,
    "ffp"
  )

  expect_true(
    is_ffp(result)
  )

  expect_equal(
    vctrs::vec_data(result),
    expected
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1
  )
})


test_that("average_ffp supports multiple probability distributions", {
  result <- average_ffp(
    p1,
    p2,
    p3
  )

  expected <- (
    vctrs::vec_data(p1) +
      vctrs::vec_data(p2) +
      vctrs::vec_data(p3)
  ) / 3

  expect_s3_class(
    result,
    "ffp"
  )

  expect_equal(
    vctrs::vec_data(result),
    expected
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1
  )
})


test_that("average_ffp preserves the identity distribution", {
  result <- average_ffp(
    p1
  )

  expect_equal(
    result,
    p1
  )
})


test_that("average_ffp preserves structural zero probabilities", {
  first <- as_ffp(
    c(
      1,
      0,
      0
    )
  )

  second <- as_ffp(
    c(
      0,
      1,
      0
    )
  )

  result <- average_ffp(
    first,
    second
  )

  expect_equal(
    vctrs::vec_data(result),
    c(
      0.5,
      0.5,
      0
    )
  )
})


test_that("average_ffp validates supplied distributions", {
  expect_error(
    average_ffp(),
    "At least one"
  )

  expect_error(
    average_ffp(
      p1,
      c(
        0.2,
        0.3,
        0.5
      )
    ),
    "must be.*ffp"
  )

  different_size <- as_ffp(
    c(
      0.5,
      0.5
    )
  )

  expect_error(
    average_ffp(
      p1,
      different_size
    ),
    "same number of scenarios"
  )
})


# combine_ffp() ------------------------------------------------------------

test_that("combine_ffp computes a weighted probability mixture", {
  result <- combine_ffp(
    p1,
    p2,
    weights = c(
      0.7,
      0.3
    )
  )

  expected <- (
    0.7 * vctrs::vec_data(p1) +
      0.3 * vctrs::vec_data(p2)
  )

  expect_s3_class(
    result,
    "ffp"
  )

  expect_true(
    is_ffp(result)
  )

  expect_equal(
    vctrs::vec_data(result),
    expected
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1
  )
})


test_that("combine_ffp supports multiple probability distributions", {
  result <- combine_ffp(
    p1,
    p2,
    p3,
    weights = c(
      0.5,
      0.3,
      0.2
    )
  )

  expected <- (
    0.5 * vctrs::vec_data(p1) +
      0.3 * vctrs::vec_data(p2) +
      0.2 * vctrs::vec_data(p3)
  )

  expect_equal(
    vctrs::vec_data(result),
    expected
  )

  expect_equal(
    sum(vctrs::vec_data(result)),
    1
  )
})


test_that("average_ffp equals combine_ffp with equal weights", {
  averaged <- average_ffp(
    p1,
    p2,
    p3
  )

  combined <- combine_ffp(
    p1,
    p2,
    p3,
    weights = rep(
      1 / 3,
      3
    )
  )

  expect_equal(
    averaged,
    combined
  )
})


test_that("combine_ffp allows zero weights", {
  first <- combine_ffp(
    p1,
    p2,
    weights = c(
      1,
      0
    )
  )

  second <- combine_ffp(
    p1,
    p2,
    weights = c(
      0,
      1
    )
  )

  expect_equal(
    first,
    p1
  )

  expect_equal(
    second,
    p2
  )
})


test_that("combine_ffp preserves the identity distribution", {
  result <- combine_ffp(
    p1,
    weights = 1
  )

  expect_equal(
    result,
    p1
  )
})


test_that("combine_ffp is invariant to coordinated input reordering", {
  first <- combine_ffp(
    p1,
    p2,
    weights = c(
      0.7,
      0.3
    )
  )

  second <- combine_ffp(
    p2,
    p1,
    weights = c(
      0.3,
      0.7
    )
  )

  expect_equal(
    first,
    second
  )
})


test_that("combine_ffp validates supplied distributions", {
  expect_error(
    combine_ffp(
      weights = numeric()
    ),
    "At least one"
  )

  expect_error(
    combine_ffp(
      p1,
      c(
        0.1,
        0.4,
        0.5
      ),
      weights = c(
        0.5,
        0.5
      )
    ),
    "must be.*ffp"
  )

  different_size <- as_ffp(
    c(
      0.5,
      0.5
    )
  )

  expect_error(
    combine_ffp(
      p1,
      different_size,
      weights = c(
        0.5,
        0.5
      )
    ),
    "same number of scenarios"
  )
})


test_that("combine_ffp requires mixture weights", {
  expect_error(
    combine_ffp(
      p1,
      p2
    ),
    "requires"
  )
})


test_that("combine_ffp requires one weight per distribution", {
  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = 1
    ),
    "one value per distribution"
  )

  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = c(
        0.2,
        0.3,
        0.5
      )
    ),
    "one value per distribution"
  )
})


test_that("combine_ffp requires finite numeric vector weights", {
  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = c(
        "0.5",
        "0.5"
      )
    ),
    "finite numeric vector"
  )

  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = c(
        NA_real_,
        NA_real_
      )
    ),
    "finite numeric vector"
  )

  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = c(
        Inf,
        -Inf
      )
    ),
    "finite numeric vector"
  )

  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = matrix(
        c(
          0.5,
          0.5
        )
      )
    ),
    "finite numeric vector"
  )
})


test_that("combine_ffp requires non-negative weights", {
  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = c(
        -0.2,
        1.2
      )
    ),
    "non-negative"
  )
})


test_that("combine_ffp requires weights that sum to one", {
  expect_error(
    combine_ffp(
      p1,
      p2,
      weights = c(
        0.6,
        0.3
      )
    ),
    "sum to 1"
  )
})


# Mathematical properties -------------------------------------------------

test_that("combined probabilities remain a valid probability distribution", {
  result <- combine_ffp(
    p1,
    p2,
    p3,
    weights = c(
      0.2,
      0.3,
      0.5
    )
  )

  probabilities <- vctrs::vec_data(
    result
  )

  expect_true(
    all(probabilities >= 0)
  )

  expect_true(
    all(probabilities <= 1)
  )

  expect_equal(
    sum(probabilities),
    1
  )
})
