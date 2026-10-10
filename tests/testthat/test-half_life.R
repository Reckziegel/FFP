test_that("half_life converts decay rates exactly", {
  lambda <- log(2) / 42

  expect_equal(
    half_life(lambda),
    42
  )
})


test_that("half_life does not round the implied half-life", {
  result <- half_life(
    0.0166
  )

  expect_equal(
    result,
    log(2) / 0.0166
  )

  expect_false(
    result == round(result)
  )
})


test_that("half_life returns a numeric scalar", {
  result <- half_life(
    0.01
  )

  expect_type(
    result,
    "double"
  )

  expect_length(
    result,
    1L
  )
})


test_that("half_life requires a finite numeric scalar", {
  expect_error(
    half_life("0.01"),
    "single finite number"
  )

  expect_error(
    half_life(
      c(
        0.01,
        0.02
      )
    ),
    "single finite number"
  )

  expect_error(
    half_life(
      matrix(0.01)
    ),
    "single finite number"
  )

  expect_error(
    half_life(
      NA_real_
    ),
    "single finite number"
  )

  expect_error(
    half_life(
      Inf
    ),
    "single finite number"
  )
})


test_that("half_life requires a positive decay rate", {
  expect_error(
    half_life(0),
    "strictly positive"
  )

  expect_error(
    half_life(-0.01),
    "strictly positive"
  )
})
