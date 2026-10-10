test_that("bind_views combines equality and inequality views", {
  x <- matrix(
    c(
      -2,  1,  0,
      -1,  2,  1,
      0,  0,  2,
      1, -1,  3,
      2,  1,  4
    ),
    ncol = 3,
    byrow = TRUE
  )

  mean_view <- view_on_mean(
    x = x,
    mean = colMeans(x)
  )

  rank_view <- view_on_rank(
    x = x,
    rank = c(3, 1, 2)
  )

  result <- bind_views(
    mean_view,
    rank_view
  )

  expect_s3_class(
    result,
    "ffp_views"
  )

  expect_named(
    result,
    c(
      "Aeq",
      "beq",
      "A",
      "b"
    )
  )

  expect_equal(
    result$Aeq,
    mean_view$Aeq
  )

  expect_equal(
    result$beq,
    mean_view$beq
  )

  expect_equal(
    result$A,
    rank_view$A
  )

  expect_equal(
    result$b,
    rank_view$b
  )
})

test_that("bind_views matches the FFP 2.0 constraint binder", {
  x <- matrix(
    c(
      -2,  1,  0,
      -1,  2,  1,
      0,  0,  2,
      1, -1,  3,
      2,  1,  4
    ),
    ncol = 3,
    byrow = TRUE
  )

  first <- view_on_mean(
    x = x,
    mean = colMeans(x)
  )

  second <- view_on_rank(
    x = x,
    rank = c(3, 1, 2)
  )

  result <- bind_views(
    first,
    second
  )

  first_constraints <- legacy_view_to_constraints(
    first
  )

  second_constraints <- legacy_view_to_constraints(
    second
  )

  expected <- bind_ffp_constraints(
    list(
      first_constraints,
      second_constraints
    )
  )

  expect_equal(
    result$Aeq,
    expected$a_eq
  )

  expect_equal(
    as.double(result$beq),
    expected$b_eq
  )

  expect_equal(
    result$A,
    expected$a_ineq
  )

  expect_equal(
    as.double(result$b),
    expected$b_ineq
  )
})

test_that("bind_views supports equality-only views", {
  x <- matrix(
    c(
      -2,  1,
      -1,  2,
      0,  0,
      1, -1,
      2,  1
    ),
    ncol = 2,
    byrow = TRUE
  )

  mean_view <- view_on_mean(
    x = x,
    mean = colMeans(x)
  )

  volatility_view <- view_on_volatility(
    x = x,
    vol = c(
      1.5,
      1.0
    )
  )

  result <- bind_views(
    mean_view,
    volatility_view
  )

  expect_equal(
    result$Aeq,
    rbind(
      mean_view$Aeq,
      volatility_view$Aeq
    )
  )

  expect_equal(
    result$beq,
    rbind(
      mean_view$beq,
      volatility_view$beq
    )
  )

  expect_null(
    result$A
  )

  expect_null(
    result$b
  )
})

test_that("bind_views requires a common scenario support", {
  mean_view <- view_on_mean(
    x = matrix(
      seq_len(10),
      ncol = 2
    ),
    mean = c(
      3,
      8
    )
  )

  rank_view <- view_on_rank(
    x = matrix(
      seq_len(8),
      ncol = 2
    ),
    rank = c(
      1,
      2
    )
  )

  expect_error(
    bind_views(
      mean_view,
      rank_view
    ),
    class = "ffp_error_invalid_constraints"
  )
})

test_that("bind_views requires at least one legacy view", {
  expect_error(
    bind_views(),
    class = "ffp_error_invalid_constraints"
  )
})
