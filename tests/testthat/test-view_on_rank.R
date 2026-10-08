ret_ts  <- diff(log(EuStockMarkets))
ret_mtx <- matrix(ret_ts, ncol = 4)
ret_xts <- xts::xts(ret_mtx, order.by = Sys.Date() + 1:nrow(ret_mtx))
ret_tbl <- tibble::as_tibble(ret_ts)

p   <- rep(1 / nrow(ret_ts), nrow(ret_ts))

vmean_ts <- view_on_rank(x = ret_ts, rank = c(1, 2))
test_that("`view_on_rank` works for ts", {
  expect_type(vmean_ts, "list")
  expect_s3_class(vmean_ts, "ffp_views")
  expect_length(vmean_ts, 2L)
  expect_named(vmean_ts, c("A", "b"))
})

vmean_mtx <- view_on_rank(x = ret_mtx, rank = c(1, 2))
test_that("`view_on_rank` works for matrix", {
  expect_type(vmean_mtx, "list")
  expect_s3_class(vmean_mtx, "ffp_views")
  expect_length(vmean_mtx, 2L)
  expect_named(vmean_ts, c("A", "b"))
})

vmean_xts <- view_on_rank(x = ret_xts, rank = c(1, 2))
test_that("`view_on_rank` works for xts", {
  expect_type(vmean_xts, "list")
  expect_s3_class(vmean_xts, "ffp_views")
  expect_length(vmean_xts, 2L)
  expect_named(vmean_ts, c("A", "b"))
})

vmean_tbl <- view_on_rank(x = ret_tbl, rank = c(1, 2))
test_that("`view_on_rank` works for tbl_df", {
  expect_type(vmean_tbl, "list")
  expect_s3_class(vmean_tbl, "ffp_views")
  expect_length(vmean_tbl, 2L)
  expect_named(vmean_ts, c("A", "b"))
})

test_that("view_on_rank follows descending expected-return order", {
  x <- matrix(
    c(
      -0.02,  0.01,  0.03,
      0.00,  0.02,  0.01,
      0.03, -0.01,  0.02,
      0.01,  0.00, -0.01
    ),
    ncol = 3,
    byrow = TRUE
  )

  result <- view_on_rank(
    x = x,
    rank = c(1, 2, 3)
  )

  expected <- rbind(
    x[, 2] - x[, 1],
    x[, 3] - x[, 2]
  )

  expect_equal(
    result$A,
    expected
  )

  expect_equal(
    as.double(result$b),
    c(0, 0)
  )
})


test_that("view_on_rank matches the FFP 2.0 rank constraint", {
  x <- matrix(
    c(
      -0.02,  0.01,  0.03,
      0.00,  0.02,  0.01,
      0.03, -0.01,  0.02,
      0.01,  0.00, -0.01
    ),
    ncol = 3,
    byrow = TRUE
  )

  legacy <- view_on_rank(
    x = x,
    rank = c(1, 3, 2)
  )

  model <- ffp_model(x) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_rank(
        x = x,
        order = c(1, 3, 2)
      )
    )

  constraints <- compile_views(model)

  expect_equal(
    legacy$A,
    constraints$a_ineq
  )

  expect_equal(
    as.double(legacy$b),
    constraints$b_ineq
  )
})


test_that("view_on_rank uses adjacent ranking constraints", {
  x <- matrix(
    seq_len(20),
    ncol = 4
  )

  result <- view_on_rank(
    x = x,
    rank = c(4, 2, 1)
  )

  expect_equal(
    nrow(result$A),
    2L
  )

  expect_equal(
    result$A[1, ],
    x[, 2] - x[, 4]
  )

  expect_equal(
    result$A[2, ],
    x[, 1] - x[, 2]
  )
})
