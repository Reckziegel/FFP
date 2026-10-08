ret_ts  <- diff(log(EuStockMarkets))
ret_mtx <- matrix(ret_ts, ncol = 4)
ret_xts <- xts::xts(ret_mtx, order.by = Sys.Date() + 1:nrow(ret_mtx))
ret_tbl <- tibble::as_tibble(ret_ts)

p <- rep(1 / nrow(ret_ts), nrow(ret_ts))

vmean_ts <- view_on_correlation(x = ret_ts, cor = cor(ret_ts))
test_that("`view_on_correlation` works for ts", {
  expect_type(vmean_ts, "list")
  expect_s3_class(vmean_ts, "ffp_views")
  expect_length(vmean_ts, 2L)
  expect_named(vmean_ts, c("Aeq", "beq"))
})

vmean_mtx <- view_on_correlation(x = ret_mtx, cor = cor(ret_mtx))
test_that("`view_on_correlation` works for matrix", {
  expect_type(vmean_mtx, "list")
  expect_s3_class(vmean_mtx, "ffp_views")
  expect_length(vmean_mtx, 2L)
  expect_named(vmean_mtx, c("Aeq", "beq"))
})

vmean_xts <- view_on_correlation(x = ret_xts, cor = cor(ret_xts))
test_that("`view_on_correlation` works for xts", {
  expect_type(vmean_xts, "list")
  expect_s3_class(vmean_xts, "ffp_views")
  expect_length(vmean_xts, 2L)
  expect_named(vmean_xts, c("Aeq", "beq"))
})

vmean_tbl <- view_on_correlation(x = ret_tbl, cor = cor(ret_tbl))
test_that("`view_on_correlation` works for tbl_df", {
  expect_type(vmean_tbl, "list")
  expect_s3_class(vmean_tbl, "ffp_views")
  expect_length(vmean_tbl, 2L)
  expect_named(vmean_tbl, c("Aeq", "beq"))
})


test_that("view_on_correlation matches the FFP 2.0 correlation constraint", {
  x <- matrix(
    c(
      -0.03,  0.01,
      -0.01,  0.03,
      0.00, -0.01,
      0.02,  0.04,
      0.04,  0.02
    ),
    ncol = 2,
    byrow = TRUE
  )

  target <- matrix(
    c(
      1.0, 0.4,
      0.4, 1.0
    ),
    nrow = 2,
    byrow = TRUE
  )

  legacy <- view_on_correlation(
    x = x,
    cor = target
  )

  model <- ffp_model(x) |>
    ffp_prior(
      prior_uniform()
    ) |>
    ffp_view(
      view_correlation(
        x = x,
        target = target
      )
    )

  constraints <- compile_views(model)

  expect_equal(
    legacy$Aeq,
    constraints$a_eq
  )

  expect_equal(
    as.double(legacy$beq),
    constraints$b_eq,
    tolerance = 1e-15
  )
})


test_that("view_on_correlation diagonal preserves reference second moments", {
  x <- matrix(
    c(
      -2,  3,
      -1,  1,
      0, -1,
      1,  0,
      4,  2
    ),
    ncol = 2,
    byrow = TRUE
  )

  result <- view_on_correlation(
    x = x,
    cor = diag(2)
  )

  expected_second_moments <- colMeans(
    x^2
  )

  expect_equal(
    result$beq[c(1, 3), 1],
    expected_second_moments,
    tolerance = 1e-15
  )
})


test_that("view_on_correlation uses probability-weighted dispersion", {
  x <- matrix(
    c(
      -2,  3,
      -1,  1,
      0, -1,
      1,  0,
      4,  2
    ),
    ncol = 2,
    byrow = TRUE
  )

  target <- matrix(
    c(
      1.0, 0.5,
      0.5, 1.0
    ),
    nrow = 2,
    byrow = TRUE
  )

  prior <- rep(
    1 / nrow(x),
    nrow(x)
  )

  means <- colSums(
    x * prior
  )

  sds <- sqrt(
    colSums(
      sweep(
        x,
        MARGIN = 2,
        STATS = means,
        FUN = "-"
      )^2 *
        prior
    )
  )

  expected_cross_moment <- (
    means[[1]] * means[[2]] +
      sds[[1]] * sds[[2]] *
      target[1, 2]
  )

  result <- view_on_correlation(
    x = x,
    cor = target
  )

  expect_equal(
    result$beq[2, 1],
    expected_cross_moment,
    tolerance = 1e-15
  )
})
