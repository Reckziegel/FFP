ret_ts <- diff(
  log(EuStockMarkets)
)

ret_mtx <- matrix(
  ret_ts,
  ncol = 4,
  dimnames = list(
    NULL,
    colnames(ret_ts)
  )
)

ret_xts <- xts::xts(
  ret_mtx,
  order.by = Sys.Date() + seq_len(nrow(ret_mtx))
)

ret_df <- as.data.frame(
  ret_ts
)

ret_tbl <- tibble::as_tibble(
  ret_ts
)

p_ts <- exp_decay(
  ret_ts,
  0.001
)

p_mtx <- exp_decay(
  ret_mtx,
  0.001
)

p_xts <- exp_decay(
  ret_xts,
  0.001
)

p_df <- exp_decay(
  ret_df,
  0.001
)

p_tbl <- exp_decay(
  ret_tbl,
  0.001
)


test_that("empirical_stats returns one row per variable", {
  result <- empirical_stats(
    x = ret_mtx,
    p = p_mtx,
    level = 0.05
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_identical(
    nrow(result),
    4L
  )

  expect_identical(
    ncol(result),
    7L
  )

  expect_named(
    result,
    c(
      "variable",
      "mean",
      "sd",
      "skewness",
      "kurtosis",
      "value_at_risk",
      "expected_shortfall"
    )
  )

  expect_identical(
    result$variable,
    colnames(ret_mtx)
  )
})


test_that("empirical_stats supports legacy input classes", {
  inputs <- list(
    ts = list(
      x = ret_ts,
      p = p_ts
    ),
    matrix = list(
      x = ret_mtx,
      p = p_mtx
    ),
    xts = list(
      x = ret_xts,
      p = p_xts
    ),
    data_frame = list(
      x = ret_df,
      p = p_df
    ),
    tibble = list(
      x = ret_tbl,
      p = p_tbl
    )
  )

  for (input in inputs) {
    result <- empirical_stats(
      x = input$x,
      p = input$p,
      level = 0.05
    )

    expect_s3_class(
      result,
      "tbl_df"
    )

    expect_identical(
      nrow(result),
      4L
    )

    expect_identical(
      ncol(result),
      7L
    )
  }
})


test_that("empirical_stats supports univariate distributions", {
  result <- empirical_stats(
    x = ret_mtx[, 1],
    p = p_mtx,
    level = 0.05
  )

  expect_identical(
    nrow(result),
    1L
  )

  expect_identical(
    result$variable,
    "x"
  )

  expect_true(
    all(
      is.finite(
        unlist(
          result[
            setdiff(
              names(result),
              "variable"
            )
          ]
        )
      )
    )
  )
})


test_that("empirical_stats delegates marginal statistics to ffp_stats", {
  result <- empirical_stats(
    x = ret_mtx,
    p = p_mtx,
    level = 0.05
  )

  expected <- ffp_stats(
    x = ret_mtx,
    p = p_mtx,
    prob = 0.05
  )$statistics

  expect_equal(
    result$mean,
    expected$mean
  )

  expect_equal(
    result$sd,
    expected$sd
  )

  expect_equal(
    result$skewness,
    expected$skewness
  )

  expect_equal(
    result$kurtosis,
    expected$kurtosis
  )
})


test_that("empirical_stats uses the FFP 2.0 loss convention", {
  x <- c(
    -3,
    -1,
    2
  )

  p <- as_ffp(
    c(
      0.1,
      0.4,
      0.5
    )
  )

  result <- empirical_stats(
    x = x,
    p = p,
    level = 0.25
  )

  risk <- ffp_risk(
    x = x,
    p = p,
    confidence = 0.75
  )$risk

  expect_equal(
    result$value_at_risk,
    risk$value_at_risk
  )

  expect_equal(
    result$expected_shortfall,
    risk$expected_shortfall
  )
})


test_that("empirical_stats uses exactly the requested tail mass", {
  x <- c(
    -10,
    -2,
    1,
    3
  )

  p <- as_ffp(
    c(
      0.05,
      0.35,
      0.30,
      0.30
    )
  )

  result <- empirical_stats(
    x = x,
    p = p,
    level = 0.20
  )

  expected_shortfall <- -(
    -10 * 0.05 +
      -2 * 0.15
  ) / 0.20

  expect_equal(
    result$value_at_risk,
    2
  )

  expect_equal(
    result$expected_shortfall,
    expected_shortfall
  )
})


test_that("empirical_stats tail level changes the risk measures", {
  level_001 <- empirical_stats(
    ret_mtx[, 1],
    p_mtx,
    level = 0.01
  )

  level_005 <- empirical_stats(
    ret_mtx[, 1],
    p_mtx,
    level = 0.05
  )

  level_010 <- empirical_stats(
    ret_mtx[, 1],
    p_mtx,
    level = 0.10
  )

  expect_gte(
    level_001$value_at_risk,
    level_005$value_at_risk
  )

  expect_gte(
    level_005$value_at_risk,
    level_010$value_at_risk
  )
})


test_that("empirical_stats requires an ffp probability distribution", {
  expect_error(
    empirical_stats(
      ret_mtx[, 1],
      as.double(p_mtx)
    ),
    "must be an"
  )
})


test_that("empirical_stats validates scenario and probability sizes", {
  expect_error(
    empirical_stats(
      EuStockMarkets,
      p_ts
    ),
    "same number"
  )
})


test_that("empirical_stats validates tail probability", {
  expect_error(
    empirical_stats(
      ret_mtx[, 1],
      p_mtx,
      level = 0
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    empirical_stats(
      ret_mtx[, 1],
      p_mtx,
      level = 1
    ),
    "strictly between 0 and 1"
  )

  expect_error(
    empirical_stats(
      ret_mtx[, 1],
      p_mtx,
      level = c(
        0.02,
        0.05
      )
    ),
    "single finite number"
  )

  expect_error(
    empirical_stats(
      ret_mtx[, 1],
      p_mtx,
      level = matrix(0.01)
    ),
    "single finite number"
  )
})
