library(dplyr, warn.conflicts = FALSE)

# ts testing
ret_ts  <- diff(log(EuStockMarkets))
ret_mtx <- matrix(ret_ts, ncol = 4)
ret_xts <- xts::xts(ret_mtx, order.by = Sys.Date() + 1:nrow(ret_mtx))
ret_df  <- as.data.frame(ret_ts)
ret_tbl <- tibble::as_tibble(ret_ts)

p_ts  <- exp_decay(ret_ts, 0.001)
p_mtx <- exp_decay(ret_mtx, 0.001)
p_xts <- exp_decay(ret_xts, 0.001)
p_df  <- exp_decay(ret_df, 0.001)
p_tbl <- exp_decay(ret_tbl, 0.001)

# ts
es_ts_u <- empirical_stats(ret_ts[ , 1], p_ts)
es_ts_m <- empirical_stats(ret_ts, p_ts, 0.05)

# mtx
es_mtx_u <- empirical_stats(ret_mtx[ , 1], p_mtx)
es_mtx_m <- empirical_stats(ret_mtx, p_mtx, 0.05)

# xts
es_xts_u <- empirical_stats(ret_xts[ , 1], p_xts)
es_xts_m <- empirical_stats(ret_xts, p_xts, 0.05)

# df
es_df_u <- empirical_stats(ret_df[ , 1], p_df)
es_df_m <- empirical_stats(ret_df, p_df, 0.05)

# tbl
es_tbl_u <- empirical_stats(ret_tbl[ , 1], p_tbl)
es_tbl_m <- empirical_stats(ret_tbl, p_tbl, 0.05)

test_that("level must be a single double", {
  expect_error(empirical_stats(ret_ts[ , 1], p_ts, c(0.02, 0.05)))
  expect_error(empirical_stats(ret_ts[ , 1], p_ts, matrix(0.01)))
})

test_that("p must be from an `ffp` class", {
  expect_error(empirical_stats(ret_ts[ , 1], as.double(p_ts)))
})

test_that("`x` and `p` must have the same size", {
  expect_error(empirical_stats(EuStockMarkets, p_ts))
})

test_that("`empirical_stats` works on ts data", {
  # dimension
  expect_equal(NCOL(es_ts_u), 3L)
  expect_equal(NROW(es_ts_m), 24L)
  #class
  expect_type(es_ts_u, "list")
  expect_type(es_ts_m, "list")
  expect_s3_class(es_ts_u, "tbl_df")
  expect_s3_class(es_ts_m, "tbl_df")
  expect_equal(
    purrr::map(es_ts_u, class), list(stat = "factor", name = "character", value = "numeric")
  )
  expect_equal(
    purrr::map(es_ts_m, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("`empirical_stats` works on mtx data", {
  # dimension
  expect_equal(NCOL(es_mtx_u), 3L)
  expect_equal(NROW(es_mtx_m), 24L)
  #class
  expect_type(es_mtx_u, "list")
  expect_type(es_mtx_m, "list")
  expect_s3_class(es_mtx_u, "tbl_df")
  expect_s3_class(es_mtx_m, "tbl_df")
  expect_equal(
    purrr::map(es_mtx_u, class), list(stat = "factor", name = "character", value = "numeric")
  )
  expect_equal(
    purrr::map(es_mtx_m, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("`empirical_stats` works on xts data", {
  # dimension
  expect_equal(NCOL(es_xts_u), 3L)
  expect_equal(NROW(es_xts_m), 24L)
  #class
  expect_type(es_xts_u, "list")
  expect_type(es_xts_m, "list")
  expect_s3_class(es_xts_u, "tbl_df")
  expect_s3_class(es_xts_m, "tbl_df")
  expect_equal(
    purrr::map(es_xts_u, class), list(stat = "factor", name = "character", value = "numeric")
  )
  expect_equal(
    purrr::map(es_xts_m, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("`empirical_stats` works on df data", {
  # dimension
  expect_equal(NCOL(es_df_u), 3L)
  expect_equal(NROW(es_df_m), 24L)
  #class
  expect_type(es_df_u, "list")
  expect_type(es_df_m, "list")
  expect_s3_class(es_df_u, "tbl_df")
  expect_s3_class(es_df_m, "tbl_df")
  expect_equal(
    purrr::map(es_df_u, class), list(stat = "factor", name = "character", value = "numeric")
  )
  expect_equal(
    purrr::map(es_df_m, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("`empirical_stats` works on tbl data", {
  # dimension
  expect_equal(NCOL(es_tbl_u), 3L)
  expect_equal(NROW(es_tbl_m), 24L)
  #class
  expect_type(es_tbl_u, "list")
  expect_type(es_tbl_m, "list")
  expect_s3_class(es_tbl_u, "tbl_df")
  expect_s3_class(es_tbl_m, "tbl_df")
  expect_equal(
    purrr::map(es_tbl_u, class), list(stat = "factor", name = "character", value = "numeric")
  )
  expect_equal(
    purrr::map(es_tbl_m, class), list(stat = "factor", name = "character", value = "numeric")
  )
})

test_that("argument level impacts the VaR and CVaR", {
  lvl_0.01 <- empirical_stats(ret_mtx[ , 1], p_mtx, 0.01) %>%
    filter(stat == "VaR") %>%
    pull(value)
  lvl_0.05 <- empirical_stats(ret_mtx[ , 1], p_mtx, 0.05) %>%
    filter(stat == "VaR") %>%
    pull(value)
  lvl_0.10 <- empirical_stats(ret_mtx[ , 1], p_mtx, 0.10) %>%
    filter(stat == "VaR") %>%
    pull(value)

  expect_true(lvl_0.01 > lvl_0.05)
  expect_true(lvl_0.05 > lvl_0.10)

})

