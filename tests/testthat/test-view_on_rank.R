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
