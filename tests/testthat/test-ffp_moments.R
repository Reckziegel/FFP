ret_ts  <- diff(log(EuStockMarkets))
ret_mtx <- matrix(ret_ts, ncol = 4)
ret_xts <- xts::xts(ret_mtx, order.by = Sys.Date() + 1:nrow(ret_mtx))
ret_df  <- as.data.frame(ret_ts)
ret_tbl <- tibble::as_tibble(ret_ts)

p <- rep(1 / nrow(ret_ts), nrow(ret_ts))

ffp_ts <- ffp_moments(x = ret_ts, p = p)
test_that("`ffp_moments` works with ts objects", {
  expect_type(ffp_ts, "list")
  expect_length(ffp_ts, 2L)
  expect_named(ffp_ts, c("mu", "sigma"))
  expect_length(ffp_ts[[1]], 4L)
  expect_equal(ncol(ffp_ts[[2]]), 4L)
  expect_equal(nrow(ffp_ts[[2]]), 4L)
})

ffp_mtx <- ffp_moments(x = ret_mtx, p = p)
test_that("`ffp_moments` works with matrix objects", {
  expect_type(ffp_mtx, "list")
  expect_length(ffp_mtx, 2L)
  expect_named(ffp_mtx, c("mu", "sigma"))
  expect_length(ffp_mtx[[1]], 4L)
  expect_equal(ncol(ffp_mtx[[2]]), 4L)
  expect_equal(nrow(ffp_mtx[[2]]), 4L)
})

ffp_xts <- ffp_moments(x = ret_xts, p = p)
test_that("`ffp_moments` works with xts objects", {
  expect_type(ffp_xts, "list")
  expect_length(ffp_xts, 2L)
  expect_named(ffp_xts, c("mu", "sigma"))
  expect_length(ffp_xts[[1]], 4L)
  expect_equal(ncol(ffp_xts[[2]]), 4L)
  expect_equal(nrow(ffp_xts[[2]]), 4L)
})

ffp_df <- ffp_moments(x = ret_df, p = p)
test_that("`ffp_moments` works with data.frame objects", {
  expect_type(ffp_df, "list")
  expect_length(ffp_df, 2L)
  expect_named(ffp_df, c("mu", "sigma"))
  expect_length(ffp_df[[1]], 4L)
  expect_equal(ncol(ffp_df[[2]]), 4L)
  expect_equal(nrow(ffp_df[[2]]), 4L)
})

ffp_tbl <- ffp_moments(x = ret_tbl, p = p)
test_that("`ffp_moments` works with tibble objects", {
  expect_type(ffp_tbl, "list")
  expect_length(ffp_tbl, 2L)
  expect_named(ffp_tbl, c("mu", "sigma"))
  expect_length(ffp_tbl[[1]], 4L)
  expect_equal(ncol(ffp_tbl[[2]]), 4L)
  expect_equal(nrow(ffp_tbl[[2]]), 4L)
})
