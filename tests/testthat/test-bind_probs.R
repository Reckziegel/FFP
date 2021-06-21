# x <- smoothing(EuStockMarkets, 0.001)
# y <- smoothing(EuStockMarkets, 0.002)
# z <- list(a = 1)
# p_tidy <- bind_probs(x, y, z)
#
# test_that("bind_probs rejects data that do not belong to `ffp`", {
#   expect_equal(nrow(p_tidy), nrow(x) + nrow(y))
#   expect_error(bind_probs(z, z))
# })
#
# test_that("bind_probs keeps the same n of rows and cols", {
#   expect_equal(ncol(p_tidy), ncol(x) + 1)
#   expect_equal(ncol(p_tidy), ncol(y) + 1)
#
#   expect_s3_class(p_tidy, "ffp")
#   expect_equal(purrr::map(p_tidy, class), list(.rowid = "integer", .p = "numeric", .key = "factor"))
#
# })
#
