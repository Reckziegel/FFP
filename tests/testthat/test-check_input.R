# data <- stats::runif(20)
# index <- seq(Sys.Date(), Sys.Date() + 9, "day")
# # data numeric
# data_dbl <- data
# # matrix data
# data_mtx <- matrix(data, ncol = 2)
# # data xts
# data_xts <- xts::xts(matrix(data, ncol = 2), order.by = index)
# # data data.frame
# data_df <- data.frame(matrix(data, ncol = 2))
# # data tbl
# data_tbl <- tibble::tibble(index = index, a = data[1:10], b = data[11:20])
#
# test_that("check_input objects into a matrix", {
#   # dbl
#   expect_type(check_input(data_dbl), "double")
#   expect_length(check_input(data_tbl), 20L)
#   # mtx
#   expect_type(check_input(data_mtx), "double")
#   expect_equal(nrow(check_input(data_mtx)), 10L)
#   expect_equal(ncol(check_input(data_mtx)), 2L)
#   # xts
#   expect_type(check_input(data_xts), "double")
#   expect_equal(nrow(check_input(data_xts)), 10L)
#   expect_equal(ncol(check_input(data_xts)), 2L)
#   # data.frame
#   expect_type(check_input(data_df), "double")
#   expect_equal(nrow(check_input(data_df)), 10L)
#   expect_equal(ncol(check_input(data_df)), 2L)
#   # tbl
#   expect_type(check_input(data_tbl), "double")
#   expect_equal(nrow(check_input(data_tbl)), 10L)
#   expect_equal(ncol(check_input(data_tbl)), 2L)
# })
#
# test_that("ts throws and error", {
#   expect_error(check_input(AirPassengers))
# })
