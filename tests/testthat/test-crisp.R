set.seed(123)
# data
xu <- stats::rnorm(50)
xm <- matrix(stats::rnorm(100), ncol = 2)
index <- seq(Sys.Date(), Sys.Date() + 49, "day")
cond <- xu < 0

# Univariate
xu_vec <- xu
xu_mat <- as.matrix(xu)
xu_ts <- stats::as.ts(xu)
xu_xts <- xts::xts(xu, index)
xu_df <- as.data.frame(xu)
xu_tbl <- tibble::tibble(index = index, x = xu)

# Multivariate
xm_vec <- xm
xm_mat <- as.matrix(xm)
xm_ts <- stats::as.ts(xm)
xm_xts <- xts::xts(xm, index)
xm_df <- as.data.frame(xm)
xm_tbl <- tibble::tibble(index = index, x = xm)

# condition must be specified ---------------------------------------------

test_that("condition must specified", {
  expect_error(crisp(xu))
})

test_that("error if condition is not logical", {
  expect_error(crisp(xu, as.data.frame(cond)))
  expect_error(crisp(xu, as.matrix(cond)))
})

test_that("error if sizes differ", {
  expect_error(crisp(c(xu, rnorm(2)), cond))
})


# works on different classes ----------------------------------------------

# doubles
crisp_numeric_u <- crisp(xu, cond)
test_that("works on univariate doubles", {
  # type
  expect_type(crisp_numeric_u, "list")
  expect_s3_class(crisp_numeric_u, "tbl_df")
  expect_equal(purrr::map(crisp_numeric_u, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_numeric_u, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_numeric_u), NROW(xu))
  expect_equal(ncol(crisp_numeric_u), 2L)
})


# matrices
crisp_matu <- crisp(xu_mat, cond)
test_that("works on univariate matrices", {
  # type
  expect_type(crisp_matu, "list")
  expect_s3_class(crisp_matu, "tbl_df")
  expect_equal(purrr::map(crisp_matu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_matu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_matu), NROW(xu_mat))
  expect_equal(ncol(crisp_matu), 2L)
})

crisp_matm <- crisp(xm_mat, cond)
test_that("works on multivariate matrices", {
  # type
  expect_type(crisp_matm, "list")
  expect_s3_class(crisp_matm, "tbl_df")
  expect_equal(purrr::map(crisp_matm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_matm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_matm), NROW(xm_mat))
  expect_equal(ncol(crisp_matm), 2L)
})

# ts
crisp_tsu <- crisp(xu_ts, cond)
test_that("works on univariate ts", {
  # type
  expect_type(crisp_tsu, "list")
  expect_s3_class(crisp_tsu, "tbl_df")
  expect_equal(purrr::map(crisp_tsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_tsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_tsu), NROW(xu_ts))
  expect_equal(ncol(crisp_tsu), 2L)
})

crisp_tsm <- crisp(xm_ts, cond)
test_that("works on multivariate ts", {
  # type
  expect_type(crisp_tsm, "list")
  expect_s3_class(crisp_tsm, "tbl_df")
  expect_equal(purrr::map(crisp_tsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_tsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_tsm), NROW(xm_ts))
  expect_equal(ncol(crisp_tsm), 2L)
})

# xts
crisp_xtsu <- crisp(xu_xts, cond)
test_that("works on univariate xts", {
  # type
  expect_type(crisp_xtsu, "list")
  expect_s3_class(crisp_xtsu, "tbl_df")
  expect_equal(purrr::map(crisp_xtsu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_xtsu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_xtsu), NROW(xu_xts))
  expect_equal(ncol(crisp_xtsu), 2L)
})

crisp_xtsm <- crisp(xm_xts, cond)
test_that("works on multivariate xts", {
  # type
  expect_type(crisp_xtsm, "list")
  expect_s3_class(crisp_xtsm, "tbl_df")
  expect_equal(purrr::map(crisp_xtsm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_xtsm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_xtsm), NROW(xm_xts))
  expect_equal(ncol(crisp_xtsm), 2L)
})

# data.frame
crisp_dfu <- crisp(xu_df, cond)
test_that("works on univariate data.frames", {
  # type
  expect_type(crisp_dfu, "list")
  expect_s3_class(crisp_dfu, "tbl_df")
  expect_equal(purrr::map(crisp_dfu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_dfu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_dfu), NROW(xu_df))
  expect_equal(ncol(crisp_dfu), 2L)
})

crisp_dfm <- crisp(xm_df, cond)
test_that("works on multivariate data.frames", {
  # type
  expect_type(crisp_dfm, "list")
  expect_s3_class(crisp_dfm, "tbl_df")
  expect_equal(purrr::map(crisp_dfm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_dfm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_dfm), NROW(xm_df))
  expect_equal(ncol(crisp_dfm), 2L)
})

# tbl
crisp_tblu <- crisp(xu_tbl, cond)
test_that("works on univariate tibbles", {
  # type
  expect_type(crisp_tblu, "list")
  expect_s3_class(crisp_tblu, "tbl_df")
  expect_equal(purrr::map(crisp_tblu, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_tblu, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_tblu), NROW(xu_tbl))
  expect_equal(ncol(crisp_tblu), 2L)
})

crisp_tblm <- crisp(xm_tbl, cond)
test_that("works on multivariate tibbles", {
  # type
  expect_type(crisp_tblm, "list")
  expect_s3_class(crisp_tblm, "tbl_df")
  expect_equal(purrr::map(crisp_tblm, class), list(.rowid = "integer", .p = "numeric"))
  # names
  expect_named(crisp_tblm, c(".rowid", ".p"))
  # size
  expect_equal(nrow(crisp_tblm), NROW(xm_tbl))
  expect_equal(ncol(crisp_tblm), 2L)
})
