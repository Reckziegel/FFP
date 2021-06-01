# data
set.seed(123)
x <- stats::rnorm(50)
index <- seq(Sys.Date(), Sys.Date() + 49, "day")
cond <- x < 0

# classes
x_vec <- x
x_mat <- as.matrix(x)
x_ts <- stats::as.ts(x)
x_xts <- xts::xts(x, index)
x_df <- as.data.frame(x)
x_tbl <- tibble::tibble(index = index, x = x)

# condition must be specified ---------------------------------------------

test_that("condition must specified", {
  expect_error(crisp(x))
})

test_that("error if condition is not logical", {
  expect_error(crisp(x, as.data.frame(cond)))
  expect_error(crisp(x, as.matrix(cond)))
})

test_that("error if sizes differ", {
  expect_error(crisp(c(x, rnorm(2)), cond))
})

# test_that("error if x is multivariate", {
#   expect_error(crisp(cbind(x, x), cond))
# })


# works on different classes ----------------------------------------------

# doubles
crisp_numeric <- crisp(x, cond)
test_that("works on doubles", {
  expect_type(crisp_numeric, "double")
  expect_length(crisp_numeric, length(x))
})


# matrices
crisp_mat <- crisp(x_mat, cond)
test_that("works on matrices", {
  expect_type(crisp_mat, "double")
  expect_length(crisp_mat, length(x))
})

# ts
crisp_ts <- crisp(x_ts, cond)
test_that("works on ts", {
  expect_type(crisp_ts, "double")
  expect_length(crisp_ts, length(x))
})

# xts
crisp_xts <- crisp(x_xts, cond)
test_that("works on xts", {
  expect_type(crisp_xts, "double")
  expect_length(crisp_xts, length(x))
})

# data.frame
crisp_df <- crisp(x_df, cond)
test_that("works on data.frames", {
  expect_type(crisp_df, "double")
  expect_length(crisp_df, length(x))
})

# tbl
crisp_tbl <- crisp(x_tbl, cond)
test_that("works on tibbles", {
  expect_type(crisp_tbl, "double")
  expect_length(crisp_tbl, length(x))
})
