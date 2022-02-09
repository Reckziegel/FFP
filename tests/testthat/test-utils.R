stocks <- tibble::tibble(
  time = Sys.Date() + 0:4,
  X    = rnorm(5, 0, 1),
  Y    = rnorm(5, 0, 2),
  Z    = rnorm(5, 0, 4)
)

test_that("`any_is_date` works", {
  expect_true(any_is_date(stocks))
})


test_that("`any_is_double` works", {
  expect_true(any_is_double(stocks))
})

test_that("`which_is_date` works", {
  expect_equal(which_is_date(stocks), 1L)
})

test_that("`get_date_col` works", {
  expect_equal(ncol(get_date_col(stocks)), 1L)
  expect_s3_class(get_date_col(stocks), "tbl_df")
  expect_named(get_date_col(stocks), "time")
})

test_that("`get_double_col` works", {
  expect_equal(NCOL(get_double_col(stocks)), 3L)
})

test_that("`has_dim` works", {
  expect_true(has_dim(stocks))
})

test_that("`has_dim` works", {
  expect_true(has_dim(stocks))
})

test_that("`tbl_to_mtx` works", {
  expect_equal(ncol(tbl_to_mtx(stocks)), 3L)
  expect_type(tbl_to_mtx(stocks), "double")
})

test_that("`ffp_match_call` works", {
  expect_equal(class(ffp_match_call(mean(x))), "call")
})
