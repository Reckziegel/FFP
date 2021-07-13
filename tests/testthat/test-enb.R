set.seed(123)
x <- stats::rnorm(100)
lgl <- x > 0
cp <- crisp(x, lgl)
test_that("ens() works", {
  # size
  expect_type(ens(cp), "double")
  expect_length(ens(cp), 1L)
  # bounds
  expect_lte(ens(cp), length(x))
  expect_gte(ens(cp), 1L)
})
