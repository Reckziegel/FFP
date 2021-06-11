hl <- half_life(0.01)
test_that("half_life returns a single numeric value", {
  expect_length(hl, 1L)
  expect_equal(class(hl), "numeric")
})

test_that("half_life throw an error when lambda is not numeric & > 1", {
  expect_error(half_life("1"))
  expect_error(half_life(c(0.01, 0.01)))

})

