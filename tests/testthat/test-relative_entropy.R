prior <- rep(1 / 100, 100)
posterior <- runif(100)
posterior <- posterior / sum(posterior)
re <- relative_entropy(prior, posterior)

test_that("multiplication works", {
  expect_type(re, "double")
  expect_length(re, 1L)
  expect_gt(re, 0)

  expect_error(relative_entropy(prior, c(prior, prior)))
  expect_error(relative_entropy(prior, runif(100)))

})
