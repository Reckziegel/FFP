test_that("ens handles zero-probability scenarios", {
  p <- as_ffp(
    c(
      0.5,
      0.5,
      0
    )
  )

  expect_equal(
    ens(p),
    2
  )
})


test_that("ens equals the number of equally weighted scenarios", {
  p <- as_ffp(
    rep(
      1 / 10,
      10
    )
  )

  expect_equal(
    ens(p),
    10
  )
})


test_that("ens is one for a fully concentrated distribution", {
  p <- as_ffp(
    c(
      1,
      0,
      0
    )
  )

  expect_equal(
    ens(p),
    1
  )
})
