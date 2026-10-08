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
xu_tbl <- tibble::tibble(
  index = index,
  x = xu
)

# Multivariate
xm_vec <- xm
xm_mat <- as.matrix(xm)
xm_ts <- stats::as.ts(xm)
xm_xts <- xts::xts(xm, index)
xm_df <- as.data.frame(xm)
xm_tbl <- tibble::tibble(
  index = index,
  x = xm
)


# condition must be specified ---------------------------------------------

test_that("condition must specified", {
  expect_error(
    crisp(xu)
  )
})

test_that("error if condition is not logical", {
  expect_error(
    crisp(
      xu,
      as.data.frame(cond)
    )
  )

  expect_error(
    crisp(
      xu,
      as.matrix(cond)
    )
  )
})

test_that("error if sizes differ", {
  expect_error(
    crisp(
      c(xu, stats::rnorm(2)),
      cond
    )
  )
})


# works on different classes ----------------------------------------------

# doubles
crisp_numeric_u <- crisp(
  xu,
  cond
)

test_that("works on univariate doubles", {
  expect_type(
    crisp_numeric_u,
    "double"
  )

  expect_s3_class(
    crisp_numeric_u,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_numeric_u),
    vctrs::vec_size(xu)
  )
})


# matrices
crisp_matu <- crisp(
  xu_mat,
  cond
)

test_that("works on univariate matrices", {
  expect_type(
    crisp_matu,
    "double"
  )

  expect_s3_class(
    crisp_matu,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_matu),
    vctrs::vec_size(xu_mat)
  )
})

crisp_matm <- crisp(
  xm_mat,
  cond
)

test_that("works on multivariate matrices", {
  expect_type(
    crisp_matm,
    "double"
  )

  expect_s3_class(
    crisp_matm,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_matm),
    vctrs::vec_size(xm_mat)
  )
})


# ts
crisp_tsu <- crisp(
  xu_ts,
  cond
)

test_that("works on univariate ts", {
  expect_type(
    crisp_tsu,
    "double"
  )

  expect_s3_class(
    crisp_tsu,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_tsu),
    vctrs::vec_size(xu_ts)
  )
})

crisp_tsm <- crisp(
  xm_ts,
  cond
)

test_that("works on multivariate ts", {
  expect_type(
    crisp_tsm,
    "double"
  )

  expect_s3_class(
    crisp_tsm,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_tsm),
    vctrs::vec_size(xm_ts)
  )
})


# xts
crisp_xtsu <- crisp(
  xu_xts,
  cond
)

test_that("works on univariate xts", {
  expect_type(
    crisp_xtsu,
    "double"
  )

  expect_s3_class(
    crisp_xtsu,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_xtsu),
    vctrs::vec_size(xu_xts)
  )
})

crisp_xtsm <- crisp(
  xm_xts,
  cond
)

test_that("works on multivariate xts", {
  expect_type(
    crisp_xtsm,
    "double"
  )

  expect_s3_class(
    crisp_xtsm,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_xtsm),
    vctrs::vec_size(xm_xts)
  )
})


# data.frame
crisp_dfu <- crisp(
  xu_df,
  cond
)

test_that("works on univariate data.frames", {
  expect_type(
    crisp_dfu,
    "double"
  )

  expect_s3_class(
    crisp_dfu,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_dfu),
    vctrs::vec_size(xu_df)
  )
})

crisp_dfm <- crisp(
  xm_df,
  cond
)

test_that("works on multivariate data.frames", {
  expect_type(
    crisp_dfm,
    "double"
  )

  expect_s3_class(
    crisp_dfm,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_dfm),
    vctrs::vec_size(xm_df)
  )
})


# tbl
crisp_tblu <- crisp(
  xu_tbl,
  cond
)

test_that("works on univariate tibbles", {
  expect_type(
    crisp_tblu,
    "double"
  )

  expect_s3_class(
    crisp_tblu,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_tblu),
    vctrs::vec_size(xu_tbl)
  )
})

crisp_tblm <- crisp(
  xm_tbl,
  cond
)

test_that("works on multivariate tibbles", {
  expect_type(
    crisp_tblm,
    "double"
  )

  expect_s3_class(
    crisp_tblm,
    "ffp"
  )

  expect_equal(
    vctrs::vec_size(crisp_tblm),
    vctrs::vec_size(xm_tbl)
  )
})


# Identical results -------------------------------------------------------

test_that("results are identical and don't depend on the class", {
  expect_identical(
    as.double(crisp_numeric_u),
    as.double(crisp_matu)
  )

  expect_identical(
    as.double(crisp_tsu),
    as.double(crisp_tblu)
  )

  expect_identical(
    as.double(crisp_matm),
    as.double(crisp_xtsm)
  )

  expect_identical(
    as.double(crisp_tsm),
    as.double(crisp_tblm)
  )
})


# FFP 2.0 core equivalence ------------------------------------------------

test_that("legacy crisp matches the FFP 2.0 core", {
  expected <- crisp_conditioning_probabilities(
    cond
  )

  expect_identical(
    as.double(crisp_numeric_u),
    expected
  )
})


test_that("crisp preserves structural zeros outside the conditioning set", {
  x <- c(
    -2,
    -1,
    0,
    1,
    2
  )

  condition <- x > 0

  result <- crisp(
    x,
    condition
  )

  expect_identical(
    as.double(result),
    c(
      0,
      0,
      0,
      0.5,
      0.5
    )
  )

  expect_identical(
    which(as.double(result) == 0),
    which(!condition)
  )
})


test_that("crisp rejects an empty conditioning set", {
  expect_error(
    crisp(
      xu,
      rep(FALSE, length(xu))
    ),
    class = "ffp_error_empty_conditioning_set"
  )
})


test_that("crisp rejects missing conditioning values", {
  condition <- cond
  condition[[1]] <- NA

  expect_error(
    crisp(
      xu,
      condition
    ),
    class = "ffp_error_invalid_prior_condition"
  )
})


# Legacy metadata ---------------------------------------------------------

test_that("crisp preserves legacy metadata", {
  result <- crisp(
    xu,
    cond
  )

  expect_identical(
    attr(
      result,
      "fn",
      exact = TRUE
    ),
    "crisp"
  )

  user_call <- attr(
    result,
    "user_call",
    exact = TRUE
  )

  expect_true(
    is.call(user_call)
  )

  expect_match(
    paste(
      deparse(user_call),
      collapse = ""
    ),
    "crisp"
  )
})


test_that("crisp remains compatible with bind_probs", {
  result <- crisp(
    xu,
    cond
  )

  bound <- bind_probs(
    result
  )

  expect_s3_class(
    bound,
    "tbl_df"
  )

  expect_equal(
    bound$probs,
    as.double(result)
  )

  expect_true(
    all(
      grepl(
        "crisp",
        as.character(bound$fn),
        fixed = TRUE
      )
    )
  )
})
