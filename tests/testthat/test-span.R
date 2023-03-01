# ------------------------------------------------------------------------------
# iv_pairwise_span()

test_that("can take the pairwise span", {
  x <- iv(1, 3)
  y <- iv(2, 4)

  expect_identical(
    iv_pairwise_span(x, y),
    iv(1, 4)
  )

  y <- iv(3, 4)

  expect_identical(
    iv_pairwise_span(x, y),
    iv(1, 4)
  )
})

test_that("merges across gaps", {
  x <- iv(1, 3)
  y <- iv(4, 5)

  expect_identical(iv_pairwise_span(x, y), iv(1, 5))
  expect_identical(iv_pairwise_span(y, x), iv(1, 5))
})

test_that("pairwise span propagates NAs", {
  x <- iv(c(0, NA), c(2, NA))
  y <- iv(1, 4)

  expect_identical(
    iv_pairwise_span(x, y),
    iv(c(0, NA), c(4, NA))
  )
  expect_identical(
    iv_pairwise_span(y, x),
    iv(c(0, NA), c(4, NA))
  )
})

test_that("pairwise span is generic over container", {
  x <- nested_integer_iv(1, 2)
  y <- nested_integer_iv(2, 3)
  expect_identical(iv_pairwise_span(x, y), nested_integer_iv(1, 3))
})
