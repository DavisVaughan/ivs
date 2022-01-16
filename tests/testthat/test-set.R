# ------------------------------------------------------------------------------
# iv_set_complement()

# Most of the tests for this are in vctrs

test_that("complement is working", {
  x <- new_iv(c(10L, 0L, 3L), c(12L, 5L, 6L))

  expect_identical(iv_set_complement(x), new_iv(6L, 10L))

  expect_identical(
    iv_set_complement(x, lower = -1L, upper = 15L),
    new_iv(c(-1L, 6L, 12L), c(0L, 10L, 15L))
  )
})

test_that("complement is generic", {
  x <- new_nested_integer_iv(new_iv(start = c(1L, 5L), end = c(3L, 7L)))

  expect_identical(
    iv_set_complement(x),
    new_nested_integer_iv(new_iv(3L, 5L))
  )
})
