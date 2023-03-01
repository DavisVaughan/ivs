# ------------------------------------------------------------------------------
# iv_pairwise_set_complement()

test_that("can pairwise complement", {
  expect_identical(
    iv_pairwise_set_complement(iv(1, 2), iv(5, 6)),
    iv(2, 5)
  )

  expect_identical(
    iv_pairwise_set_complement(iv(1, 2), iv(-1, 0)),
    iv(0, 1)
  )
})

test_that("pairwise complement propagates NAs", {
  x <- iv(c(0, NA), c(2, NA))
  y <- iv(4, 5)

  expect_identical(
    iv_pairwise_set_complement(x, y),
    iv(c(2, NA), c(4, NA))
  )
  expect_identical(
    iv_pairwise_set_complement(y, x),
    iv(c(2, NA), c(4, NA))
  )
})

test_that("pairwise complement of interval with itself is not allowed", {
  x <- iv(1, 2)
  expect_snapshot((expect_error(iv_pairwise_set_complement(x, x))))
})

test_that("pairwise complement of abutting intervals is not allowed", {
  x <- iv(1, 2)

  y <- iv(0, 1)
  expect_snapshot((expect_error(iv_pairwise_set_complement(x, y))))

  y <- iv(2, 3)
  expect_snapshot((expect_error(iv_pairwise_set_complement(x, y))))
})

test_that("pairwise complement of overlapping intervals is not allowed", {
  x <- iv(1, 3)

  expect_snapshot(
    (expect_error(iv_pairwise_set_complement(x, x)))
  )

  y <- iv(0, 4)

  expect_snapshot({
    (expect_error(iv_pairwise_set_complement(x, y)))
    (expect_error(iv_pairwise_set_complement(y, x)))
  })

  y <- iv(2, 4)

  expect_snapshot({
    (expect_error(iv_pairwise_set_complement(x, y)))
    (expect_error(iv_pairwise_set_complement(y, x)))
  })
})

test_that("pairwise complement is generic over container", {
  x <- nested_integer_iv(1, 3)
  y <- nested_integer_iv(-1, 0)
  expect_identical(iv_pairwise_set_complement(x, y), nested_integer_iv(0, 1))
})

# ------------------------------------------------------------------------------
# iv_pairwise_set_union()

test_that("can take the pairwise union", {
  x <- iv(1, 3)
  y <- iv(2, 4)

  expect_identical(
    iv_pairwise_set_union(x, y),
    iv(1, 4)
  )

  y <- iv(3, 4)

  expect_identical(
    iv_pairwise_set_union(x, y),
    iv(1, 4)
  )
})

test_that("errors on gaps", {
  x <- iv(1, 3)
  y <- iv(4, 5)

  expect_snapshot((expect_error(iv_pairwise_set_union(x, y))))

  expect_snapshot((expect_error(iv_pairwise_set_union(y, x))))
})

test_that("pairwise union propagates NAs", {
  x <- iv(c(0, NA), c(2, NA))
  y <- iv(1, 4)

  expect_identical(
    iv_pairwise_set_union(x, y),
    iv(c(0, NA), c(4, NA))
  )
  expect_identical(
    iv_pairwise_set_union(y, x),
    iv(c(0, NA), c(4, NA))
  )
})

test_that("pairwise union is generic over container", {
  x <- nested_integer_iv(1, 2)
  y <- nested_integer_iv(2, 3)
  expect_identical(iv_pairwise_set_union(x, y), nested_integer_iv(1, 3))
})

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

# ------------------------------------------------------------------------------
# iv_pairwise_set_intersect()

test_that("can take pairwise intersection", {
  x <- iv(start = 1L, end = 4L)
  y <- iv(start = 0L, end = 3L)

  expect_identical(
    iv_pairwise_set_intersect(x, y),
    iv(start = 1L, end = 3L)
  )
})

test_that("can recycle inputs", {
  x <- iv(start = c(1L, 2L), end = c(4L, 5L))
  y <- iv(start = 0L, end = 3L)

  expect_identical(
    iv_pairwise_set_intersect(x, y),
    iv(start = c(1L, 2L), end = c(3L, 3L))
  )
})

test_that("pairwise intersection between non-overlapping intervals errors", {
  x <- iv(start = 1L, end = 4L)

  y <- iv(start = 5L, end = 6L)

  expect_snapshot(
    (expect_error(iv_pairwise_set_intersect(x, y)))
  )

  y <- iv(start = -1L, end = 0L)

  expect_snapshot(
    (expect_error(iv_pairwise_set_intersect(x, y)))
  )

  y <- iv(start = 4L, end = 5L)

  expect_snapshot(
    (expect_error(iv_pairwise_set_intersect(x, y)))
  )

  y <- iv(start = 0L, end = 1L)

  expect_snapshot(
    (expect_error(iv_pairwise_set_intersect(x, y)))
  )
})

test_that("pairwise intersection propagates NAs", {
  x <- iv(c(0, NA), c(2, NA))
  y <- iv(1, 4)

  expect_identical(
    iv_pairwise_set_intersect(x, y),
    iv(c(1, NA), c(2, NA))
  )
  expect_identical(
    iv_pairwise_set_intersect(y, x),
    iv(c(1, NA), c(2, NA))
  )
})

test_that("pairwise intersect is generic over container", {
  x <- nested_integer_iv(1, 3)
  y <- nested_integer_iv(2, 3)
  expect_identical(iv_pairwise_set_intersect(x, y), nested_integer_iv(2, 3))
})

# ------------------------------------------------------------------------------
# iv_pairwise_set_difference()

test_that("can pairwise difference from all sides of `x`", {
  x <- iv(1, 10)

  y <- iv(-1, 0)
  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(1, 10)
  )

  y <- iv(-1, 1)
  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(1, 10)
  )

  y <- iv(1, 3)
  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(3, 10)
  )

  y <- iv(7, 10)
  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(1, 7)
  )

  y <- iv(10, 12)
  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(1, 10)
  )

  y <- iv(11, 12)
  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(1, 10)
  )
})

test_that("pairwise difference between interval and itself is not allowed", {
  x <- iv(1, 3)
  expect_snapshot((expect_error(iv_pairwise_set_difference(x, x))))
})

test_that("throws error when `y` is contained within `x`", {
  x <- iv(1, 4)
  y <- iv(2, 3)

  expect_snapshot((expect_error(iv_pairwise_set_difference(x, y))))
})

test_that("throws error when `y` contains `x`", {
  x <- iv(2, 3)
  y <- iv(1, 4)

  expect_snapshot((expect_error(iv_pairwise_set_difference(x, y))))
})

test_that("pairwise difference propagates NAs", {
  x <- iv(c(0, NA), c(2, NA))
  y <- iv(1, 4)

  expect_identical(
    iv_pairwise_set_difference(x, y),
    iv(c(0, NA), c(1, NA))
  )
  expect_identical(
    iv_pairwise_set_difference(y, x),
    iv(c(2, NA), c(4, NA))
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_set_symmetric_difference()

test_that("can symmetric pairwise difference when LHS endpoint matches", {
  expect_identical(
    iv_pairwise_set_symmetric_difference(iv(1, 3), iv(1, 5)),
    iv(3, 5)
  )

  expect_identical(
    iv_pairwise_set_symmetric_difference(iv(1, 5), iv(1, 3)),
    iv(3, 5)
  )
})

test_that("can symmetric pairwise difference when RHS endpoint matches", {
  expect_identical(
    iv_pairwise_set_symmetric_difference(iv(1, 5), iv(3, 5)),
    iv(1, 3)
  )

  expect_identical(
    iv_pairwise_set_symmetric_difference(iv(3, 5), iv(1, 5)),
    iv(1, 3)
  )
})

test_that("throws error when neither endpoint matches", {
  expect_snapshot({
    # No overlap
    (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 2), iv(3, 4))))
    (expect_error(iv_pairwise_set_symmetric_difference(iv(3, 4), iv(1, 2))))

    # Cross over
    (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 3), iv(2, 4))))
    (expect_error(iv_pairwise_set_symmetric_difference(iv(2, 4), iv(1, 3))))

    # Contains / Within
    (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 4), iv(2, 3))))
    (expect_error(iv_pairwise_set_symmetric_difference(iv(2, 3), iv(1, 4))))
  })
})

test_that("throws error when both endpoints match", {
  expect_snapshot(
    (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 2), iv(1, 2))))
  )
})

test_that("symmetric pairwise difference propagates NAs", {
  x <- iv(c(0, NA), c(4, NA))
  y <- iv(1, 4)

  expect_identical(
    iv_pairwise_set_symmetric_difference(x, y),
    iv(c(0, NA), c(1, NA))
  )
  expect_identical(
    iv_pairwise_set_symmetric_difference(y, x),
    iv(c(0, NA), c(1, NA))
  )
})
