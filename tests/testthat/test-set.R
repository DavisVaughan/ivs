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

# ------------------------------------------------------------------------------
# iv_set_union()

test_that("set union doesn't keep abutting separate", {
  x <- iv(start = c(1L, 2L), end = c(2L, 3L))
  y <- iv(start = 5L, end = 6L)

  expect_identical(
    iv_set_union(x, y),
    iv(start = c(1L, 5L), end = c(3L, 6L))
  )
})

test_that("union treats intervals as half open `[a, b)`", {
  x <- iv(start = 1L, end = 2L)
  y <- iv(start = 3L, end = 5L)

  expect_identical(
    iv_set_union(x, y),
    iv(start = c(1L, 3L), end = c(2L, 5L))
  )
})

test_that("union keeps a single missing interval", {
  x <- iv(c(1, NA, NA), c(2, NA, NA))
  y <- iv(2, 3)

  expect_identical(
    iv_set_union(x, y),
    iv_pairs(c(1, 3), c(NA, NA))
  )
  expect_identical(
    iv_set_union(y, x),
    iv_pairs(c(1, 3), c(NA, NA))
  )
})

test_that("union is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3))
  y <- nested_integer_iv_pairs(c(2, 5))
  expect_identical(iv_set_union(x, y), nested_integer_iv_pairs(c(1, 5)))
})

# ------------------------------------------------------------------------------
# iv_set_intersect()

test_that("set intersect doesn't keep abutting", {
  x <- iv(start = c(1L, 2L), end = c(2L, 3L))

  expect_identical(
    iv_set_intersect(x, x),
    iv(start = 1L, end = 3L)
  )
})

test_that("intersect works", {
  x <- iv(start = c(1L, 6L), end = c(4L, 8L))
  y <- iv(start = 2L, end = 3L)
  z <- iv(start = 3L, end = 7L)

  expect_identical(
    iv_set_intersect(x, y),
    iv(start = 2L, end = 3L)
  )
  expect_identical(
    iv_set_intersect(x, z),
    iv(start = c(3L, 6L), end = c(4L, 7L))
  )
})

test_that("intersect works with size zero inputs", {
  x <- iv(start = integer(), end = integer())
  expect_identical(iv_set_intersect(x, x), x)

  y <- iv(start = 1L, end = 2L)
  expect_identical(iv_set_intersect(x, y), x)
  expect_identical(iv_set_intersect(y, x), x)
})

test_that("intersect keeps a single missing interval", {
  x <- iv(c(NA, 0, NA), c(NA, 2, NA))
  y <- iv(c(1, NA), c(4, NA))

  expect_identical(
    iv_set_intersect(x, y),
    iv_pairs(c(1, 2), c(NA, NA))
  )
  expect_identical(
    iv_set_intersect(y, x),
    iv_pairs(c(1, 2), c(NA, NA))
  )
})

test_that("intersect works when all of one input is missing", {
  x <- iv(c(NA, NA), c(NA, NA))
  y <- iv(c(NA, 0, NA), c(NA, 2, NA))
  z <- iv(0, 2)

  expect_identical(
    iv_set_intersect(x, y),
    iv(NA_real_, NA_real_)
  )
  expect_identical(
    iv_set_intersect(y, x),
    iv(NA_real_, NA_real_)
  )

  expect_identical(
    iv_set_intersect(x, z),
    iv(double(), double())
  )
  expect_identical(
    iv_set_intersect(z, x),
    iv(double(), double())
  )
})

test_that("takes ptype on early exits", {
  x <- iv(integer(), integer())
  y <- iv(c(1L, 3L), c(3L, 4L))

  expect_identical(iv_set_intersect(x, y), x)
  expect_identical(iv_set_intersect(y, x), x)

  z <- iv(NA_integer_, NA_integer_)

  expect_identical(iv_set_intersect(z, y), x)
  expect_identical(iv_set_intersect(y, z), x)
})

test_that("intersect is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3))
  y <- nested_integer_iv_pairs(c(2, 3))
  expect_identical(iv_set_intersect(x, y), nested_integer_iv_pairs(c(2, 3)))
})

# ------------------------------------------------------------------------------
# iv_set_difference()

test_that("difference doesn't keep abutting separate", {
  x <- iv(start = c(1L, 2L), end = c(2L, 3L))
  y <- iv(start = integer(), end = integer())

  expect_identical(
    iv_set_difference(x, y),
    iv(start = 1L, end = 3L)
  )
})

test_that("difference works", {
  x <- iv(start = c(1L, 6L), end = c(4L, 8L))
  y <- iv(start = 2L, end = 3L)
  z <- iv(start = 3L, end = 7L)

  expect_identical(
    iv_set_difference(x, y),
    iv(start = c(1L, 3L, 6L), end = c(2L, 4L, 8L))
  )
  expect_identical(
    iv_set_difference(x, z),
    iv(start = c(1L, 7L), end = c(3L, 8L))
  )
})

test_that("difference works with size zero inputs", {
  x <- iv(start = integer(), end = integer())
  expect_identical(iv_set_difference(x, x), x)

  y <- iv(start = 1L, end = 2L)
  expect_identical(iv_set_difference(x, y), x)
  expect_identical(iv_set_difference(y, x), y)
})

test_that("difference keeps a single missing interval when applicable", {
  x <- iv(c(NA, 0, NA), c(NA, 2, NA))
  y <- iv(c(1, NA), c(4, NA))
  z <- iv(1, 4)

  expect_identical(
    iv_set_difference(x, y),
    iv_pairs(c(0, 1))
  )
  expect_identical(
    iv_set_difference(y, x),
    iv_pairs(c(2, 4))
  )

  expect_identical(
    iv_set_difference(x, z),
    iv_pairs(c(0, 1), c(NA, NA))
  )
  expect_identical(
    iv_set_difference(z, x),
    iv_pairs(c(2, 4))
  )
})

test_that("difference works when all of one input is missing", {
  x <- iv(c(NA, NA), c(NA, NA))
  y <- iv(c(NA, 0, NA), c(NA, 2, NA))
  z <- iv(0, 2)

  expect_identical(
    iv_set_difference(x, y),
    iv(double(), double())
  )
  expect_identical(
    iv_set_difference(y, x),
    iv(0, 2)
  )

  expect_identical(
    iv_set_difference(x, z),
    iv(NA_real_, NA_real_)
  )
  expect_identical(
    iv_set_difference(z, x),
    iv(0, 2)
  )
})

test_that("minimizes on early exits", {
  x <- iv(integer(), integer())
  y <- iv(c(1L, 3L), c(3L, 4L))

  expect_identical(iv_set_difference(x, y), x)
  expect_identical(iv_set_difference(y, x), iv(1L, 4L))

  z <- iv(NA_integer_, NA_integer_)

  expect_identical(iv_set_difference(z, y), z)
  expect_identical(iv_set_difference(y, z), iv(1L, 4L))
})

test_that("difference is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3))
  y <- nested_integer_iv_pairs(c(2, 3))
  expect_identical(iv_set_difference(x, y), nested_integer_iv_pairs(c(1, 2)))
})

# ------------------------------------------------------------------------------
# iv_symmetric_difference()

test_that("symmetric difference works with size zero inputs", {
  x <- iv(start = integer(), end = integer())
  expect_identical(iv_symmetric_difference(x, x), x)

  y <- iv(start = 1L, end = 2L)
  expect_identical(iv_symmetric_difference(x, y), y)
  expect_identical(iv_symmetric_difference(y, x), y)
})

test_that("minimizes on early exits", {
  x <- iv(integer(), integer())
  y <- iv(c(1L, 3L), c(3L, 4L))

  expect_identical(iv_symmetric_difference(x, y), iv(1L, 4L))
  expect_identical(iv_symmetric_difference(y, x), iv(1L, 4L))

  z <- iv(NA_integer_, NA_integer_)

  expect_identical(iv_symmetric_difference(z, y), iv_pairs(c(1L, 4L), c(NA, NA)))
  expect_identical(iv_symmetric_difference(y, z), iv_pairs(c(1L, 4L), c(NA, NA)))
})

test_that("symmetric difference performs xor", {
  x <- iv(c(3, 0, 7), c(8, 2, 10))
  y <- iv(c(1, 8), c(5, 12))

  expect_identical(
    iv_symmetric_difference(x, y),
    iv(c(0, 2, 5, 10), c(1, 3, 8, 12))
  )
})

test_that("symmetric difference keeps a single missing interval when applicable", {
  x <- iv(c(NA, 0, NA), c(NA, 2, NA))
  y <- iv(c(1, NA), c(4, NA))
  z <- iv(1, 4)

  expect_identical(
    iv_symmetric_difference(x, y),
    iv_pairs(c(0, 1), c(2, 4))
  )
  expect_identical(
    iv_symmetric_difference(x, y),
    iv_symmetric_difference(y, x)
  )

  expect_identical(
    iv_symmetric_difference(x, z),
    iv_pairs(c(0, 1), c(2, 4), c(NA, NA))
  )
  expect_identical(
    iv_symmetric_difference(x, z),
    iv_symmetric_difference(z, x)
  )
})

test_that("symmetric difference works when all of one input is missing", {
  x <- iv(c(NA, NA), c(NA, NA))
  y <- iv(c(NA, 0, NA), c(NA, 2, NA))
  z <- iv(0, 2)

  expect_identical(
    iv_symmetric_difference(x, y),
    iv(0, 2)
  )
  expect_identical(
    iv_symmetric_difference(x, y),
    iv_symmetric_difference(y, x)
  )

  expect_identical(
    iv_symmetric_difference(x, z),
    iv_pairs(c(0, 2), c(NA, NA))
  )
  expect_identical(
    iv_symmetric_difference(x, z),
    iv_symmetric_difference(z, x)
  )
})

test_that("symmetric difference is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3))
  y <- nested_integer_iv_pairs(c(2, 5))
  expect_identical(
    iv_symmetric_difference(x, y),
    nested_integer_iv_pairs(c(1, 2), c(3, 5))
  )
})
