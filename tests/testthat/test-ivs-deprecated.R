# ------------------------------------------------------------------------------
# iv_complement()

test_that("`iv_complement()` works but throws a deprecation warning", {
  x <- iv(1, 2)

  expect_snapshot({
    out <- iv_complement(x)
  })
  expect_identical(
    out,
    iv_set_complement(x)
  )
})

# ------------------------------------------------------------------------------
# iv_union()

test_that("`iv_union()` works but throws a deprecation warning", {
  x <- iv(1, 3)
  y <- iv(2, 4)

  expect_snapshot({
    out <- iv_union(x, y)
  })
  expect_identical(
    out,
    iv_set_union(x, y)
  )
})

# ------------------------------------------------------------------------------
# iv_intersect()

test_that("`iv_intersect()` works but throws a deprecation warning", {
  x <- iv(1, 3)
  y <- iv(2, 4)

  expect_snapshot({
    out <- iv_intersect(x, y)
  })
  expect_identical(
    out,
    iv_set_intersect(x, y)
  )
})

# ------------------------------------------------------------------------------
# iv_difference()

test_that("`iv_difference()` works but throws a deprecation warning", {
  x <- iv(1, 3)
  y <- iv(2, 4)

  expect_snapshot({
    out <- iv_difference(x, y)
  })
  expect_identical(
    out,
    iv_set_difference(x, y)
  )
})

# ------------------------------------------------------------------------------
# iv_symmetric_difference()

test_that("`iv_symmetric_difference()` works but throws a deprecation warning", {
  x <- iv(1, 3)
  y <- iv(2, 4)

  expect_snapshot({
    out <- iv_symmetric_difference(x, y)
  })
  expect_identical(
    out,
    iv_set_symmetric_difference(x, y)
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_complement()

test_that("`iv_pairwise_complement()` works but throws a deprecation warning", {
  x <- iv(1, 3)
  y <- iv(5, 6)

  expect_snapshot({
    out <- iv_pairwise_complement(x, y)
  })
  expect_identical(
    out,
    iv_pairwise_set_complement(x, y)
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_union()

test_that("`iv_pairwise_union()` works but throws a deprecation warning", {
  x <- iv(1, 3)
  y <- iv(2, 6)

  expect_snapshot({
    out <- iv_pairwise_union(x, y)
  })
  expect_identical(
    out,
    iv_pairwise_set_union(x, y)
  )
})
