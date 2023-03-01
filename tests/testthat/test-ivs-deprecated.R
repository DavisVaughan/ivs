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
