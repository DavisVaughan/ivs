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
