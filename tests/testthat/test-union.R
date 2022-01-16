# ------------------------------------------------------------------------------
# iv_union()

# Most tests handled by vctrs

test_that("computes the union", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_union(x), iv(1, 5))
})

test_that("can keep abutting intervals separate", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_union(x, keep_abutting = TRUE), x)
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  expect_identical(iv_union(x), x)
})

test_that("union is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 7))

  expect_identical(
    iv_union(x),
    nested_integer_iv_pairs(c(1, 7))
  )
})

# ------------------------------------------------------------------------------
# iv_replace_union()

test_that("computes the order on the group locations to map back to original locations", {
  x <- iv_pairs(
    c(1, 5),
    c(11, 14),
    c(5, 6),
    c(9, 12)
  )

  expect <- iv_pairs(
    c(1, 6),
    c(9, 14),
    c(1, 6),
    c(9, 14)
  )

  expect_identical(iv_replace_union(x), expect)
})

test_that("replace union is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 7))

  expect_identical(
    iv_replace_union(x),
    nested_integer_iv_pairs(c(1, 7), c(1, 7))
  )
})

# ------------------------------------------------------------------------------
# iv_locate_union()

# Most tests handled by vctrs

test_that("locates the union", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_locate_union(x), data_frame(start = 1L, end = 2L))
})

test_that("can keep abutting intervals separate", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_locate_union(x, keep_abutting = TRUE), data_frame(start = 1:2, end = 1:2))
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  expect_identical(iv_locate_union(x), data_frame(start = c(1L, NA), end = c(1L, NA)))
})

# ------------------------------------------------------------------------------
# iv_locate_union_groups()

# Most tests handled by vctrs

test_that("locates the union", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  out <- iv_locate_union_groups(x)
  expect_identical(out$key, data_frame(start = 1L, end = 2L))
  expect_identical(out$loc, list(1:2))
})

test_that("can keep abutting intervals separate", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  out <- iv_locate_union_groups(x, keep_abutting = TRUE)
  expect_identical(out$key, data_frame(start = 1:2, end = 1:2))
  expect_identical(out$loc, list(1L, 2L))
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  out <- iv_locate_union_groups(x)
  expect_identical(out$key, data_frame(start = c(1L, NA), end = c(1L, NA)))
  expect_identical(out$loc, list(1L, 2L))
})
