# ------------------------------------------------------------------------------
# iv_merge()

# Most tests handled by vctrs

test_that("computes the merge", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_merge(x), iv(1, 5))
})

test_that("can choose not to merge abutting", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_merge(x, abutting = FALSE), x)
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  expect_identical(iv_merge(x), x)
})

test_that("merge is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 7))

  expect_identical(
    iv_merge(x),
    nested_integer_iv_pairs(c(1, 7))
  )
})

# ------------------------------------------------------------------------------
# iv_replace_merged()

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

  expect_identical(iv_replace_merged(x), expect)
})

test_that("replace merge is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 7))

  expect_identical(
    iv_replace_merged(x),
    nested_integer_iv_pairs(c(1, 7), c(1, 7))
  )
})

# ------------------------------------------------------------------------------
# iv_locate_merge_bounds()

# Most tests handled by vctrs

test_that("locates the merge bounds", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_locate_merge_bounds(x), data_frame(start = 1L, end = 2L))
})

test_that("can choose not to merge abutting intervals", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_locate_merge_bounds(x, abutting = FALSE), data_frame(start = 1:2, end = 1:2))
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  expect_identical(iv_locate_merge_bounds(x), data_frame(start = c(1L, NA), end = c(1L, NA)))
})

# ------------------------------------------------------------------------------
# iv_locate_merge_groups()

# Most tests handled by vctrs

test_that("locates the merge groups", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  out <- iv_locate_merge_groups(x)
  expect_identical(out$key, data_frame(start = 1L, end = 2L))
  expect_identical(out$loc, list(1:2))
})

test_that("can choose not to merge abutting intervals", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  out <- iv_locate_merge_groups(x, abutting = FALSE)
  expect_identical(out$key, data_frame(start = 1:2, end = 1:2))
  expect_identical(out$loc, list(1L, 2L))
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  out <- iv_locate_merge_groups(x)
  expect_identical(out$key, data_frame(start = c(1L, NA), end = c(1L, NA)))
  expect_identical(out$loc, list(1L, 2L))
})
