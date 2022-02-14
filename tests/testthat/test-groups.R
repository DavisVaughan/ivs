# ------------------------------------------------------------------------------
# iv_groups()

# Most tests handled by vctrs

test_that("can compute groups", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_groups(x), iv(1, 5))
})

test_that("can choose not to group abutting", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_groups(x, abutting = FALSE), x)
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  expect_identical(iv_groups(x), x)
})

test_that("groups is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 7))

  expect_identical(
    iv_groups(x),
    nested_integer_iv_pairs(c(1, 7))
  )
})

# ------------------------------------------------------------------------------
# iv_identify_group()

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

  expect_identical(iv_identify_group(x), expect)
})

test_that("identify group is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 7))

  expect_identical(
    iv_identify_group(x),
    nested_integer_iv_pairs(c(1, 7), c(1, 7))
  )
})

# ------------------------------------------------------------------------------
# iv_locate_group_bounds()

# Most tests handled by vctrs

test_that("locates the group bounds", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_locate_group_bounds(x), data_frame(start = 1L, end = 2L))
})

test_that("can choose not to group abutting intervals", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  expect_identical(iv_locate_group_bounds(x, abutting = FALSE), data_frame(start = 1:2, end = 1:2))
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  expect_identical(iv_locate_group_bounds(x), data_frame(start = c(1L, 2L), end = c(1L, 2L)))
})

# ------------------------------------------------------------------------------
# iv_locate_groups()

# Most tests handled by vctrs

test_that("locates the groups", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  out <- iv_locate_groups(x)
  expect_identical(out$key, iv(1, 5))
  expect_identical(out$loc, list(1:2))
})

test_that("can choose not to group abutting intervals", {
  x <- iv_pairs(c(1, 3), c(3, 5))
  out <- iv_locate_groups(x, abutting = FALSE)
  expect_identical(out$key, x)
  expect_identical(out$loc, list(1L, 2L))
})

test_that("can retain missing", {
  x <- iv_pairs(c(1, 3), c(NA, NA))
  out <- iv_locate_groups(x)
  expect_identical(out$key, x)
  expect_identical(out$loc, list(1L, 2L))
})
