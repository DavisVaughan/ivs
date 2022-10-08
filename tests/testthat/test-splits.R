# ------------------------------------------------------------------------------
# iv_splits()

test_that("split works", {
  x <- iv_pairs(c(1, 7), c(-1, 0), c(2, 3), c(6, 9), c(15, 16))

  expect_identical(
    iv_splits(x),
    iv_pairs(
      c(-1, 0),
      c(1, 2),
      c(2, 3),
      c(3, 6),
      c(6, 7),
      c(7, 9),
      c(15, 16)
    )
  )
})

test_that("split retains a single missing interval", {
  x <- iv_pairs(c(1, 4), c(NA, NA), c(5, 6), c(NA, NA))
  expect_identical(
    iv_splits(x),
    iv_pairs(c(1, 4), c(5, 6), c(NA, NA))
  )
})

test_that("split works with empty iv", {
  x <- iv(integer(), integer())
  expect_identical(iv_splits(x), x)
})

test_that("split works with single interval", {
  x <- iv(1, 2)
  expect_identical(iv_splits(x), x)
})

test_that("split works with single missing interval", {
  x <- iv(NA, NA)
  expect_identical(iv_splits(x), x)
})

test_that("split works with `on`", {
  x <- iv(1, 5)

  expect_identical(
    iv_splits(x, on = 2),
    iv_pairs(c(1, 2), c(2, 5))
  )

  expect_identical(
    iv_splits(x, on = c(2, 4)),
    iv_pairs(c(1, 2), c(2, 4), c(4, 5))
  )
})

test_that("still splits on boundaries in `x` when `on` is also present", {
  x <- iv_pairs(c(1, 5), c(4, 6))

  expect_identical(
    iv_splits(x, on = 2),
    iv_pairs(c(1, 2), c(2, 4), c(4, 5), c(5, 6))
  )
})

test_that("split works if `on` is out of range", {
  x <- iv(1, 5)

  expect_identical(iv_splits(x, on = 0), x)
  expect_identical(iv_splits(x, on = 6), x)
})

test_that("split works if `on` is a missing value", {
  x <- iv(1, 5)

  expect_identical(iv_splits(x, on = NA), x)

  x <- iv(NA, NA)

  expect_identical(iv_splits(x, on = NA), x)
})

test_that("split is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3), c(2, 4))
  expect_identical(
    iv_splits(x),
    nested_integer_iv_pairs(c(1, 2), c(2, 3), c(3, 4))
  )
})

# ------------------------------------------------------------------------------
# iv_identify_splits()

test_that("identify splits works", {
  x <- iv_pairs(c(1, 7), c(-1, 0), c(2, 3), c(6, 9), c(15, 16))

  expect_identical(
    iv_identify_splits(x),
    list_of(
      iv_pairs(c(1, 2), c(2, 3), c(3, 6), c(6, 7)),
      iv_pairs(c(-1, 0)),
      iv_pairs(c(2, 3)),
      iv_pairs(c(6, 7), c(7, 9)),
      iv_pairs(c(15, 16))
    )
  )
})

test_that("identify splits retains missing intervals (and doesn't duplicate them!)", {
  x <- iv_pairs(c(1, 4), c(NA, NA), c(2, 6), c(NA, NA))

  expect_identical(
    iv_identify_splits(x),
    list_of(
      iv_pairs(c(1, 2), c(2, 4)),
      iv_pairs(c(NA_real_, NA_real_)),
      iv_pairs(c(2, 4), c(4, 6)),
      iv_pairs(c(NA_real_, NA_real_))
    )
  )
})

test_that("identify splits returns a list-of", {
  expect_true(is_list_of(iv_identify_splits(iv(1, 2))))
})

test_that("identify splits works with empty iv", {
  x <- iv(integer(), integer())
  expect_identical(iv_identify_splits(x), list_of(.ptype = x))
})

test_that("identify splits works with single interval", {
  x <- iv(1, 2)
  expect_identical(iv_identify_splits(x), list_of(x))
})

test_that("identify splits works with single missing interval", {
  x <- iv(NA, NA)
  out <- iv_identify_splits(x)

  expect_identical(out, list_of(x))
  expect_identical(attr(out, "ptype"), iv(logical(), logical()))
})

test_that("identify splits works with `on`", {
  x <- iv_pairs(c(1, 5), c(4, 6))

  expect_identical(
    iv_identify_splits(x, on = 2),
    list_of(
      iv_pairs(c(1, 2), c(2, 4), c(4, 5)),
      iv_pairs(c(4, 5), c(5, 6))
    )
  )
})

test_that("identify splits is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3), c(2, 4))

  expect_identical(
    iv_identify_splits(x),
    list_of(
      nested_integer_iv_pairs(c(1, 2), c(2, 3)),
      nested_integer_iv_pairs(c(2, 3), c(3, 4))
    )
  )
})

# ------------------------------------------------------------------------------
# iv_locate_splits()

test_that("locate splits works", {
  x <- iv_pairs(c(1, 7), c(-1, 0), c(2, 3), c(6, 9), c(15, 16))

  out <- iv_locate_splits(x)

  expect_identical(out$key, iv_splits(x))

  expect_identical(
    out$loc,
    list(
      2L,
      1L,
      c(1L, 3L),
      1L,
      c(1L, 4L),
      4L,
      5L
    )
  )
})

test_that("locate splits retains missing intervals", {
  x <- iv_pairs(c(NA, NA), c(NA, NA))

  out <- iv_locate_splits(x)

  expect_identical(out$key, iv_pairs(c(NA, NA)))
  expect_identical(out$loc, list(c(1L, 2L)))
})

test_that("locate splits works with empty iv", {
  x <- iv(integer(), integer())

  out <- iv_locate_splits(x)

  expect_identical(out$key, iv(integer(), integer()))
  expect_identical(out$loc, list())
})

test_that("locate splits works with single missing interval", {
  x <- iv(NA, NA)

  out <- iv_locate_splits(x)

  expect_identical(out$key, iv(NA, NA))
  expect_identical(out$loc, list(1L))
})

test_that("locate splits works with `on`", {
  x <- iv_pairs(c(1, 5), c(4, 6))

  out <- iv_locate_splits(x, on = 2)

  expect_identical(out$key, iv_splits(x, on = 2))
  expect_identical(out$loc, list(1L, 1L, c(1L, 2L), 2L))
})

# ------------------------------------------------------------------------------
# iv_split_candidates()

test_that("works with a single value", {
  expect_identical(
    iv_split_candidates(1, 2),
    list(start = 1, end = 2)
  )
})

test_that("works with a single missing value", {
  expect_identical(
    iv_split_candidates(NA, NA),
    list(start = NA, end = NA)
  )
})

test_that("works with a single `on` value", {
  expect_identical(
    iv_split_candidates(integer(), integer(), on = 1L),
    list(start = integer(), end = integer())
  )
})

test_that("works with a missing `on` value", {
  expect_identical(
    iv_split_candidates(NA_integer_, NA_integer_, on = NA_integer_),
    list(start = NA_integer_, end = NA_integer_)
  )
  expect_identical(
    iv_split_candidates(integer(), integer(), on = NA_integer_),
    list(start = NA_integer_, end = NA_integer_)
  )
})

test_that("casts `on` to type of `x` bounds", {
  x <- iv(1, 2)
  expect_snapshot((expect_error(iv_splits(x, on = "x"))))
})
