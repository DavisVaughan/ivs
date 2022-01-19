# ------------------------------------------------------------------------------
# iv_split()

test_that("split works", {
  x <- iv_pairs(c(1, 7), c(-1, 0), c(2, 3), c(6, 9), c(15, 16))

  expect_identical(
    iv_split(x),
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
    iv_split(x),
    iv_pairs(c(1, 4), c(5, 6), c(NA, NA))
  )
})

test_that("split works with empty iv", {
  x <- iv(integer(), integer())
  expect_identical(iv_split(x), x)
})

test_that("split works with single interval", {
  x <- iv(1, 2)
  expect_identical(iv_split(x), x)
})

test_that("split works with single missing interval", {
  x <- iv(NA, NA)
  expect_identical(iv_split(x), x)
})

test_that("split is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3), c(2, 4))
  expect_identical(
    iv_split(x),
    nested_integer_iv_pairs(c(1, 2), c(2, 3), c(3, 4))
  )
})

# ------------------------------------------------------------------------------
# iv_replace_splits()

test_that("replace split works", {
  x <- iv_pairs(c(1, 7), c(-1, 0), c(2, 3), c(6, 9), c(15, 16))

  expect_identical(
    iv_replace_splits(x),
    list_of(
      iv_pairs(c(1, 2), c(2, 3), c(3, 6), c(6, 7)),
      iv_pairs(c(-1, 0)),
      iv_pairs(c(2, 3)),
      iv_pairs(c(6, 7), c(7, 9)),
      iv_pairs(c(15, 16))
    )
  )
})

test_that("replace split retains missing intervals (and doesn't duplicate them!)", {
  x <- iv_pairs(c(1, 4), c(NA, NA), c(2, 6), c(NA, NA))

  expect_identical(
    iv_replace_splits(x),
    list_of(
      iv_pairs(c(1, 2), c(2, 4)),
      iv_pairs(c(NA_real_, NA_real_)),
      iv_pairs(c(2, 4), c(4, 6)),
      iv_pairs(c(NA_real_, NA_real_))
    )
  )
})

test_that("replace split returns a list-of", {
  expect_true(is_list_of(iv_replace_splits(iv(1, 2))))
})

test_that("replace split works with empty iv", {
  x <- iv(integer(), integer())
  expect_identical(iv_replace_splits(x), list_of(.ptype = x))
})

test_that("replace split works with single interval", {
  x <- iv(1, 2)
  expect_identical(iv_replace_splits(x), list_of(x))
})

test_that("replace split works with single missing interval", {
  x <- iv(NA, NA)
  expect_identical(iv_replace_splits(x), list_of(x))
})

test_that("replace split is generic over container", {
  x <- nested_integer_iv_pairs(c(1, 3), c(2, 4))

  expect_identical(
    iv_replace_splits(x),
    list_of(
      nested_integer_iv_pairs(c(1, 2), c(2, 3)),
      nested_integer_iv_pairs(c(2, 3), c(3, 4))
    )
  )
})

# ------------------------------------------------------------------------------
# iv_locate_split_groups()

test_that("locate split groups works", {
  x <- iv_pairs(c(1, 7), c(-1, 0), c(2, 3), c(6, 9), c(15, 16))

  out <- iv_locate_split_groups(x)

  expect_identical(out$key, iv_split(x))

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

test_that("locate split groups retains missing intervals", {
  x <- iv_pairs(c(NA, NA), c(NA, NA))

  out <- iv_locate_split_groups(x)

  expect_identical(out$key, iv_pairs(c(NA, NA)))
  expect_identical(out$loc, list(c(1L, 2L)))
})

test_that("locate split groups works with empty iv", {
  x <- iv(integer(), integer())

  out <- iv_locate_split_groups(x)

  expect_identical(out$key, iv(integer(), integer()))
  expect_identical(out$loc, list())
})

test_that("locate split groups works with single missing interval", {
  x <- iv(NA, NA)

  out <- iv_locate_split_groups(x)

  expect_identical(out$key, iv(NA, NA))
  expect_identical(out$loc, list(1L))
})
