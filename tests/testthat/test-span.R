# ------------------------------------------------------------------------------
# iv_span()

test_that("takes the span over the whole range", {
  x <- iv_pairs(c(1, 3), c(7, 10), c(-6, -2), c(9, 12))

  expect_identical(iv_span(x), iv(-6, 12))
})

test_that("missing + `missing = propagate` returns missing value", {
  x <- iv_pairs(c(1, 3), c(NA, NA), c(-5, -1))
  expect_identical(iv_span(x), iv(NA_real_, NA_real_))
})

test_that("missing + `missing = error` errors", {
  x <- iv_pairs(c(1, 3), c(NA, NA), c(-5, -1))

  expect_snapshot(error = TRUE, {
    iv_span(x, missing = "error")
  })
})

test_that("missing + `missing = <iv>` returns `missing` value", {
  x <- iv_pairs(c(1, 3), c(NA, NA), c(-5, -1))
  missing <- iv(-Inf, Inf)
  expect_identical(iv_span(x, missing = missing), missing)
})

test_that("missing + `missing = drop` filters out missings", {
  x <- iv_pairs(c(1, 3), c(NA, NA), c(-5, -1))
  expect_identical(iv_span(x, missing = "drop"), iv(-5, 3))
})

test_that("missing + `missing = drop` + `empty = missing` returns missing value", {
  x <- iv_pairs(c(NA, NA), c(NA, NA))
  expect_identical(iv_span(x, missing = "drop"), iv(NA, NA))
})

test_that("missing + `missing = drop` + `empty = error` errors", {
  x <- iv_pairs(c(NA, NA), c(NA, NA), ptype = double())

  expect_snapshot(error = TRUE, {
    iv_span(x, missing = "drop", empty = "error")
  })
})

test_that("missing + `missing = drop` + `empty = <iv>` returns `empty` value", {
  x <- iv_pairs(c(NA, NA), c(NA, NA), ptype = double())
  empty <- iv(-Inf, Inf)
  expect_identical(iv_span(x, missing = "drop", empty = empty), empty)
})

test_that("empty + `empty = missing` returns missing value", {
  x <- iv(integer(), integer())
  expect_identical(iv_span(x), iv(NA_integer_, NA_integer_))
})

test_that("empty + `empty = error` errors", {
  x <- iv(integer(), integer())

  expect_snapshot(error = TRUE, {
    iv_span(x, empty = "error")
  })
})

test_that("empty + `empty = <iv>` returns `empty` value", {
  x <- iv(integer(), integer())
  empty <- iv(0L, 1L)
  expect_identical(iv_span(x, empty = empty), empty)
})

test_that("span is generic over the container", {
  x <- nested_integer_iv_pairs(c(-5, 0), c(2, 4))
  expect_identical(iv_span(x), nested_integer_iv(-5, 4))

  x <- nested_integer_iv_pairs(c(-5, 0), c(2, 4), c(NA, NA))
  expect_identical(iv_span(x), nested_integer_iv(NA, NA))
  expect_identical(iv_span(x, missing = "drop"), nested_integer_iv(-5, 4))
})

test_that("span casts pre-proxied `empty` to pre-proxied type of `x`", {
  x <- nested_integer_iv(integer(), integer())
  empty <- nested_integer_iv(0, 1)
  expect_identical(iv_span(x, empty = empty), empty)

  x <- nested_integer_iv(integer(), integer())
  empty <- iv(0, 1)
  expect_snapshot(error = TRUE, {
    iv_span(x, empty = empty)
  })

  x <- iv(integer(), integer())
  empty <- nested_integer_iv(0, 1)
  expect_snapshot(error = TRUE, {
    iv_span(x, empty = empty)
  })
})

test_that("span casts pre-proxied `missing` to pre-proxied type of `x`", {
  x <- nested_integer_iv(NA, NA)
  missing <- nested_integer_iv(0, 1)
  expect_identical(iv_span(x, missing = missing), missing)

  x <- nested_integer_iv(NA, NA)
  missing <- iv(0L, 1L)
  expect_snapshot(error = TRUE, {
    iv_span(x, missing = missing)
  })

  x <- iv(NA, NA, ptype = integer())
  missing <- nested_integer_iv(0, 1)
  expect_snapshot(error = TRUE, {
    iv_span(x, missing = missing)
  })
})

test_that("errors on non-empty dots", {
  x <- iv(1, 2)

  expect_snapshot(error = TRUE, {
    iv_span(x, 2)
  })
})

test_that("validates `x` is an iv", {
  expect_snapshot(error = TRUE, {
    iv_span(1)
  })
})

test_that("validates `empty`", {
  x <- iv(integer(), integer())

  expect_snapshot(error = TRUE, {
    iv_span(x, empty = "x")
  })
  expect_snapshot(error = TRUE, {
    iv_span(x, empty = 1)
  })
  expect_snapshot(error = TRUE, {
    iv_span(x, empty = iv(1.5, 2.5))
  })
  expect_snapshot(error = TRUE, {
    iv_span(x, empty = iv(1:2, 2:3))
  })
})

test_that("validates `missing`", {
  x <- iv(integer(), integer())

  expect_snapshot(error = TRUE, {
    iv_span(x, missing = "x")
  })
  expect_snapshot(error = TRUE, {
    iv_span(x, missing = 1)
  })
  expect_snapshot(error = TRUE, {
    iv_span(x, missing = iv(1.5, 2.5))
  })
  expect_snapshot(error = TRUE, {
    iv_span(x, missing = iv(1:2, 2:3))
  })
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
