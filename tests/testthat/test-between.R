# ------------------------------------------------------------------------------
# iv_locate_between()

test_that("locate between gets endpoints right", {
  x <- c(1, 3)
  y <- iv_pairs(c(1, 5), c(0, 1))

  expect_identical(
    iv_locate_between(x, y),
    data_frame(needles = c(1L, 2L), haystack = c(1L, 1L))
  )
})

test_that("can control missing value results", {
  x <- c(NA, 3)
  y <- iv_pairs(c(NA, NA))

  expect_identical(
    iv_locate_between(x, y),
    data_frame(needles = c(1L, 2L), haystack = c(1L, NA_integer_))
  )
  expect_identical(
    iv_locate_between(x, y, missing = NA_integer_),
    data_frame(needles = c(1L, 2L), haystack = c(NA_integer_, NA_integer_))
  )
  expect_identical(
    iv_locate_between(x, y, missing = "drop"),
    data_frame(needles = c(2L), haystack = c(NA_integer_))
  )
})

test_that("`NaN` values look like `NA_real_`", {
  x <- NaN
  y <- iv_pairs(c(NA, NA))

  expect_identical(
    iv_locate_between(x, y, missing = "equals"),
    data_frame(needles = 1L, haystack = 1L)
  )
})

test_that("between takes the common type", {
  expect_snapshot(
    (expect_error(iv_locate_between(1, iv("a", "b"))))
  )
})

test_that("between can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_locate_between(NA, iv(1, 2), missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_locate_includes()

test_that("locate includes gets endpoints right", {
  x <- iv_pairs(c(1, 5), c(0, 1))
  y <- c(1, 3)

  expect_identical(
    iv_locate_includes(x, y),
    data_frame(needles = c(1L, 1L, 2L), haystack = c(1L, 2L, NA))
  )
})

test_that("can control missing value results", {
  x <- iv_pairs(c(1, 4), c(NA, NA))
  y <- c(NA, 3)

  expect_identical(
    iv_locate_includes(x, y),
    data_frame(needles = c(1L, 2L), haystack = c(2L, 1L))
  )
  expect_identical(
    iv_locate_includes(x, y, missing = NA_integer_),
    data_frame(needles = c(1L, 2L), haystack = c(2L, NA_integer_))
  )
  expect_identical(
    iv_locate_includes(x, y, missing = "drop"),
    data_frame(needles = 1L, haystack = 2L)
  )
})

test_that("`NaN` values look like `NA_real_`", {
  x <- iv_pairs(c(NA, NA))
  y <- NaN

  expect_identical(
    iv_locate_includes(x, y, missing = "equals"),
    data_frame(needles = 1L, haystack = 1L)
  )
})

test_that("includes takes the common type", {
  expect_snapshot(
    (expect_error(iv_locate_includes(iv("a", "b"), 1)))
  )
})

test_that("includes can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_locate_includes(iv(NA, NA), 1, missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_count_between()

test_that("count between gets endpoints right", {
  x <- c(1, 3)
  y <- iv_pairs(c(1, 5), c(0, 1))

  expect_identical(
    iv_count_between(x, y),
    c(1L, 1L)
  )
})

test_that("can control missing value results", {
  x <- c(NA, 3)
  y <- iv_pairs(c(NA, NA))

  expect_identical(
    iv_count_between(x, y),
    c(1L, 0L)
  )
  expect_identical(
    iv_count_between(x, y, missing = NA_integer_),
    c(NA, 0L)
  )
})

test_that("`NaN` values look like `NA_real_`", {
  x <- NaN
  y <- iv_pairs(c(NA, NA))

  expect_identical(
    iv_count_between(x, y, missing = "equals"),
    1L
  )
})

test_that("between can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_count_between(NA, iv(1, 2), missing = "error")))
  )
})

test_that("between can error on unmatched needles", {
  expect_snapshot(
    (expect_error(iv_count_between(3, iv(1, 2), no_match = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_between()

test_that("detect between gets endpoints right", {
  x <- c(1, 3, 5)
  y <- iv_pairs(c(1, 5), c(0, 1))

  expect_identical(
    iv_between(x, y),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("can control missing value results", {
  x <- c(NA, 3)
  y <- iv_pairs(c(NA, NA))

  expect_identical(
    iv_between(x, y),
    c(TRUE, FALSE)
  )
  expect_identical(
    iv_between(x, y, missing = NA),
    c(NA, FALSE)
  )
  expect_identical(
    iv_between(x, y, missing = FALSE),
    c(FALSE, FALSE)
  )
})

test_that("`NaN` values look like `NA_real_`", {
  x <- NaN
  y <- iv_pairs(c(NA, NA))

  expect_identical(
    iv_between(x, y, missing = "equals"),
    TRUE
  )
})

test_that("detect between takes the common type", {
  expect_snapshot(
    (expect_error(iv_between(1, iv("a", "b"))))
  )
})

test_that("detect between can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_between(NA, iv(1, 2), missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_between()

test_that("detect pairwise between gets endpoints right", {
  x <- c(1, 3, 5)
  y <- iv_pairs(c(1, 5), c(0, 1), c(1, 5))

  expect_identical(
    iv_pairwise_between(x, y),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("missing values always propagate", {
  expect_identical(
    iv_pairwise_between(NA, iv(NA, NA)),
    NA
  )
  expect_identical(
    iv_pairwise_between(NA, iv(2, 3)),
    NA
  )
  expect_identical(
    iv_pairwise_between(1, iv(NA, NA)),
    NA
  )
})

test_that("`NaN` values look like `NA_real_`", {
  x <- NaN
  y <- iv_pairs(c(1, 2))

  expect_identical(
    iv_pairwise_between(x, y),
    NA
  )
})

test_that("detect pairwise between takes the common type", {
  expect_snapshot(
    (expect_error(iv_pairwise_between(1, iv("a", "b"))))
  )
})
