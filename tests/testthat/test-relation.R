# ------------------------------------------------------------------------------
# iv_locate_overlaps()

test_that("can locate overlaps", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2))

  expect_identical(
    iv_locate_overlaps(x, y),
    data_frame(needles = c(1L, 1L, 2L), haystack = c(1L, 2L, NA))
  )
})

test_that("iv_locate_overlaps - takes common type", {
  expect_snapshot((expect_error(iv_locate_overlaps(iv(1, 2), iv("a", "b")))))
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_locate_overlaps(x, x),
    data_frame(needles = 1L, haystack = 1L)
  )
})

test_that("can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_locate_overlaps(iv(NA, NA), iv(1, 2), missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_overlaps()

test_that("can detect overlaps", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2))

  expect_identical(
    iv_overlaps(x, y),
    c(TRUE, FALSE)
  )
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)
  y <- iv(1, 2)

  expect_identical(
    iv_overlaps(x, x),
    TRUE
  )
  expect_identical(
    iv_overlaps(x, y),
    FALSE
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_overlaps()

test_that("can detect overlaps pairwise", {
  x <- iv_pairs(c(1, 5), c(10 ,12), c(0, 6))
  y <- iv_pairs(c(3, 6), c(0, 2), c(2, 3))

  expect_identical(
    iv_pairwise_overlaps(x, y),
    c(TRUE, FALSE, TRUE)
  )

  expect_identical(
    iv_pairwise_overlaps(x, y, type = "contains"),
    c(FALSE, FALSE, TRUE)
  )
})

# ------------------------------------------------------------------------------
# iv_prepare_overlaps(), through iv_overlaps() and iv_pairwise_overlaps()

test_that("'any' - any overlap at all", {
  expect_false(iv_overlaps(iv(0, 1), iv(1, 5)))

  expect_true(iv_overlaps(iv(0, 2), iv(1, 5)))
  expect_true(iv_overlaps(iv(0, 6), iv(1, 5)))
  expect_true(iv_overlaps(iv(1, 2), iv(1, 5)))
  expect_true(iv_overlaps(iv(2, 3), iv(1, 5)))
  expect_true(iv_overlaps(iv(1, 5), iv(1, 5)))
  expect_true(iv_overlaps(iv(2, 5), iv(1, 5)))
  expect_true(iv_overlaps(iv(2, 6), iv(1, 5)))

  expect_false(iv_overlaps(iv(5, 6), iv(1, 5)))

  expect_true(iv_overlaps(iv(NA, NA), iv(NA, NA)))
})

test_that("'contains' - needle contains haystack", {
  expect_false(iv_overlaps(iv(0, 1), iv(1, 5), type = "contains"))
  expect_false(iv_overlaps(iv(0, 2), iv(1, 5), type = "contains"))

  expect_true(iv_overlaps(iv(0, 6), iv(1, 5), type = "contains"))

  expect_false(iv_overlaps(iv(1, 2), iv(1, 5), type = "contains"))
  expect_false(iv_overlaps(iv(2, 3), iv(1, 5), type = "contains"))

  expect_true(iv_overlaps(iv(1, 5), iv(1, 5), type = "contains"))

  expect_false(iv_overlaps(iv(2, 5), iv(1, 5), type = "contains"))
  expect_false(iv_overlaps(iv(2, 6), iv(1, 5), type = "contains"))
  expect_false(iv_overlaps(iv(5, 6), iv(1, 5), type = "contains"))

  expect_true(iv_overlaps(iv(NA, NA), iv(NA, NA), type = "contains"))
})

test_that("'within' - needle within haystack", {
  expect_false(iv_overlaps(iv(0, 1), iv(1, 5), type = "within"))
  expect_false(iv_overlaps(iv(0, 2), iv(1, 5), type = "within"))

  expect_false(iv_overlaps(iv(0, 6), iv(1, 5), type = "within"))

  expect_true(iv_overlaps(iv(1, 2), iv(1, 5), type = "within"))
  expect_true(iv_overlaps(iv(2, 3), iv(1, 5), type = "within"))
  expect_true(iv_overlaps(iv(1, 5), iv(1, 5), type = "within"))
  expect_true(iv_overlaps(iv(2, 5), iv(1, 5), type = "within"))

  expect_false(iv_overlaps(iv(2, 6), iv(1, 5), type = "within"))
  expect_false(iv_overlaps(iv(5, 6), iv(1, 5), type = "within"))

  expect_true(iv_overlaps(iv(NA, NA), iv(NA, NA), type = "within"))
})

test_that("'starts' - needle and haystack have same start", {
  expect_false(iv_overlaps(iv(0, 1), iv(1, 5), type = "starts"))
  expect_false(iv_overlaps(iv(0, 2), iv(1, 5), type = "starts"))

  expect_false(iv_overlaps(iv(0, 6), iv(1, 5), type = "starts"))

  expect_true(iv_overlaps(iv(1, 2), iv(1, 5), type = "starts"))

  expect_false(iv_overlaps(iv(2, 3), iv(1, 5), type = "starts"))

  expect_true(iv_overlaps(iv(1, 5), iv(1, 5), type = "starts"))

  expect_false(iv_overlaps(iv(2, 5), iv(1, 5), type = "starts"))
  expect_false(iv_overlaps(iv(2, 6), iv(1, 5), type = "starts"))
  expect_false(iv_overlaps(iv(5, 6), iv(1, 5), type = "starts"))

  expect_true(iv_overlaps(iv(NA, NA), iv(NA, NA), type = "starts"))
})

test_that("'ends' - needle and haystack have same end", {
  expect_false(iv_overlaps(iv(0, 1), iv(1, 5), type = "ends"))
  expect_false(iv_overlaps(iv(0, 2), iv(1, 5), type = "ends"))
  expect_false(iv_overlaps(iv(0, 6), iv(1, 5), type = "ends"))
  expect_false(iv_overlaps(iv(1, 2), iv(1, 5), type = "ends"))
  expect_false(iv_overlaps(iv(2, 3), iv(1, 5), type = "ends"))

  expect_true(iv_overlaps(iv(1, 5), iv(1, 5), type = "ends"))
  expect_true(iv_overlaps(iv(2, 5), iv(1, 5), type = "ends"))

  expect_false(iv_overlaps(iv(2, 6), iv(1, 5), type = "ends"))
  expect_false(iv_overlaps(iv(5, 6), iv(1, 5), type = "ends"))

  expect_true(iv_overlaps(iv(NA, NA), iv(NA, NA), type = "ends"))
})

test_that("'equals' - needle and haystack have same start and end", {
  expect_false(iv_overlaps(iv(0, 1), iv(1, 5), type = "equals"))
  expect_false(iv_overlaps(iv(0, 2), iv(1, 5), type = "equals"))
  expect_false(iv_overlaps(iv(0, 6), iv(1, 5), type = "equals"))
  expect_false(iv_overlaps(iv(1, 2), iv(1, 5), type = "equals"))
  expect_false(iv_overlaps(iv(2, 3), iv(1, 5), type = "equals"))

  expect_true(iv_overlaps(iv(1, 5), iv(1, 5), type = "equals"))

  expect_false(iv_overlaps(iv(2, 5), iv(1, 5), type = "equals"))
  expect_false(iv_overlaps(iv(2, 6), iv(1, 5), type = "equals"))
  expect_false(iv_overlaps(iv(5, 6), iv(1, 5), type = "equals"))

  expect_true(iv_overlaps(iv(NA, NA), iv(NA, NA), type = "equals"))
})

# ------------------------------------------------------------------------------
# iv_locate_precedes()

test_that("can locate precedes", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_locate_precedes(x, y),
    data_frame(needles = c(1L, 2L), haystack = c(3L, NA))
  )
})

test_that("can locate the closest one it precedes", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7), c(6, 9), c(8, 10))

  expect_identical(
    iv_locate_precedes(x, y, closest = TRUE),
    data_frame(needles = c(1L, 1L, 2L), haystack = c(3L, 4L, NA))
  )
  expect_identical(
    iv_locate_precedes(x, y, closest = TRUE, multiple = "first"),
    data_frame(needles = c(1L, 2L), haystack = c(3L, NA))
  )
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_locate_precedes(x, x),
    data_frame(needles = 1L, haystack = NA_integer_)
  )
})

# ------------------------------------------------------------------------------
# iv_locate_follows()

test_that("can locate follows", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_locate_follows(x, y),
    data_frame(needles = c(1L, 2L, 2L, 2L), haystack = c(NA, 1:3))
  )
})

test_that("can locate the closest one it follows", {
  x <- iv_pairs(c(1, 5), c(10, 12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7), c(6, 9), c(8, 10))

  expect_identical(
    iv_locate_follows(x, y, closest = TRUE),
    data_frame(needles = c(1L, 2L), haystack = c(NA, 5L))
  )
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_locate_follows(x, x),
    data_frame(needles = 1L, haystack = NA_integer_)
  )
})

# ------------------------------------------------------------------------------
# iv_locate_positional(), through iv_locate_precedes()

test_that("iv_locate_precedes - takes common type", {
  expect_snapshot((expect_error(iv_locate_precedes(iv(1, 2), iv("a", "b")))))
})

test_that("iv_locate_precedes - validates 'closest'", {
  expect_snapshot((expect_error(iv_locate_precedes(iv(1, 2), iv(1, 2), closest = "x"))))
})

test_that("`missing = 'equals'` forces `no_match` because missings will never match", {
  x <- iv_pairs(c(1, 3), c(NA, NA))

  expect_identical(
    iv_locate_precedes(x, x, missing = "equals", no_match = 0L),
    data_frame(needles = c(1L, 2L), haystack = c(0L, 0L))
  )
})

test_that("can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_locate_precedes(iv(NA, NA), iv(1, 2), missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_precedes()

test_that("can detect precedes", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_precedes(x, y),
    c(TRUE, FALSE)
  )
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_precedes(x, x),
    FALSE
  )
})

# ------------------------------------------------------------------------------
# iv_follows()

test_that("can detect follows", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_follows(x, y),
    c(FALSE, TRUE)
  )
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_follows(x, x),
    FALSE
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_precedes()

test_that("can detect pairwise precedes", {
  x <- iv_pairs(c(1, 5), c(10 ,12), c(2, 3))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_pairwise_precedes(x, y),
    c(FALSE, FALSE, TRUE)
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_follows()

test_that("can detect pairwise follows", {
  x <- iv_pairs(c(1, 5), c(10 ,12), c(2, 3))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_pairwise_follows(x, y),
    c(FALSE, TRUE, FALSE)
  )
})

# ------------------------------------------------------------------------------
# iv_prepare_positional(), through iv_precedes() and iv_follows()

test_that("'precedes'", {
  expect_true(iv_precedes(iv(-1, 0), iv(1, 5)))
  expect_true(iv_precedes(iv(0, 1), iv(1, 5)))

  expect_false(iv_precedes(iv(0, 2), iv(1, 5)))
  expect_false(iv_precedes(iv(0, 6), iv(1, 5)))
  expect_false(iv_precedes(iv(1, 2), iv(1, 5)))
  expect_false(iv_precedes(iv(2, 3), iv(1, 5)))
  expect_false(iv_precedes(iv(1, 5), iv(1, 5)))
  expect_false(iv_precedes(iv(2, 5), iv(1, 5)))
  expect_false(iv_precedes(iv(2, 6), iv(1, 5)))
  expect_false(iv_precedes(iv(5, 6), iv(1, 5)))
  expect_false(iv_precedes(iv(6, 7), iv(1, 5)))

  expect_false(iv_precedes(iv(NA, NA), iv(NA, NA)))
})

test_that("'follows'", {
  expect_false(iv_follows(iv(-1, 0), iv(1, 5)))
  expect_false(iv_follows(iv(0, 1), iv(1, 5)))
  expect_false(iv_follows(iv(0, 2), iv(1, 5)))
  expect_false(iv_follows(iv(0, 6), iv(1, 5)))
  expect_false(iv_follows(iv(1, 2), iv(1, 5)))
  expect_false(iv_follows(iv(2, 3), iv(1, 5)))
  expect_false(iv_follows(iv(1, 5), iv(1, 5)))
  expect_false(iv_follows(iv(2, 5), iv(1, 5)))
  expect_false(iv_follows(iv(2, 6), iv(1, 5)))

  expect_true(iv_follows(iv(5, 6), iv(1, 5)))
  expect_true(iv_follows(iv(6, 7), iv(1, 5)))

  expect_false(iv_follows(iv(NA, NA), iv(NA, NA)))
})

# ------------------------------------------------------------------------------
# iv_locate_relation()

test_that("can locate relation", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_locate_relation(x, y, type = "overlaps"),
    data_frame(needles = c(1L, 2L), haystack = c(1L, NA))
  )
  expect_identical(
    iv_locate_relation(x, y, type = "precedes"),
    data_frame(needles = c(1L, 2L), haystack = c(3L, NA))
  )
})

test_that("iv_locate_relation - takes common type", {
  expect_snapshot((expect_error(iv_locate_relation(iv(1, 2), iv("a", "b")))))
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_locate_relation(x, x, type = "overlaps"),
    data_frame(needles = 1L, haystack = NA_integer_)
  )
  expect_identical(
    iv_locate_relation(x, x, type = "equals"),
    data_frame(needles = 1L, haystack = 1L)
  )
})

test_that("can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_locate_relation(iv(NA, NA), iv(1, 2), type = "equals", missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_relates()

test_that("can detect relation", {
  x <- iv_pairs(c(1, 5), c(10 ,12))
  y <- iv_pairs(c(3, 6), c(0, 2), c(6, 7))

  expect_identical(
    iv_relates(x, y, type = "overlaps"),
    c(TRUE, FALSE)
  )
})

test_that("treats missings as equal by default", {
  x <- iv(NA, NA)

  expect_identical(
    iv_relates(x, x, type = "overlaps"),
    FALSE
  )
  expect_identical(
    iv_relates(x, x, type = "equals"),
    TRUE
  )
})

# ------------------------------------------------------------------------------
# iv_pairwise_relates()

test_that("can detect overlaps pairwise", {
  x <- iv_pairs(c(1, 5), c(10 ,12), c(0, 6))
  y <- iv_pairs(c(3, 6), c(0, 2), c(2, 3))

  expect_identical(
    iv_pairwise_relates(x, y, type = "contains"),
    c(FALSE, FALSE, TRUE)
  )
})

# ------------------------------------------------------------------------------
# iv_prepare_relation(), through iv_relates()

test_that("'precedes'", {
  expect_true(iv_relates(iv(-1, 0), iv(1, 5), type = "precedes"))

  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "precedes"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "precedes"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "precedes"))
})

test_that("'preceded-by'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "preceded-by"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "preceded-by"))

  expect_true(iv_relates(iv(6, 7), iv(1, 5), type = "preceded-by"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "preceded-by"))
})

test_that("'meets'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "meets"))

  expect_true(iv_relates(iv(0, 1), iv(1, 5), type = "meets"))

  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "meets"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "meets"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "meets"))
})

test_that("'met-by'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "met-by"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "met-by"))

  expect_true(iv_relates(iv(5, 6), iv(1, 5), type = "met-by"))

  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "met-by"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "met-by"))
})

test_that("'overlaps'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "overlaps"))

  expect_true(iv_relates(iv(0, 2), iv(1, 5), type = "overlaps"))

  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "overlaps"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "overlaps"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "overlaps"))
})

test_that("'overlapped-by'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "overlapped-by"))

  expect_true(iv_relates(iv(2, 6), iv(1, 5), type = "overlapped-by"))

  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "overlapped-by"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "overlapped-by"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "overlapped-by"))
})

test_that("'starts'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "starts"))

  expect_true(iv_relates(iv(1, 2), iv(1, 5), type = "starts"))

  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "starts"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "starts"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "starts"))
})

test_that("'started-by'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "started-by"))

  expect_true(iv_relates(iv(1, 6), iv(1, 5), type = "started-by"))

  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "started-by"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "started-by"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "started-by"))
})

test_that("'finishes'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "finishes"))

  expect_true(iv_relates(iv(2, 5), iv(1, 5), type = "finishes"))

  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "finishes"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "finishes"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "finishes"))
})

test_that("'finished-by'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "finished-by"))

  expect_true(iv_relates(iv(0, 5), iv(1, 5), type = "finished-by"))

  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "finished-by"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "finished-by"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "finished-by"))
})

test_that("'during'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "during"))

  expect_true(iv_relates(iv(2, 3), iv(1, 5), type = "during"))

  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "during"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "during"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "during"))
})

test_that("'contains'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "contains"))

  expect_true(iv_relates(iv(0, 6), iv(1, 5), type = "contains"))

  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(1, 5), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "contains"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "contains"))

  expect_false(iv_relates(iv(NA, NA), iv(NA, NA), type = "contains"))
})

test_that("'equals'", {
  expect_false(iv_relates(iv(-1, 0), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(0, 1), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(0, 2), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(0, 6), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(1, 2), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(1, 6), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(2, 3), iv(1, 5), type = "equals"))

  expect_true(iv_relates(iv(1, 5), iv(1, 5), type = "equals"))

  expect_false(iv_relates(iv(0, 5), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(2, 5), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(2, 6), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(5, 6), iv(1, 5), type = "equals"))
  expect_false(iv_relates(iv(6, 7), iv(1, 5), type = "equals"))

  expect_true(iv_relates(iv(NA, NA), iv(NA, NA), type = "equals"))
})

# ------------------------------------------------------------------------------
# iv_detect_impl(), through iv_overlaps()

test_that("gives correct results for various forms of 'missing'", {
  x <- iv(NA, NA)
  y <- iv(1, 2)

  expect_false(iv_overlaps(x, y, missing = "equals"))
  expect_true(iv_overlaps(x, x, missing = "equals"))

  expect_identical(iv_overlaps(x, y, missing = NA), NA)
  expect_identical(iv_overlaps(x, y, missing = FALSE), FALSE)
  expect_identical(iv_overlaps(x, y, missing = TRUE), TRUE)
})

test_that("iv_detect_impl - takes common type", {
  expect_snapshot((expect_error(iv_overlaps(iv(1, 2), iv("a", "b")))))
})

test_that("iv_detect_impl - validates 'missing'", {
  expect_snapshot({
    (expect_error(iv_overlaps(iv(1, 2), iv(1, 2), missing = 1)))
    (expect_error(iv_overlaps(iv(1, 2), iv(1, 2), missing = "x")))
    (expect_error(iv_overlaps(iv(1, 2), iv(1, 2), missing = c(TRUE, FALSE))))
  })
})

test_that("detect can error on missing needles", {
  expect_snapshot(
    (expect_error(iv_overlaps(iv(NA, NA), iv(1, 2), missing = "error")))
  )
})

# ------------------------------------------------------------------------------
# iv_detect_pairwise_impl(), through iv_pairwise_overlaps()

test_that("missing intervals always propagate", {
  x <- iv_pairs(c(1, 2), c(NA, NA), c(NA, NA))
  y <- iv_pairs(c(NA, NA), c(3, 4), c(NA, NA))

  expect_identical(iv_pairwise_overlaps(x, y), c(NA, NA, NA))
})

test_that("iv_detect_pairwise_impl - recycles correctly", {
  expect_snapshot((expect_error(iv_pairwise_overlaps(iv(1:2, 2:3), iv(1:3, 2:4)))))
})

test_that("iv_detect_pairwise_impl - takes common type", {
  expect_snapshot((expect_error(iv_pairwise_overlaps(iv(1, 2), iv("a", "b")))))
})
