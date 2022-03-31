# ------------------------------------------------------------------------------
# iv_count_locations()

test_that("counts are returned in appearance order", {
  locations <- data_frame(
    needles = c(1L, 1L, 2L, 2L, 2L, 3L),
    haystack = c(2L, 2L, 2L, 2L, 2L, 2L)
  )

  expect_identical(
    iv_count_locations(locations, missing = "error", no_match = "error"),
    c(2L, 3L, 1L)
  )
})

test_that("missings can be differentiated from no_match", {
  locations <- data_frame(
    needles = c(1L, 2L, 3L),
    haystack = c(2L, SIGNAL_MISSING, SIGNAL_NO_MATCH)
  )

  expect_identical(
    iv_count_locations(locations, missing = 0L, no_match = NA_integer_),
    c(1L, 0L, NA)
  )
})

# ------------------------------------------------------------------------------
# check_count_missing()

test_that("equals and error pass through", {
  expect_identical(check_count_missing("equals"), "equals")
  expect_identical(check_count_missing("error"), "error")
})

test_that("drop is not allowed", {
  expect_snapshot(error = TRUE, check_count_missing("drop"))
})

test_that("integerish is allowed", {
  expect_identical(check_count_missing(1), 1L)
})

test_that("`missing` is checked", {
  expect_snapshot(error = TRUE, check_count_missing(1.5))
  expect_snapshot(error = TRUE, check_count_missing(c(1, 2)))
})

# ------------------------------------------------------------------------------
# translate_count_missing()

test_that("string is left alone", {
  expect_identical(translate_count_missing("foo"), "foo")
})

test_that("integer is converted to SIGNAL_MISSING", {
  expect_identical(translate_count_missing(1L), SIGNAL_MISSING)
})

# ------------------------------------------------------------------------------
# check_count_no_match()

test_that("error passes through", {
  expect_identical(check_count_no_match("error"), "error")
})

test_that("drop is not allowed", {
  expect_snapshot(error = TRUE, check_count_no_match("drop"))
})

test_that("integerish is allowed", {
  expect_identical(check_count_no_match(1), 1L)
})

test_that("`no_match` is checked", {
  expect_snapshot(error = TRUE, check_count_no_match(1.5))
  expect_snapshot(error = TRUE, check_count_no_match(c(1, 2)))
})

# ------------------------------------------------------------------------------
# translate_count_no_match()

test_that("string is left alone", {
  expect_identical(translate_count_no_match("foo"), "foo")
})

test_that("integer is converted to SIGNAL_NO_MATCH", {
  expect_identical(translate_count_no_match(1L), SIGNAL_NO_MATCH)
})
