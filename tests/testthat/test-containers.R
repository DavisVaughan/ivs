# ------------------------------------------------------------------------------
# iv_containers()

# Most tests handled by vctrs

test_that("can compute containers", {
  x <- iv_pairs(c(1, 5), c(3, 5), c(4, 6))
  expect_identical(iv_containers(x), iv_pairs(c(1, 5), c(4, 6)))
})

test_that("works with empty iv", {
  x <- iv(integer(), integer())
  expect_identical(iv_containers(x), x)
})

test_that("duplicate intervals only return a single container", {
  x <- iv_pairs(c(1, 2), c(1, 2))
  expect_identical(iv_containers(x), iv(1, 2))
})

test_that("containers are in sorted order", {
  x <- iv_pairs(c(4, 6), c(1, 5), c(0, 5), c(5, 7))

  expect_identical(
    iv_containers(x),
    iv_pairs(c(0, 5), c(4, 6), c(5, 7))
  )
})

test_that("retains missing at the end", {
  x <- iv_pairs(c(1, 3), c(NA, NA), c(NA, NA), c(0, 4))
  expect_identical(iv_containers(x), iv_pairs(c(0, 4), c(NA, NA)))
})

test_that("containers is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 5), c(NA, NA))

  expect_identical(
    iv_containers(x),
    nested_integer_iv_pairs(c(1, 5), c(NA, NA))
  )
})

# ------------------------------------------------------------------------------
# iv_identify_containers()

test_that("can identify containers", {
  x <- iv_pairs(c(2, 8), c(1, 6), c(2, 3))

  expect_identical(
    iv_identify_containers(x),
    list_of(x[1], x[2], x[c(2, 1)])
  )
})

test_that("missing intervals are identified", {
  x <- iv_pairs(c(NA, NA), c(1, 2), c(NA, NA))

  expect_identical(
    iv_identify_containers(x),
    list_of(x[1], x[2], x[1])
  )
})

test_that("identify containers works with single missing interval", {
  x <- iv(NA, NA)
  out <- iv_identify_containers(x)

  expect_identical(out, list_of(x))
  expect_identical(attr(out, "ptype"), iv(logical(), logical()))
})

test_that("identify containers is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 5), c(NA, NA), c(2, 6))

  expect_identical(
    iv_identify_containers(x),
    list_of(x[1], x[c(1, 4)], x[3], x[4])
  )
})

# ------------------------------------------------------------------------------
# iv_identify_container()

test_that("can identify container", {
  x <- iv_pairs(c(2, 8), c(1, 6), c(2, 7))

  expect_identical(
    iv_identify_container(x),
    iv_pairs(c(2, 8), c(1, 6), c(2, 8))
  )
})

test_that("errors if an interval falls in multiple containers", {
  x <- iv_pairs(c(2, 8), c(1, 6), c(2, 6))

  expect_snapshot(error = TRUE, {
    iv_identify_container(x)
  })
})

test_that("identify container is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 5), c(NA, NA), c(1, 6))

  expect_identical(
    iv_identify_container(x),
    nested_integer_iv_pairs(c(1, 6), c(1, 6), c(NA, NA), c(1, 6))
  )
})

# ------------------------------------------------------------------------------
# iv_locate_containers()

test_that("can locate containers", {
  x <- iv_pairs(c(2, 8), c(1, 6), c(2, 6))

  expect_identical(
    iv_locate_containers(x),
    data_frame(
      key = iv_pairs(c(1, 6), c(2, 8)),
      loc = list(c(2L, 3L), c(1L, 3L))
    )
  )
})

test_that("locate containers is generic", {
  x <- nested_integer_iv_pairs(c(1, 5), c(3, 5), c(NA, NA), c(2, 6))

  expect_identical(
    iv_locate_containers(x),
    data_frame(
      key = nested_integer_iv_pairs(c(1, 5), c(2, 6), c(NA, NA)),
      loc = list(c(1L, 2L), c(2L, 4L), 3L)
    )
  )
})
