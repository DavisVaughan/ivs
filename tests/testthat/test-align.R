test_that("correctly aligns ivs", {
  needles <- iv_pairs(c(1, 5), c(3, 7), c(10, 12))
  haystack <- iv_pairs(c(0, 2), c(4, 6))

  locations <- iv_locate_overlaps(needles, haystack)

  expect <- data_frame(
    needles = vec_slice(needles, locations$needles),
    haystack = vec_slice(haystack, locations$haystack)
  )

  expect_identical(
    iv_align(needles, haystack, locations = locations),
    expect
  )
})

test_that("correctly aligns with between", {
  needles <- c(1, 15, 4, 11)
  haystack <- iv_pairs(c(1, 5), c(3, 7), c(10, 12))

  locations <- iv_locate_between(needles, haystack)

  expect <- data_frame(
    needles = vec_slice(needles, locations$needles),
    haystack = vec_slice(haystack, locations$haystack)
  )

  expect_identical(
    iv_align(needles, haystack, locations = locations),
    expect
  )
})

test_that("`locations` is validated", {
  expect_snapshot({
    (expect_error(iv_align(1, 2, locations = 1)))
    (expect_error(iv_align(1, 2, locations = data_frame())))
    (expect_error(iv_align(1, 2, locations = data_frame(x = 1, haystack = 2))))
    (expect_error(iv_align(1, 2, locations = data_frame(needles = 1, x = 2))))
    (expect_error(iv_align(1, 2, locations = data_frame(needles = 1, haystack = 2L))))
    (expect_error(iv_align(1, 2, locations = data_frame(needles = 1L, haystack = 2))))
  })
})
