test_that("can generate diffs", {
  x <- c(5, 6, 10, 20)
  expect_identical(iv_diff(x), iv_pairs(c(5, 6), c(6, 10), c(10, 20)))
})

test_that("can propagate missings", {
  x <- c(NA, 1, 2)
  expect_identical(iv_diff(x), iv_pairs(c(NA, NA), c(1, 2)))

  x <- c(1, 2, NA)
  expect_identical(iv_diff(x), iv_pairs(c(1, 2), c(NA, NA)))

  x <- c(1, 2, NA, 3, 4)
  expect_identical(iv_diff(x), iv_pairs(c(1, 2), c(NA, NA), c(NA, NA), c(3, 4)))

  x <- c(NA, NA)
  expect_identical(iv_diff(x), iv_pairs(c(NA, NA)))
})

test_that("checks that `x` is a vector", {
  x <- env()

  expect_snapshot(error = TRUE, {
    iv_diff(x)
  })
})

test_that("detects strictly increasing violations", {
  x <- c(1, 0, 2, 1, 2)

  expect_snapshot(error = TRUE, {
    iv_diff(x)
  })
})

test_that("can detect strictly increasing violations in the presence of missings", {
  x <- c(1, NA, 0)

  expect_snapshot(error = TRUE, {
    iv_diff(x)
  })

  x <- c(NA, 0, 2, NA, 0, -1, NA)

  expect_snapshot(error = TRUE, {
    iv_diff(x)
  })

  x <- data_frame(a = c(1, NA, 0, 2), b = c(2, NA, 3, 1))

  expect_snapshot(error = TRUE, {
    iv_diff(x)
  })
})

test_that("incomplete values are promoted to fully missing", {
  x <- data_frame(a = c(1, NA, 4, 5), b = c(2, 3, 7, 8))

  expect_identical(
    iv_diff(x),
    iv_pairs(c(NA, NA), c(NA, NA), vec_c(x[3,], x[4,]))
  )
})

test_that("works with size 1 input", {
  expect_identical(iv_diff(1), iv(double(), double()))

  x <- data_frame(a = 1, b = 2)
  expect_identical(iv_diff(x), iv(x[0,], x[0,]))

  x <- data_frame(a = 1, b = NA)
  expect_identical(iv_diff(x), iv(x[0,], x[0,]))
})

test_that("works with single `NA`", {
  expect_identical(iv_diff(NA), iv(logical(), logical()))
})

test_that("works with size 0 input", {
  expect_identical(iv_diff(double()), iv(double(), double()))

  x <- data_frame(a = double(), b = logical())
  expect_identical(iv_diff(x), iv(x, x))
})
