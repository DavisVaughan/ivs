test_that("logical formatting", {
  expect_snapshot({
    # With trimming (have to use missing to show trim)
    iv(c(FALSE, NA), TRUE)
  })
})

test_that("integer formatting", {
  expect_snapshot({
    # With trimming
    iv(c(1L, 100L), 200L)

    # With no scientific format
    iv(1e8L, 1e9L)

    # Missings
    iv_pairs(c(1L, 5L), c(NA, NA))
  })
})

test_that("double formatting", {
  expect_snapshot({
    # With trimming
    iv(c(1.5, 1000.5), 2000.5)

    # With no scientific format
    iv(1e8, 1e9)

    # Missings
    iv_pairs(c(1, 5), c(NA, NA))

    # With various decimal values
    iv(100.555, 200)

    # Same amount of rounding as `digits`
    iv(100000.5555, 200000)
    with_options(digits = 12, print(iv(100000.5555, 200000)))
  })
})

test_that("character formatting", {
  expect_snapshot({
    # With no justification
    iv(c("x", "xxxxxxx"), "zzzzzzz")

    # Missings
    iv_pairs(c("x", "z"), c(NA, NA))
  })
})

test_that("factor formatting", {
  expect_snapshot({
    # With no justification
    start <- factor(c("x", "xxxxxxx"), levels = c("x", "xxxxxxx", "zzzzzzz"))
    end <- factor("zzzzzzz", levels = c("x", "xxxxxxx", "zzzzzzz"))
    iv(start, end)

    # Missings
    iv_pairs(factor(c("x", "z")), factor(c(NA, NA)))
  })
})

test_that("data frame formatting", {
  expect_snapshot({
    iv(data.frame(x = 1:5), data.frame(x = 2:6))

    # Missings
    iv(data.frame(x = 1:5), data.frame(x = c(2, 3, NA, 5, NA)))

    # Complex frame
    start <- data.frame(
      x = 1,
      y = "x",
      z = 4L
    )
    end <- data.frame(
      x = 2,
      y = "z",
      z = 5L
    )
    iv(start, end)
  })
})

test_that("Date formatting", {
  expect_snapshot({
    # year-month-day format
    iv(as.Date(c("2019-01-01", "2019-01-05")), as.Date("2019-01-10"))

    # Missings
    iv_pairs(as.Date(c("2019-01-01", "2019-01-05")), as.Date(c(NA, NA)))
  })
})

test_that("POSIXt formatting", {
  expect_snapshot({
    # year-month-day-hour-minute-second format
    iv(
      as.POSIXct(c("2019-01-01", "2019-01-05"), tz = "UTC"),
      as.POSIXct("2019-01-10", tz = "UTC")
    )

    # Missings
    iv_pairs(
      as.POSIXct(c("2019-01-01", "2019-01-05"), tz = "UTC"),
      as.POSIXct(c(NA, NA), tz = "UTC")
    )
  })
})

test_that("difftime formatting", {
  expect_snapshot({
    # With trimming
    iv(as.difftime(c(1, 100), units = "secs"), as.difftime(200, units = "secs"))

    # With no scientific format
    iv(as.difftime(1e8, units = "secs"), as.difftime(1e9, units = "secs"))

    # Missings
    iv(as.difftime(NA_real_, units = "secs"), as.difftime(NA_real_, units = "secs"))
  })
})

test_that("integer64 formatting", {
  skip_if_not_installed("bit64")

  expect_snapshot({
    # With trimming
    start <- bit64::as.integer64(c(1, 100))
    end <- bit64::as.integer64(200)
    iv(start, end)

    # With no scientific format
    start <- bit64::as.integer64(1e8L)
    end <- bit64::as.integer64(1e9L)
    iv(start, end)

    # Missings
    start <- bit64::as.integer64(NA)
    end <- bit64::as.integer64(NA)
    iv(start, end)
  })
})

test_that("iv formatting", {
  expect_snapshot({
    # Recursively avoid trimming
    iv(iv(1:2, 5:6), iv(100, 200))

    # Missings
    iv(iv(c(1, NA), c(5, NA)), iv(100, 200))
  })
})
