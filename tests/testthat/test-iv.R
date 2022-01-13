# ------------------------------------------------------------------------------
# new_iv()

test_that("can create a new iv", {
  x <- new_iv(1, 2)
  expect_s3_class(x, "iv")
})

test_that("can attach attributes", {
  x <- new_iv(1, 2, foo = "bar")
  expect_identical(attr(x, "foo"), "bar")
})

test_that("can subclass an iv", {
  x <- new_iv(1, 2, class = "subclass")
  expect_identical(class(x)[1:2], c("subclass", "iv"))
})

# ------------------------------------------------------------------------------
# iv()

test_that("can generate an iv", {
  expect_identical(iv(1, 2), new_iv(1, 2))
})

test_that("incomplete values are propagated", {
  expect_identical(iv(NA, TRUE), iv(NA, NA))
  expect_identical(iv(TRUE, NA), iv(NA, NA))

  # Propagates incompleteness, not missingness!
  # Seen as incomplete even though start is "less" than end
  x <- data_frame(x = 1, y = NA)
  y <- data_frame(x = 2, y = 1)

  expect <- data_frame(x = NA_real_, y = NA_real_)

  expect_identical(iv(x, y), iv(expect, expect))
})

test_that("`start` must be less than `end`", {
  expect_snapshot(error = TRUE, iv(2, 2))
  expect_snapshot(error = TRUE, iv(3, 2))
})

test_that("inputs must be type compatible", {
  expect_snapshot(error = TRUE, iv("x", 1))
})

test_that("inputs must be size compatible", {
  expect_snapshot(error = TRUE, iv(1:2, 1:3))
})

test_that("inputs must be vectors", {
  expect_snapshot(error = TRUE, iv(NULL, 2))
  expect_snapshot(error = TRUE, iv(2, NULL))
})

# ------------------------------------------------------------------------------
# iv_pairs()

test_that("can generate an iv from pairs", {
  expect_identical(iv_pairs(c(1, 2), c(3, 4)), iv(c(1, 3), c(2, 4)))
})

test_that("inputs must be in pairs", {
  expect_snapshot(error = TRUE, iv_pairs(c(1, 2), 3))
})

test_that("must have at least one input", {
  expect_snapshot(error = TRUE, iv_pairs())
})

test_that("pairs must be type compatible", {
  expect_snapshot(error = TRUE, iv_pairs(c("a", "b"), c(1, 2)))
})

# ------------------------------------------------------------------------------
# is_iv()

test_that("can check if an object is an iv", {
  expect_identical(is_iv(1), FALSE)
  expect_identical(is_iv(new_iv(1, 2)), TRUE)
})

# ------------------------------------------------------------------------------
# vec_ptype2()

test_that("ptype2 is computed right", {
  expect_identical(
    vec_ptype2(iv(1, 2), iv(1L, 2L)),
    iv(double(), double())
  )
})

test_that("ptype2 errors as needed", {
  expect_snapshot(error = TRUE, vec_ptype2(iv("x", "y"), iv(1L, 2L)))
})

# ------------------------------------------------------------------------------
# vec_cast()

test_that("cast is computed right", {
  expect_identical(
    vec_cast(iv(1, 2), iv(integer(), integer())),
    iv(1L, 2L)
  )
})

test_that("cast errors as needed", {
  expect_snapshot(error = TRUE, vec_cast(iv("x", "y"), iv(1L, 2L)))
})

# ------------------------------------------------------------------------------
# vec_ptype_abbr()

test_that("abbreviation is passed through to inner type", {
  expect_snapshot(vec_ptype_abbr(iv(1, 2)))
  expect_snapshot(vec_ptype_abbr(iv(data_frame(x = 1), data_frame(x = 2))))
})

# ------------------------------------------------------------------------------
# vec_ptype_full()

test_that("full ptype is passed through to inner type", {
  expect_snapshot(vec_ptype_full(iv(1, 2)))
  expect_snapshot(vec_ptype_full(iv(data_frame(x = 1, y = 2), data_frame(x = 2, y = 3))))
})

# ------------------------------------------------------------------------------
# iv_proxy()

test_that("proxy of an iv works", {
  x <- new_iv(1L, 2L)
  expect_identical(iv_proxy(x), x)
})

test_that("proxy of a subclass works", {
  iv <- new_iv(1L, 2L)
  x <- new_nested_integer_iv(iv)
  expect_identical(iv_proxy(x), iv)
})

test_that("default proxy error works", {
  expect_snapshot(error = TRUE, iv_proxy(1))
})

# ------------------------------------------------------------------------------
# iv_restore()

test_that("restore of an iv works", {
  x <- new_iv(1L, 2L)
  proxy <- iv_proxy(x)
  expect_identical(iv_restore(proxy, x), x)
})

test_that("restore of a subclass works", {
  iv <- new_iv(1L, 2L)
  x <- new_nested_integer_iv(iv)
  proxy <- iv_proxy(x)
  expect_identical(iv_restore(proxy, x), x)
})

test_that("default restore error works", {
  expect_snapshot(error = TRUE, iv_restore(1, 2))
})

# ------------------------------------------------------------------------------
# iv_start()

test_that("start of an iv works", {
  x <- new_iv(1L, 2L)
  expect_identical(iv_start(x), 1L)
})

test_that("start of a subclass works", {
  iv <- new_iv(1L, 2L)
  x <- new_nested_integer_iv(iv)
  expect_identical(iv_start(x), 1L)
})

# ------------------------------------------------------------------------------
# iv_end()

test_that("end of an iv works", {
  x <- new_iv(1L, 2L)
  expect_identical(iv_end(x), 2L)
})

test_that("end of a subclass works", {
  iv <- new_iv(1L, 2L)
  x <- new_nested_integer_iv(iv)
  expect_identical(iv_end(x), 2L)
})