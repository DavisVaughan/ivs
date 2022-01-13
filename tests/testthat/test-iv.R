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
# is_iv()

test_that("can check if an object is an iv", {
  expect_identical(is_iv(1), FALSE)
  expect_identical(is_iv(new_iv(1, 2)), TRUE)
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
