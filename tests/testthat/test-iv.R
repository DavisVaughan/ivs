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
