# An internal helper class used for testing,
# but this lives here so we can register the S3 methods

nested_integer_iv <- function(start, end) {
  out <- iv(start, end, ptype = integer())
  new_nested_integer_iv(out)
}

nested_integer_iv_pairs <- function(...) {
  out <- iv_pairs(..., ptype = integer())
  new_nested_integer_iv(out)
}

new_nested_integer_iv <- function(iv, ..., class = character()) {
  if (!is_iv(iv)) {
    abort("`iv` must be an <iv>.")
  }
  if (!is_bare_integer(iv_start(iv))) {
    abort("`iv_start(iv)` must be a bare integer.")
  }
  if (!is_bare_integer(iv_end(iv))) {
    abort("`iv_end(iv)` must be a bare integer.")
  }

  fields <- list(iv = iv)

  new_rcrd(fields, ..., class = c(class, "nested_integer_iv"))
}

# Proxy and restore recursively through `iv_proxy()`

#' @export
vec_proxy.nested_integer_iv <- function(x, ...) {
  vec_proxy(iv_proxy(x), ...)
}
#' @export
vec_restore.nested_integer_iv <- function(x, to, ...) {
  out <- vec_restore(x, iv_proxy(to), ...)
  iv_restore(out, to)
}

#' @export
vec_ptype_full.nested_integer_iv <- function(x, ...) {
  "nested_integer_iv"
}

#' @export
format.nested_integer_iv <- function(x, ...) {
  format(field(x, "iv"), ...)
}

#' @export
vec_ptype2.nested_integer_iv.nested_integer_iv <- function(x, y, ...) {
  iv <- new_iv(start = integer(), end = integer())
  new_nested_integer_iv(iv = iv)
}

#' @export
vec_cast.nested_integer_iv.nested_integer_iv <- function(x, to, ...) {
  x
}

#' @export
iv_proxy.nested_integer_iv <- function(x, ...) {
  field(x, "iv")
}

#' @export
iv_restore.nested_integer_iv <- function(x, to, ...) {
  new_nested_integer_iv(x)
}
