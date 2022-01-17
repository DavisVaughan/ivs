#' Set theoretical operations
#'
#' @description
#' This family of functions treats ivs as sets. They always compute
#' the [minimal][iv_union()] iv of each input and return a minimal iv.
#'
#' - `iv_set_complement()` takes the complement of a set of intervals. By
#'   default, the minimum and maximum of the inputs define the bounds to take
#'   the complement over, but this can be adjusted with `lower` and `upper`.
#'   Missing intervals are always dropped in the complement.
#'
#' - `iv_set_union()` takes the union of two sets of intervals. It is equivalent
#'   to combining the two vectors together and then calling `iv_union()`.
#'
#' - `iv_set_intersect()` takes the intersection of two sets of intervals.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @param y `[iv]`
#'
#'   An interval vector.
#'
#' @param lower,upper `[vector(1) / NULL]`
#'
#'   Bounds for the universe over which to compute the complement. These should
#'   have the same type as the element type of the interval vector. It is
#'   often useful to expand the universe to, say, `-Inf` to `Inf`.
#'
#' @return
#' For `iv_set_complement()`, a vector of the same type as `x` containing the
#' complement.
#'
#' For all other set operations, a vector of the same type as the common type of
#' `x` and `y` containing the result.
#'
#' @name iv-sets
#'
#' @examples
#' x <- iv_pairs(
#'   c(10, 12),
#'   c(0, 5),
#'   c(NA, NA),
#'   c(3, 6),
#'   c(-5, -2),
#'   c(NA, NA)
#' )
#' x
#'
#' y <- iv_pairs(
#'   c(2, 7),
#'   c(NA, NA),
#'   c(-3, -1),
#'   c(14, 15)
#' )
#' y
#'
#' # Complement contains any values from `[-5, 12)` that aren't represented
#' # in these intervals. Missing intervals are dropped.
#' iv_set_complement(x)
#'
#' # Expand out the "universe" of possible values
#' iv_set_complement(x, lower = -Inf)
#' iv_set_complement(x, lower = -Inf, upper = Inf)
#'
#' # Union returns the minimal iv containing all of the information from
#' # both interval vectors
#' iv_set_union(x, y)
#'
#' # Minimal iv representing the set intersection
#' iv_set_intersect(x, y)
NULL

#' @rdname iv-sets
#' @export
iv_set_complement <- function(x, ..., lower = NULL, upper = NULL) {
  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  out <- vec_interval_complement(
    start = start,
    end = end,
    ...,
    lower = lower,
    upper = upper
  )

  out <- new_iv(out$start, out$end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-sets
#' @export
iv_set_union <- function(x, y) {
  out <- vec_c(x, y)
  iv_union(out)
}

#' @rdname iv-sets
#' @export
iv_set_intersect <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  if (vec_size(x_proxy) == 0L) {
    return(vec_ptype(x))
  }
  if (vec_size(y_proxy) == 0L) {
    return(vec_ptype(x))
  }

  x_missing <- vec_equal_na(x_proxy)
  y_missing <- vec_equal_na(y_proxy)

  if (all(x_missing)) {
    if (any(y_missing)) {
      return(vec_init(x))
    } else {
      return(vec_ptype(x))
    }
  }
  if (all(y_missing)) {
    if (any(x_missing)) {
      return(vec_init(x))
    } else {
      return(vec_ptype(x))
    }
  }

  lower <- min(
    min(field_start(x_proxy), na.rm = TRUE),
    min(field_start(y_proxy), na.rm = TRUE)
  )
  upper <- max(
    max(field_end(x_proxy), na.rm = TRUE),
    max(field_end(y_proxy), na.rm = TRUE)
  )

  x_c <- iv_set_complement(x_proxy, lower = lower, upper = upper)
  y_c <- iv_set_complement(y_proxy, lower = lower, upper = upper)

  u <- iv_set_union(x_c, y_c)

  out <- iv_set_complement(u, lower = lower, upper = upper)

  if (any(x_missing) && any(y_missing)) {
    out <- vec_c(out, vec_init(out))
  }

  out <- iv_restore(out, x)

  out
}
