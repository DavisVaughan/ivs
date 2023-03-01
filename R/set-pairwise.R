#' Pairwise set operations
#'
#' @description
#' This family of functions performs _pairwise_ set operations on two ivs.
#' Pairwise refers to the fact that the i-th interval of `x` is going to be
#' compared against the i-th interval of `y`. This is in contrast to their
#' counterparts, like [iv_set_union()], which treat the entire vector of `x`
#' as a single set to be compared against all of `y`.
#'
#' The descriptions of these operations are the same as their non-pairwise
#' counterparts, but the ones here also have a number of restrictions due to
#' the fact that each must return an output that is the same size as its inputs:
#'
#' - For `iv_pairwise_complement()`, `x[i]` and `y[i]` can't overlap or abut,
#'   as this would generate an empty complement.
#'
#' - For `iv_pairwise_union()`, `x[i]` and `y[i]` can't be separated by a gap.
#'   Use `iv_pairwise_span()` if you want to force gaps to be filled anyways.
#'
#' - For `iv_pairwise_intersect()`, `x[i]` and `y[i]` must overlap, otherwise
#'   an empty interval would be generated.
#'
#' - For `iv_pairwise_difference()`, `x[i]` can't be completely contained
#'   within `y[i]`, as that would generate an empty interval. Additionally,
#'   `y[i]` can't be completely contained within `x[i]`, as that would result
#'   in two distinct intervals for a single observation.
#'
#' - For `iv_pairwise_symmetric_difference()`, `x[i]` and `y[i]` must share
#'   exactly one endpoint, otherwise an empty interval or two distinct intervals
#'   would be generated.
#'
#' @param x,y `[iv]`
#'
#'   A pair of interval vectors.
#'
#'   These will be cast to the same type, and recycled against each other.
#'
#' @return An iv the same size and type as `x` and `y`.
#'
#' @seealso The non-pairwise versions of these functions, such as
#' [iv_set_union()].
#'
#' @name iv-set-pairwise
#'
#' @examples
#' x <- iv_pairs(c(1, 3), c(6, 8))
#' y <- iv_pairs(c(5, 7), c(2, 3))
#'
#' iv_pairwise_complement(x, y)
#'
#' z <- iv_pairs(c(2, 5), c(4, 7))
#'
#' iv_pairwise_union(x, z)
#'
#' # Can't take the union when there are gaps
#' try(iv_pairwise_union(x, y))
#'
#' # But you can force a union across gaps with `iv_pairwise_span()`
#' iv_pairwise_span(x, y)
#'
#' iv_pairwise_intersect(x, z)
#'
#' # Can't take an intersection of non-overlapping intervals
#' try(iv_pairwise_intersect(x, y))
#'
#' iv_pairwise_difference(x, z)
#'
#' # The pairwise symmetric difference function is fairly strict,
#' # and is only well defined when exactly one of the interval endpoints match
#' w <- iv_pairs(c(1, 6), c(7, 8))
#' iv_pairwise_symmetric_difference(x, w)
NULL

#' @rdname iv-set-pairwise
#' @export
iv_pairwise_complement <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  end <- vec_pairwise_max(x_start, y_start)
  start <- vec_pairwise_min(x_end, y_end)

  overlaps_or_abuts <- start >= end
  if (any(overlaps_or_abuts, na.rm = TRUE)) {
    loc <- which(overlaps_or_abuts)[[1]]

    abort(c(
      "Can't take the complement of overlapping or abutting intervals.",
      i = glue("Location {loc} contains overlapping or abutting intervals.")
    ))
  }

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-set-pairwise
#' @export
iv_pairwise_union <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  compare_start <- vec_compare(x_start, y_start)
  compare_end <- vec_compare(x_end, y_end)

  max_start <- vec_pairwise_max(x_start, y_start, compare = compare_start)
  min_end <- vec_pairwise_min(x_end, y_end, compare = compare_end)

  has_gap <- vec_compare(max_start, min_end) == 1L
  if (any(has_gap, na.rm = TRUE)) {
    loc <- which(has_gap)[[1]]

    abort(c(
      "Can't take the union of intervals containing a gap.",
      i = glue("Location {loc} contains a gap."),
      i = "Use `iv_pairwise_span()` to combine across gaps."
    ))
  }

  start <- vec_pairwise_min(x_start, y_start, compare = compare_start)
  end <- vec_pairwise_max(x_end, y_end, compare = compare_end)

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-set-pairwise
#' @export
iv_pairwise_span <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  start <- vec_pairwise_min(x_start, y_start)
  end <- vec_pairwise_max(x_end, y_end)

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-set-pairwise
#' @export
iv_pairwise_intersect <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  start <- vec_pairwise_max(x_start, y_start)
  end <- vec_pairwise_min(x_end, y_end)

  non_overlapping <- vec_compare(start, end) >= 0L
  if (any(non_overlapping, na.rm = TRUE)) {
    loc <- which(non_overlapping)[[1]]

    abort(c(
      "Can't take the intersection of non-overlapping intervals.",
      i = "This would result in an empty interval.",
      i = glue("Location {loc} contains non-overlapping intervals.")
    ))
  }

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-set-pairwise
#' @export
iv_pairwise_difference <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  compare_start <- vec_compare(x_start, y_start)
  compare_end <- vec_compare(x_end, y_end)

  y_contained <- (compare_start == -1L) & (compare_end == 1L)
  if (any(y_contained, na.rm = TRUE)) {
    loc <- which(y_contained)[[1]]

    abort(c(
      "Can't compute a difference when `y` is completely contained within `x`.",
      i = "This would result in two distinct intervals for a single observation.",
      i = glue("Location {loc} contains this issue.")
    ))
  }

  y_contains <- (compare_start >= 0L) & (compare_end <= 0L)
  if (any(y_contains, na.rm = TRUE)) {
    loc <- which(y_contains)[[1]]

    abort(c(
      "Can't compute a difference when `y` completely contains `x`.",
      i = "This would result in an empty interval.",
      i = glue("Location {loc} contains this issue.")
    ))
  }

  start <- x_start
  end <- x_end

  max_start <- vec_pairwise_max(x_start, y_start, compare = compare_start)
  min_end <- vec_pairwise_min(x_end, y_end, compare = compare_end)

  update <- vec_compare(max_start, min_end) < 0L
  direction <- vec_equal(min_end, x_end)

  clamp_end <- update & direction
  if (any(clamp_end, na.rm = TRUE)) {
    end <- vec_assign(end, clamp_end, vec_slice(max_start, clamp_end))
  }

  clamp_start <- update & !direction
  if (any(clamp_start, na.rm = TRUE)) {
    start <- vec_assign(start, clamp_start, vec_slice(min_end, clamp_start))
  }

  if (anyNA(update)) {
    # Ensure missings in `y` get propagated
    missing <- vec_detect_missing(update)
    start <- vec_assign(start, missing, NA)
    end <- vec_assign(end, missing, NA)
  }

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-set-pairwise
#' @export
iv_pairwise_symmetric_difference <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  compare_start <- vec_compare(x_start, y_start)
  compare_end <- vec_compare(x_end, y_end)

  equal_start <- compare_start == 0L
  equal_end <- compare_end == 0L

  has_equal_start_or_end_but_not_both <- xor(equal_start, equal_end)
  if (!all(has_equal_start_or_end_but_not_both, na.rm = TRUE)) {
    loc <- which(!has_equal_start_or_end_but_not_both)[[1]]
    eq <- equal_start[[loc]] && equal_end[[loc]]

    if (eq) {
      abort(c(
        "Can't compute a symmetric difference when `x` and `y` are equal.",
        i = "This would result in an empty interval.",
        i = glue("Location {loc} contains this issue.")
      ))
    } else {
      abort(c(
        "Can't compute a symmetric difference when `x` and `y` don't share an endpoint.",
        i = "This would result in two distinct intervals for a single observation.",
        i = glue("Location {loc} contains this issue.")
      ))
    }
  }

  size <- vec_size(x_start)
  start <- vec_init(x_start, n = size)
  end <- vec_init(x_start, n = size)

  if (any(equal_start, na.rm = TRUE)) {
    equal_start <- which(equal_start)
    compare_end <- vec_slice(compare_end, equal_start)

    new_start <- vec_pairwise_min(
      vec_slice(x_end, equal_start),
      vec_slice(y_end, equal_start),
      compare = compare_end
    )
    new_end <- vec_pairwise_max(
      vec_slice(x_end, equal_start),
      vec_slice(y_end, equal_start),
      compare = compare_end
    )

    start <- vec_assign(start, equal_start, new_start)
    end <- vec_assign(end, equal_start, new_end)
  }

  if (any(equal_end, na.rm = TRUE)) {
    equal_end <- which(equal_end)
    compare_start <- vec_slice(compare_start, equal_end)

    new_start <- vec_pairwise_min(
      vec_slice(x_start, equal_end),
      vec_slice(y_start, equal_end),
      compare = compare_start
    )
    new_end <- vec_pairwise_max(
      vec_slice(x_start, equal_end),
      vec_slice(y_start, equal_end),
      compare = compare_start
    )

    start <- vec_assign(start, equal_end, new_start)
    end <- vec_assign(end, equal_end, new_end)
  }

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

# ------------------------------------------------------------------------------

vec_pairwise_min <- function(x, y, ..., compare = NULL) {
  vec_pairwise_summary(x, y, type = "min", compare = compare)
}
vec_pairwise_max <- function(x, y, ..., compare = NULL) {
  vec_pairwise_summary(x, y, type = "max", compare = compare)
}
vec_pairwise_summary <- function(x, y, type, ..., compare = NULL) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  if (is.null(compare)) {
    compare <- vec_compare(x, y)
  }

  if (type == "min") {
    x_wins <- compare <= 0L
    y_wins <- !x_wins
  } else if (type == "max") {
    x_wins <- compare >= 0L
    y_wins <- !x_wins
  } else {
    abort("Unknown `type`.")
  }

  # Assign mins and maxes, propagate missings through `vec_init()`
  out <- vec_init(x, vec_size(x))
  out <- vec_assign(out, x_wins, vec_slice(x, x_wins))
  out <- vec_assign(out, y_wins, vec_slice(y, y_wins))

  out
}
