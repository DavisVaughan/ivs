#' Union
#'
#' @description
#' This family of functions revolves around taking the union of intervals
#' within a single interval vector.
#'
#' - `iv_union()` returns the union of all the intervals in `x`.
#'
#' - `iv_replace_union()` replaces each value in `x` with its union interval.
#'   This is particularly useful alongside [dplyr::group_by()].
#'
#' - `iv_locate_union()` returns locations for slicing the [iv_start()] and
#'   [iv_end()] of `x` to generate the union.
#'
#' - `iv_locate_union_groups()` returns the same locations as
#'   `iv_locate_union()` along with a list-column of group locations that maps
#'   each element of `x` to its union interval.
#'
#' Optionally, you can choose to keep abutting intervals separate with
#' `keep_abutting`, which can be useful if you'd like to retain those
#' boundaries.
#'
#' ## Minimal interval vectors
#'
#' `iv_union()` is particularly useful because it can generate a _minimal_
#' interval vector, which covers the range of an interval vector in the most
#' compact form possible. In particular, a minimal interval vector:
#'
#' - Has no overlapping intervals
#' - Has no abutting intervals
#' - Is ordered on both `start` and `end`
#'
#' A minimal interval vector is allowed to have a single missing interval,
#' which is located at the end of the vector.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @param keep_abutting `[logical(1)]`
#'
#'   Should abutting intervals be kept separate?
#'
#'   If `FALSE`, `[a, b)` and `[b, c)` will join as `[a, c)`. If `TRUE`, they
#'   will be kept separate. To be a minimal interval vector, all abutting
#'   intervals must be merged.
#'
#' @return
#' For `iv_union()`, an iv with the same type as `x`.
#'
#' For `iv_replace_union()`, an iv with the same type and size as `x`.
#'
#' For `iv_locate_union()`, a two column data frame with `start` and `end`
#' integer columns.
#'
#' For `iv_locate_union_groups()`, a two column data frame with a `key` column
#' containing the result of `iv_locate_union()` and a `loc` list-column
#' containing integer vectors.
#'
#' @name iv-union
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' x <- iv_pairs(
#'   c(1, 5),
#'   c(2, 3),
#'   c(NA, NA),
#'   c(5, 6),
#'   c(NA, NA),
#'   c(9, 12),
#'   c(11, 14)
#' )
#' x
#'
#' # The union removes all redundancy while still covering the full range
#' # of values that were originally represented. If any missing intervals
#' # are present, a single one is retained.
#' iv_union(x)
#'
#' # Abutting intervals can be kept separate if you want to
#' # retain those boundaries
#' iv_union(x, keep_abutting = TRUE)
#'
#' # `iv_replace_union()` is useful alongside `group_by()` and `summarize()`
#' df <- tibble(x = x)
#' df <- mutate(df, u = iv_replace_union(x))
#' df
#'
#' df %>%
#'   group_by(u) %>%
#'   summarize(n = n())
#'
#' # The real workhorse here is `iv_locate_union_groups()`, which returns
#' # information on where to slice `x` to get the union, and which observations
#' # of `x` belong to which union interval
#' iv_locate_union_groups(x)
NULL

#' @rdname iv-union
#' @export
iv_union <- function(x, ..., keep_abutting = FALSE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  loc <- vec_interval_locate_minimal(
    start = start,
    end = end,
    keep_abutting = keep_abutting,
    keep_missing = TRUE
  )

  start <- vec_slice(start, loc$start)
  end <- vec_slice(end, loc$end)

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-union
#' @export
iv_replace_union <- function(x, ..., keep_abutting = FALSE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  groups <- vec_interval_locate_minimal_groups(
    start = start,
    end = end,
    keep_abutting = keep_abutting,
    keep_missing = TRUE
  )

  start <- vec_slice(start, groups$key$start)
  end <- vec_slice(end, groups$key$end)

  times <- list_sizes(groups$loc)

  loc <- vec_unchop(groups$loc, ptype = integer(), name_spec = zap())
  loc <- vec_order(loc)

  out <- new_iv(start, end)

  out <- vec_rep_each(out, times = times)
  out <- vec_slice(out, loc)

  out <- iv_restore(out, x)

  out
}

#' @rdname iv-union
#' @export
iv_locate_union <- function(x, ..., keep_abutting = FALSE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  vec_interval_locate_minimal(
    start = start,
    end = end,
    keep_abutting = keep_abutting,
    keep_missing = TRUE
  )
}

#' @rdname iv-union
#' @export
iv_locate_union_groups <- function(x, ..., keep_abutting = FALSE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  vec_interval_locate_minimal_groups(
    start = start,
    end = end,
    keep_abutting = keep_abutting,
    keep_missing = TRUE
  )
}
