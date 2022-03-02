#' Group overlapping intervals
#'
#' @description
#' This family of functions revolves around grouping overlapping intervals
#' within a single iv. When multiple overlapping intervals are grouped together
#' they result in a wider interval containing the smallest [iv_start()] and the
#' largest [iv_end()] of the overlaps.
#'
#' - `iv_groups()` merges all overlapping intervals found within `x`. The
#' resulting intervals are known as the "groups" of `x`.
#'
#' - `iv_identify_group()` identifies the group that the current interval of `x`
#' falls in. This is particularly useful alongside [dplyr::group_by()].
#'
#' - `iv_locate_groups()` returns a two column data frame with a `key` column
#' containing the result of `iv_groups()` and a `loc` list-column containing
#' integer vectors that map each interval in `x` to the group that it falls in.
#'
#' Optionally, you can choose _not_ to group abutting intervals together with
#' `abutting = FALSE`, which can be useful if you'd like to retain those
#' boundaries.
#'
#' ## Minimal interval vectors
#'
#' `iv_groups()` is particularly useful because it can generate a _minimal_
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
#' @section Graphical Representation:
#'
#' Graphically, generating groups looks like:
#'
#' ![](groups.png)
#'
#' With `abutting = FALSE`, intervals that touch aren't grouped:
#'
#' ![](groups-abutting-keep.png)
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @param abutting `[TRUE / FALSE]`
#'
#'   Should abutting intervals be grouped together?
#'
#'   If `TRUE`, `[a, b)` and `[b, c)` will merge as `[a, c)`. If `FALSE`, they
#'   will be kept separate. To be a minimal interval vector, all abutting
#'   intervals must be grouped together.
#'
#' @return
#' - For `iv_groups()`, an iv with the same type as `x`.
#'
#' - For `iv_identify_group()`, an iv with the same type and size as `x`.
#'
#' - For `iv_locate_groups()`, a two column data frame with a `key` column
#' containing the result of `iv_groups()` and a `loc` list-column containing
#' integer vectors.
#'
#' @name iv-groups
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
#' # Grouping removes all redundancy while still covering the full range
#' # of values that were originally represented. If any missing intervals
#' # are present, a single one is retained.
#' iv_groups(x)
#'
#' # Abutting intervals are typically grouped together, but you can choose not
#' # to group them if you want to retain those boundaries
#' iv_groups(x, abutting = FALSE)
#'
#' # `iv_identify_group()` is useful alongside `group_by()` and `summarize()`
#' df <- tibble(x = x)
#' df <- mutate(df, u = iv_identify_group(x))
#' df
#'
#' df %>%
#'   group_by(u) %>%
#'   summarize(n = n())
#'
#' # The real workhorse here is `iv_locate_groups()`, which returns
#' # the groups and information on which observations in `x` fall in which
#' # group
#' iv_locate_groups(x)
NULL

#' @rdname iv-groups
#' @export
iv_groups <- function(x, ..., abutting = TRUE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  groups <- vec_interval_groups(
    start = start,
    end = end,
    abutting = abutting
  )

  start <- groups$start
  end <- groups$end

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-groups
#' @export
iv_identify_group <- function(x, ..., abutting = TRUE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  groups <- vec_interval_locate_groups(
    start = start,
    end = end,
    abutting = abutting
  )

  times <- list_sizes(groups$loc)

  loc <- vec_unchop(groups$loc, ptype = integer(), name_spec = zap())
  loc <- vec_order(loc)

  start <- groups$key$start
  end <- groups$key$end

  out <- new_iv(start, end)

  out <- vec_rep_each(out, times = times)
  out <- vec_slice(out, loc)

  out <- iv_restore(out, x)

  out
}

#' @rdname iv-groups
#' @export
iv_locate_groups <- function(x, ..., abutting = TRUE) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  out <- vec_interval_locate_groups(
    start = start,
    end = end,
    abutting = abutting
  )

  start <- out$key$start
  end <- out$key$end

  key <- new_iv(start, end)
  key <- iv_restore(key, x)

  out[["key"]] <- key

  out
}
