#' Splits
#'
#' @description
#' This family of functions revolves around splitting an iv on its endpoints,
#' which results in a new iv that is entirely disjoint (i.e. non-overlapping).
#' The intervals in the resulting iv are known as "splits".
#'
#' - `iv_splits()` computes the disjoint splits for `x`.
#'
#' - `iv_identify_splits()` identifies the splits that correspond to each
#' interval in `x`. It replaces `x` with a list of the same size where each
#' element of the list contains the splits that the corresponding interval in
#' `x` overlaps. This is particularly useful alongside [tidyr::unnest()].
#'
#' - `iv_locate_splits()` returns a two column data frame with a `key` column
#' containing the result of `iv_splits()` and a `loc` list-column containing
#' integer vectors that map each interval in `x` to the splits that it overlaps.
#'
#' @section Graphical Representation:
#'
#' Graphically, generating splits looks like:
#'
#' ![](splits.png)
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @param on `[vector / NULL]`
#'
#'   An optional vector of additional values to split on.
#'
#'   This should have the same type as `iv_start(x)`.
#'
#' @return
#' - For `iv_splits()`, an iv with the same type as `x`.
#'
#' - For `iv_identify_splits()`, a list-of containing ivs with the same size as
#' `x`.
#'
#' - For `iv_locate_splits()`, a two column data frame with a `key` column
#' of the same type as `x` and `loc` list-column containing integer vectors.
#'
#' @name iv-splits
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#'
#' # Guests to a party and their arrival/departure times
#' guests <- tibble(
#'   arrive = as.POSIXct(
#'     c("2008-05-20 19:30:00", "2008-05-20 20:10:00", "2008-05-20 22:15:00"),
#'     tz = "UTC"
#'   ),
#'   depart = as.POSIXct(
#'     c("2008-05-20 23:00:00", "2008-05-21 00:00:00", "2008-05-21 00:30:00"),
#'     tz = "UTC"
#'   ),
#'   name = list(
#'     c("Mary", "Harry"),
#'     c("Diana", "Susan"),
#'     "Peter"
#'   )
#' )
#'
#' guests <- unnest(guests, name) %>%
#'   mutate(iv = iv(arrive, depart), .keep = "unused")
#'
#' guests
#'
#' # You can determine the disjoint intervals at which people
#' # arrived/departed with `iv_splits()`
#' iv_splits(guests$iv)
#'
#' # Say you'd like to determine who was at the party at any given time
#' # throughout the night
#' guests <- mutate(guests, splits = iv_identify_splits(iv))
#' guests
#'
#' # Unnest the splits to generate disjoint intervals for each guest
#' guests <- guests %>%
#'   unnest(splits) %>%
#'   select(name, splits)
#'
#' guests
#'
#' # Tabulate who was there at any given time
#' guests %>%
#'   group_by(splits) %>%
#'   summarise(n = n(), who = list(name))
#'
#' # ---------------------------------------------------------------------------
#'
#' x <- iv_pairs(c(1, 5), c(4, 9), c(12, 15))
#' x
#'
#' # You can provide additional singular values to split on with `on`
#' iv_splits(x, on = c(2, 13))
NULL

#' @rdname iv-splits
#' @export
iv_splits <- function(x, ..., on = NULL) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  args <- iv_split_candidates(start, end, on = on)
  candidate_start <- args$start
  candidate_end <- args$end

  needles <- data_frame(start = candidate_start, end = candidate_end)
  haystack <- data_frame(start = end, end = start)

  # Find actual overlaps among all possible candidates
  loc <- vec_locate_matches(
    needles,
    haystack,
    condition = c("<", ">"),
    no_match = "drop",
    multiple = "any",
    incomplete = "match"
  )

  out <- vec_slice(needles, loc$needles)

  out <- new_iv(out$start, out$end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-splits
#' @export
iv_identify_splits <- function(x, ..., on = NULL) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  args <- iv_split_candidates(start, end, on = on)
  candidate_start <- args$start
  candidate_end <- args$end

  needles <- data_frame(start = start, end = end)
  haystack <- data_frame(start = candidate_end, end = candidate_start)

  loc <- vec_locate_matches(
    needles,
    haystack,
    condition = c("<", ">"),
    no_match = "error",
    incomplete = NA_integer_
  )
  # TODO: https://github.com/r-lib/vctrs/issues/1210
  # vec_partition(loc$haystack, vec_identify_runs(loc$needles))
  loc <- vec_split(loc$haystack, loc$needles)

  candidates <- new_iv(candidate_start, candidate_end)
  candidates <- iv_restore(candidates, x)

  out <- vec_chop(candidates, loc$val)
  out <- new_list_of(out, ptype = vec_ptype(candidates))

  out
}

#' @rdname iv-splits
#' @export
iv_locate_splits <- function(x, ..., on = NULL) {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  args <- iv_split_candidates(start, end, on = on)
  candidate_start <- args$start
  candidate_end <- args$end

  needles <- data_frame(start = candidate_start, end = candidate_end)
  haystack <- data_frame(start = end, end = start)

  # Find actual overlaps among all possible candidates
  loc <- vec_locate_matches(
    needles,
    haystack,
    condition = c("<", ">"),
    no_match = "drop",
    incomplete = "match"
  )
  # TODO: https://github.com/r-lib/vctrs/issues/1210
  # vec_partition(loc$haystack, vec_identify_runs(loc$needles))
  loc <- vec_split(loc$haystack, loc$needles)

  key <- vec_slice(needles, loc$key)
  key <- new_iv(key$start, key$end)
  key <- iv_restore(key, x)

  loc <- loc$val

  out <- data_frame(key = key, loc = loc)

  out
}

iv_split_candidates <- function(start, end, ..., on = NULL, call = caller_env()) {
  check_dots_empty0(...)

  on <- vec_cast(on, start, x_arg = "on", to_arg = "iv_start(x)", call = call)

  # Candidates are built from all sorted unique values
  points <- vec_sort(vec_unique(vec_c(start, end, on)))
  size_points <- vec_size(points)

  # If a missing interval is present, it is at the very end.
  # We remove it before proceeding, because this "candidates" computation
  # revolves around computing `start` and `end` locations, where `start < end`
  # is a requirement. In the case of a single missing interval, we end up with
  # 1 unique point (an `NA`), but we still want to keep `[NA, NA)` as an
  # interval candidate so we remove it now and add it back at the end.
  last <- vec_slice(points, size_points)
  any_missing <- any(vec_equal_na(last))
  if (any_missing) {
    points <- vec_slice(points, -size_points)
    size_points <- size_points - 1L
  }

  slice_start <- 1L
  slice_end <- size_points

  loc_start <- seq2(slice_start, slice_end - 1L)
  loc_end <- seq2(slice_start + 1L, slice_end)

  candidate_start <- vec_slice(points, loc_start)
  candidate_end <- vec_slice(points, loc_end)

  if (any_missing) {
    candidate_start <- vec_c(candidate_start, vec_init(candidate_start))
    candidate_end <- vec_c(candidate_end, vec_init(candidate_end))
  }

  list(start = candidate_start, end = candidate_end)
}
