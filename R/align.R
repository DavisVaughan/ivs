#' Align after locating relationships
#'
#' @description
#' `iv_align()` will align/join `needles` and `haystack` together using a data
#' frame of `locations`. These `locations` are intended to be the output of one
#' of: [iv_locate_overlaps()], [iv_locate_precedes()], [iv_locate_follows()],
#' [iv_locate_relates()], or [iv_locate_between()].
#'
#' This is mainly a convenience function that slices both `needles` and
#' `haystack` according to those `locations`, and then stores the result
#' in a new two column data frame.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param needles,haystack `[vector]`
#'
#'   Two vectors to align.
#'
#' @param locations `[two-column data frame]`
#'
#'   The data frame of locations returned from one of [iv_locate_overlaps()],
#'   [iv_locate_precedes()], [iv_locate_follows()], [iv_locate_relates()], or
#'   [iv_locate_between()].
#'
#' @return A two column data frame with a `$needles` column containing the
#' sliced version of `needles` and a `$haystack` column containing the sliced
#' version of `haystack`.
#'
#' @export
#' @examples
#' needles <- iv_pairs(c(1, 5), c(3, 7), c(10, 12))
#' haystack <- iv_pairs(c(0, 2), c(4, 6))
#'
#' locations <- iv_locate_overlaps(needles, haystack)
#' iv_align(needles, haystack, locations = locations)
#'
#' locations <- iv_locate_overlaps(needles, haystack, no_match = "drop")
#' iv_align(needles, haystack, locations = locations)
#'
#' needles <- c(1, 15, 4, 11)
#' haystack <- iv_pairs(c(1, 5), c(3, 7), c(10, 12))
#'
#' locations <- iv_locate_between(needles, haystack)
#' iv_align(needles, haystack, locations = locations)
iv_align <- function(needles, haystack, ..., locations) {
  check_dots_empty0(...)
  check_locations(locations)

  needles <- vec_slice(needles, locations[["needles"]])
  haystack <- vec_slice(haystack, locations[["haystack"]])

  data_frame(needles = needles, haystack = haystack)
}

check_locations <- function(locations, ..., call = caller_env()) {
  check_dots_empty0(...)

  if (!is.data.frame(locations)) {
    abort("`locations` must be a data frame.", call = call)
  }
  if (ncol(locations) != 2L) {
    abort("`locations` must be a two column data frame.", call = call)
  }

  if (!has_name(locations, "needles")) {
    abort("`locations` must have a column named \"needles\".", call = call)
  }
  if (!has_name(locations, "haystack")) {
    abort("`locations` must have a column named \"haystack\".", call = call)
  }

  needles <- locations[["needles"]]
  haystack <- locations[["haystack"]]

  if (!is_integer(needles)) {
    abort("`locations$needles` must be an integer vector.", call = call)
  }
  if (!is_integer(haystack)) {
    abort("`locations$haystack` must be an integer vector.", call = call)
  }

  return()
}
