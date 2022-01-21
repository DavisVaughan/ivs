#' Locate where a vector falls between an iv
#'
#' @description
#' `iv_locate_between()` locates where `needles`, a vector, falls between the
#' bounds of `haystack`, an iv. It works similar to [base::match()], where
#' `needles[i]` checks for a match in all of `haystack`. Unlike `match()`, _all_
#' matches are returned, rather than just the first.
#'
#' This function returns a two column data frame. The `needles` column is an
#' integer vector pointing to locations in `needles`. The `haystack` column is
#' an integer vector pointing to locations in `haystack` with a match.
#'
#' ## Missing values
#'
#' Unlike `match()`, missing values in `needles` force an error to be thrown
#' by default. `match()` matches using equality, so it is typically clear that
#' you also want missing values to match exactly. The relationships implemented
#' here match using inequalities, and it is much less clear what the desired
#' result is for missing values. If you have missing values that you'd
#' like to match exactly, set `missing = "match"`. If you have missing values
#' that you'd like to force to be unmatched, set `missing = NA`.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams vctrs::vec_locate_matches
#'
#' @param needles,haystack `[vector, iv]`
#'
#'   `needles` should be a vector and `haystack` should be an iv. `needles`
#'   should have the same type as the start/end components of `haystack`.
#'
#'   * Each element of `needles` represents the value to search for.
#'
#'   * `haystack` represents the intervals to search in.
#'
#' @param missing `[integer(1) / "match" / "drop" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"match"` matches missing values in `needles` to missing intervals in
#'     `haystack`. Missing values will be matched exactly.
#'
#'   - `"drop"` drops missing values in `needles` from the result.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
#'     This is the default.
#'
#'   - If a single integer is provided, this represents the value returned in
#'     the `haystack` column for values in `needles` that are missing.
#'
#' @return
#' A data frame containing two integer columns named `needles` and `haystack`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Detect when a vector falls between an iv][iv_detect_between]
#'
#' [Detecting when a vector falls between an iv in parallel][iv_detect_parallel_between]
#'
#' @export
#' @examples
#' library(vctrs)
#'
#' join <- function(x, y, loc) {
#'   data_frame(
#'     x = vec_slice(x, loc$needles),
#'     y = vec_slice(y, loc$haystack)
#'   )
#' }
#'
#' x <- as.Date(c("2019-01-05", "2019-01-10", "2019-01-07", "2019-01-20"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-04", "2019-01-08")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-10", "2019-01-20")),
#'   as.Date(c("2019-01-15", "2019-01-20"))
#' )
#'
#' x
#' y
#'
#' # Find any location where `x` is between the intervals in `y`
#' loc <- iv_locate_between(x, y)
#' loc
#'
#' join(x, y, loc)
#'
#' # Drop values in `x` without a match
#' loc <- iv_locate_between(x, y, no_match = "drop")
#' loc
#'
#' join(x, y, loc)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(NA, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` force an error to be thrown
#' try(iv_locate_between(a, b))
#'
#' # If you'd like missing values to match exactly, regardless of `type`,
#' # use `missing = "match"`
#' iv_locate_between(a, b, missing = "match")
#'
#' # If you'd like missing values in `needles` to always be considered
#' # unmatched, set `missing = NA`
#' iv_locate_between(a, b, missing = NA)
iv_locate_between <- function(needles,
                              haystack,
                              ...,
                              missing = "error",
                              no_match = NA_integer_,
                              remaining = "drop",
                              multiple = "all") {
  check_dots_empty0(...)

  haystack <- iv_proxy(haystack)
  haystack_start <- field_start(haystack)
  haystack_end <- field_end(haystack)

  ptype <- vec_ptype2(
    needles,
    haystack_start,
    x_arg = "needles",
    y_arg = "iv_start(haystack)"
  )

  args <- vec_cast_common(
    needles,
    haystack_start,
    haystack_end,
    .to = ptype
  )
  needles <- args[[1L]]
  haystack_start <- args[[2L]]
  haystack_end <- args[[3L]]

  needles <- data_frame(a = needles, b = needles)
  haystack <- data_frame(a = haystack_start, b = haystack_end)
  condition <- c(">=", "<")

  with_relation_errors(vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  ))
}

#' Detect when a vector falls between an iv
#'
#' @description
#' `iv_detect_between()` detects when `needles`, a vector, falls between the
#' bounds of `haystack`, an iv. It works similar to [base::%in%], where
#' `needles[i]` checks for a match in all of `haystack`.
#'
#' This function returns a logical vector the same size as `needles` containing
#' `TRUE` if the value in `needles` is between any interval in `haystack` and
#' `FALSE` otherwise.
#'
#' ## Missing values
#'
#' Unlike `%in%`, missing values in `needles` force an error to be thrown by
#' default. `%in%` detects matches using equality, so it is typically clear that
#' you also want missing values to match exactly. The relationships implemented
#' here match using inequalities, and it is much less clear what the desired
#' result is for missing values. If you have missing values that you'd like to
#' match exactly, set `missing = "match"`. If you'd like missing values to be
#' unmatched, set `missing = FALSE`. If you'd like missing values to be
#' propagated, set `missing = NA`.
#'
#' @inheritParams iv_locate_between
#'
#' @param missing `[logical(1) / "match" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"match"` matches missing values in `needles` to missing values in
#'     `haystack`. Missing values will be matched exactly, regardless of the
#'     `type`. Matching missing values result in a `TRUE` value in the
#'     result, and unmatched missing values result in a `FALSE`.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
#'     This is the default.
#'
#'   - If a single logical value is provided, this represents the value returned
#'     in the result for values in `needles` that are missing. You can force
#'     missing values to be unmatched by setting this to `FALSE`, and you
#'     can force them to be propagated by setting this to `NA`.
#'
#' @return A logical vector the same size as `needles`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating where a vector falls between an iv][iv_locate_between]
#'
#' [Detecting when a vector falls between an iv in parallel][iv_detect_parallel_between]
#'
#' @export
#' @examples
#' x <- as.Date(c("2019-01-05", "2019-01-10", "2019-01-07", "2019-01-20"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-04", "2019-01-08")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-10", "2019-01-20")),
#'   as.Date(c("2019-01-15", "2019-01-20"))
#' )
#'
#' x
#' y
#'
#' # Detect if any location where `x` is between the intervals in `y`
#' iv_detect_between(x, y)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # Missing values error by default
#' try(iv_detect_between(a, b))
#'
#' # If you'd like missing values to match exactly, set `missing = "match"`
#' iv_detect_between(a, b, missing = "match")
#'
#' # If you'd like missing values to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_detect_between(a, b, missing = FALSE)
#'
#' # If you'd like to propagate missing values, set `missing = NA`
#' iv_detect_between(a, b, missing = NA)
iv_detect_between <- function(needles,
                              haystack,
                              ...,
                              missing = "error") {
  check_dots_empty0(...)

  haystack <- iv_proxy(haystack)
  haystack_start <- field_start(haystack)
  haystack_end <- field_end(haystack)

  ptype <- vec_ptype2(
    needles,
    haystack_start,
    x_arg = "needles",
    y_arg = "iv_start(haystack)"
  )

  args <- vec_cast_common(
    needles,
    haystack_start,
    haystack_end,
    .to = ptype
  )
  needles <- args[[1L]]
  haystack_start <- args[[2L]]
  haystack_end <- args[[3L]]

  needles <- data_frame(a = needles, b = needles)
  haystack <- data_frame(a = haystack_start, b = haystack_end)
  condition <- c(">=", "<")

  incomplete <- check_detect_missing(missing)

  matches <- with_relation_errors(vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = 0L,
    multiple = "any"
  ))

  # 0L -> FALSE
  # NA_integer -> NA
  # otherwise -> TRUE
  out <- as.logical(matches$haystack)

  out
}

#' Detect when a vector falls between an iv in parallel
#'
#' @description
#' `iv_detect_parallel_between()` detects when `x`, a vector, falls between the
#' bounds of `y`, an iv, _in parallel_, where parallel means that the i-th value
#' of `x` is compared against the i-th interval of `y`. This is in contrast to
#' [iv_detect_between()], which works more like [base::%in%].
#'
#' These functions return a logical vector the same size as the common size of
#' `x` and `y`.
#'
#' ## Missing values
#'
#' Missing values in `x` or `y` force an error to be thrown by default, as it is
#' unclear what the desired result is when missing values are involved. This is
#' consistent with [iv_detect_between()]. If you'd like to match missing values
#' exactly, set `missing = "match"`. If you'd like missing values to be
#' unmatched, set `missing = FALSE`. If you'd like missing values to be
#' propagated, set `missing = NA`.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams iv_detect_between
#'
#' @param x,y `[vector, iv]`
#'
#'   `x` should be a vector and `y` should be an iv. `x` should have the same
#'   type as the start/end components of `y`.
#'
#'   These will be recycled against each other.
#'
#' @param missing `[logical(1) / "match" / "error"]`
#'
#'   Handling of missing values in `x` or `y`.
#'
#'   - `"match"` matches missing values in `x` to missing intervals in
#'     `y`. If both are missing, then this results in `TRUE`, otherwise it
#'     results in `FALSE`.
#'
#'   - `"error"` throws an error if any values are missing.
#'     This is the default.
#'
#'   - If a single logical value is provided, this represents the value returned
#'     in the i-th element of the result if missing values are present in the
#'     i-th values of `x` or `y`. You can force missing values to be
#'     unmatched by setting this to `FALSE`, and you can force them to be
#'     propagated by setting this to `NA`.
#'
#' @return A logical vector the same size as the common size of `x` and `y`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating where a vector falls between an iv][iv_locate_between]
#'
#' [Detecting when a vector falls between an iv][iv_detect_between]
#'
#' @export
#' @examples
#' x <- as.Date(c("2019-01-01", "2019-01-08", "2019-01-21"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-18", "2019-01-21"))
#' )
#'
#' x
#' y
#'
#' # Does the i-th value of `x` fall between the i-th interval of `y`?
#' iv_detect_parallel_between(x, y)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # Missing values error by default
#' try(iv_detect_parallel_between(a, b))
#'
#' # If you'd like missing values to match exactly, set `missing = "match"`
#' iv_detect_parallel_between(a, b, missing = "match")
#'
#' # If you'd like missing values to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_detect_parallel_between(a, b, missing = FALSE)
#'
#' # If you'd like to propagate missing values, set `missing = NA`
#' iv_detect_parallel_between(a, b, missing = NA)
iv_detect_parallel_between <- function(x,
                                       y,
                                       ...,
                                       missing = "error") {
  check_dots_empty0(...)

  missing <- check_detect_parallel_missing(missing)

  args <- vec_recycle_common(x = x, y = y)
  x <- args[[1L]]
  y <- args[[2L]]

  y <- iv_proxy(y)
  y_start <- field_start(y)
  y_end <- field_end(y)

  ptype <- vec_ptype2(
    x,
    y_start,
    x_arg = "x",
    y_arg = "iv_start(y)"
  )

  args <- vec_cast_common(
    x,
    y_start,
    y_end,
    .to = ptype
  )
  x <- args[[1L]]
  y_start <- args[[2L]]
  y_end <- args[[3L]]

  after_start <- vec_compare(x, y_start) >= 0L
  before_end <- vec_compare(x, y_end) < 0L

  out <- after_start & before_end
  out <- apply_detect_parallel_missing(out, x, y, missing)

  out
}
