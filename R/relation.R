#' Locate relationships between two ivs
#'
#' @description
#' This family of functions locates different types of relationships between
#' two ivs. It works similar to [base::match()], where `needles[i]` checks for
#' a relationship in all of `haystack`. Unlike `match()`, _all_ matching
#' relationships are returned, rather than just the first.
#'
#' - `iv_locate_overlaps()` locates a specific `type` of overlap between the two
#'   ivs.
#'
#' - `iv_locate_precedes()` locates where `needles[i]` precedes (i.e. comes
#'   before) any interval in `haystack`.
#'
#' - `iv_locate_follows()` locates where `needles[i]` follows (i.e. comes
#'   after) any interval in `haystack`.
#'
#' These functions return a two column data frame. The `needles` column is an
#' integer vector pointing to locations in `needles`. The `haystack` column is
#' an integer vector pointing to locations in `haystack` with a matching
#' relationship.
#'
#' ## Missing intervals
#'
#' Unlike `match()`, missing intervals in `needles` force an error to be thrown
#' by default. `match()` matches using equality, so it is typically clear that
#' you also want missing values to match exactly. The relationships implemented
#' here match using inequalities, and it is much less clear what the desired
#' result is for missing intervals. If you have missing intervals that you'd
#' like to match exactly, set `missing = "match"`. If you have missing intervals
#' that you'd like to force to be unmatched, set `missing = NA`.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams vctrs::vec_locate_matches
#'
#' @param needles,haystack `[iv]`
#'
#'   Interval vectors used for relation matching.
#'
#'   * Each element of `needles` represents the interval to search for.
#'
#'   * `haystack` represents the intervals to search in.
#'
#'   Prior to comparison, `needles` and `haystack` are coerced to the same type.
#'
#' @param type `[character(1)]`
#'
#'   The type of relationship to find. One of:
#'
#'   - `"any"`: Finds any overlap whatsoever between an interval in `needles`
#'     and an interval in `haystack`.
#'
#'   - `"within"`: Finds when an interval in `needles` is completely within
#'     (or equal to) an interval in `haystack`.
#'
#'   - `"contains"`: Finds when an interval in `needles` completely contains
#'     (or equals) an interval in `haystack`.
#'
#'   - `"equals"`: Finds when an interval in `needles` is exactly equal to
#'     an interval in `haystack`.
#'
#'   - `"starts"`: Finds when the start of an interval in `needles` matches the
#'     start of an interval in `haystack`.
#'
#'   - `"ends"`: Finds when the end of an interval in `needles` matches the end
#'     of an interval in `haystack`.
#'
#' @param missing `[integer(1) / "match" / "drop" / "error"]`
#'
#'   Handling of missing intervals in `needles`.
#'
#'   - `"match"` matches missing intervals in `needles` to missing intervals in
#'     `haystack`. Missing intervals will be matched exactly, regardless of the
#'     `type`.
#'
#'   - `"drop"` drops missing intervals in `needles` from the result.
#'
#'   - `"error"` throws an error if any intervals in `needles` are missing.
#'     This is the default.
#'
#'   - If a single integer is provided, this represents the value returned in
#'     the `haystack` column for intervals in `needles` that are missing.
#'
#' @param closest `[TRUE / FALSE]`
#'
#'   Should only the closest relationship be returned?
#'
#'   If `TRUE`, will only return the closest interval(s) in `haystack` that
#'   the current value of `needles` either precedes or follows. Note that
#'   multiple intervals can still be returned if there are ties, which can
#'   be resolved using `multiple`.
#'
#' @return
#' A data frame containing two integer columns named `needles` and `haystack`.
#'
#' @seealso
#' [Detecting relationships][relation-detect]
#'
#' [Detecting relationships in parallel][relation-detect-parallel]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' @name relation-locate
#'
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
#' x <- iv_pairs(
#'   as.Date(c("2019-01-05", "2019-01-10")),
#'   as.Date(c("2019-01-07", "2019-01-15")),
#'   as.Date(c("2019-01-20", "2019-01-31"))
#' )
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
#' # Find any overlap between `x` and `y`
#' loc <- iv_locate_overlaps(x, y)
#' loc
#'
#' join(x, y, loc)
#'
#' # Find where `x` contains `y` and drop results when there isn't a match
#' loc <- iv_locate_overlaps(x, y, type = "contains", no_match = "drop")
#' loc
#'
#' join(x, y, loc)
#'
#' # Find where `x` precedes `y`
#' loc <- iv_locate_precedes(x, y)
#' loc
#'
#' join(x, y, loc)
#'
#' # Filter down to find only the closest interval in `y` of all the intervals
#' # where `x` preceded it
#' loc <- iv_locate_precedes(x, y, closest = TRUE)
#'
#' join(x, y, loc)
#'
#' # Note that `closest` can result in duplicates if there is a tie.
#' # `2019-01-20` appears as an end date twice in `haystack`.
#' loc <- iv_locate_follows(x, y, closest = TRUE)
#' loc
#'
#' join(x, y, loc)
#'
#' # Force just one of the ties to be returned by using `multiple`.
#' # Here we just request any of the ties, with no guarantee on which one.
#' loc <- iv_locate_follows(x, y, closest = TRUE, multiple = "any")
#' loc
#'
#' join(x, y, loc)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- iv(NA, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing intervals in `needles` force an error to be thrown
#' try(iv_locate_overlaps(a, b))
#'
#' # If you'd like missing intervals to match exactly, regardless of `type`,
#' # use `missing = "match"`
#' iv_locate_overlaps(a, b, missing = "match")
#'
#' # If you'd like missing intervals in `needles` to always be considered
#' # unmatched, set `missing = NA`
#' iv_locate_overlaps(a, b, missing = NA)
NULL


#' Detect a relationship between two ivs
#'
#' @description
#' This family of functions detects different types of relationships between
#' two ivs. It works similar to [base::%in%], where `needles[i]` checks for
#' a relationship in all of `haystack`.
#'
#' - `iv_detect_overlaps()` detects a specific `type` of overlap between the two
#'   ivs.
#'
#' - `iv_detect_precedes()` detects if `needles[i]` precedes (i.e. comes
#'   before) any interval in `haystack`.
#'
#' - `iv_detect_follows()` detects if `needles[i]` follows (i.e. comes
#'   after) any interval in `haystack`.
#'
#' These functions return a logical vector the same size as `needles` containing
#' `TRUE` if the interval in `needles` has a matching relationship in
#' `haystack` and `FALSE` otherwise.
#'
#' ## Missing intervals
#'
#' Unlike `%in%`, missing intervals in `needles` force an error to be thrown by
#' default. `%in%` detects matches using equality, so it is typically clear that
#' you also want missing values to match exactly. The relationships implemented
#' here match using inequalities, and it is much less clear what the desired
#' result is for missing intervals. If you have missing intervals that you'd
#' like to match exactly, set `missing = "match"`. If you'd like missing
#' intervals to be unmatched, set `missing = FALSE`. If you'd like missing
#' intervals to be propagated, set `missing = NA`.
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param missing `[logical(1) / "match" / "error"]`
#'
#'   Handling of missing intervals in `needles`.
#'
#'   - `"match"` matches missing intervals in `needles` to missing intervals in
#'     `haystack`. Missing intervals will be matched exactly, regardless of the
#'     `type`. Matching missing intervals result in a `TRUE` value in the
#'     result, and unmatched missing intervals result in a `FALSE`.
#'
#'   - `"error"` throws an error if any intervals in `needles` are missing.
#'     This is the default.
#'
#'   - If a single logical value is provided, this represents the value returned
#'     in the result for intervals in `needles` that are missing. You can force
#'     missing intervals to be unmatched by setting this to `FALSE`, and you
#'     can force them to be propagated by setting this to `NA`.
#'
#' @return A logical vector the same size as `needles`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Detecting relationships in parallel][relation-detect-parallel]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' @name relation-detect
#'
#' @examples
#' library(vctrs)
#'
#' x <- iv_pairs(
#'   as.Date(c("2019-01-05", "2019-01-10")),
#'   as.Date(c("2019-01-07", "2019-01-15")),
#'   as.Date(c("2019-01-20", "2019-01-31"))
#' )
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
#' # Does each interval of `x` overlap `y` at all?
#' iv_detect_overlaps(x, y)
#'
#' # Which intervals of `y` are within an interval in `x`?
#' iv_detect_overlaps(y, x, type = "within")
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- iv(c(1, NA), c(2, NA))
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # Missing intervals error by default
#' try(iv_detect_overlaps(a, b))
#'
#' # If you'd like missing intervals to match exactly, set `missing = "match"`
#' iv_detect_overlaps(a, b, missing = "match")
#'
#' # If you'd like missing intervals to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_detect_overlaps(a, b, missing = FALSE)
#'
#' # If you'd like to propagate missing intervals, set `missing = NA`
#' iv_detect_overlaps(a, b, missing = NA)
NULL


#' Detect a relationship in parallel between two ivs
#'
#' @description
#' This family of functions detects different types of relationships between
#' two ivs _in parallel_, where parallel means that the i-th interval of
#' `x` is compared against the i-th interval of `y`. This is in contrast to
#' [iv_detect_overlaps()], which works more like [base::%in%].
#'
#' - `iv_detect_parallel_overlaps()` detects a specific `type` of overlap
#'   between the i-th interval of `x` and the i-th interval of `y`.
#'
#' - `iv_detect_parallel_precedes()` detects if the i-th interval of `x`
#'   precedes (i.e. comes before) the i-th interval of `y`.
#'
#' - `iv_detect_parallel_follows()` detects if the i-th interval of `x`
#'   follows (i.e. comes after) the i-th interval of `y`.
#'
#' These functions return a logical vector the same size as the common size of
#' `x` and `y`.
#'
#' ## Missing intervals
#'
#' Missing intervals in `x` or `y` force an error to be thrown by default, as
#' it is unclear what the desired result is when missing intervals are involved.
#' This is consistent with [iv_detect_overlaps()]. If you'd like to match
#' missing intervals exactly, set `missing = "match"`. If you'd like missing
#' intervals to be unmatched, set `missing = FALSE`. If you'd like missing
#' intervals to be propagated, set `missing = NA`.
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param x,y `[iv]`
#'
#'   A pair of interval vectors.
#'
#'   These will be recycled against each other and cast to the same type.
#'
#' @param missing `[logical(1) / "match" / "error"]`
#'
#'   Handling of missing intervals in `x` or `y`.
#'
#'   - `"match"` matches missing intervals in `x` to missing intervals in
#'     `y`. Regardless of `type`, if both intervals are missing, then this
#'     results in `TRUE`, otherwise it results in `FALSE`.
#'
#'   - `"error"` throws an error if any intervals are missing.
#'     This is the default.
#'
#'   - If a single logical value is provided, this represents the value returned
#'     in the i-th element of the result if missing intervals are present in the
#'     i-th interval of `x` or `y`. You can force missing intervals to be
#'     unmatched by setting this to `FALSE`, and you can force them to be
#'     propagated by setting this to `NA`.
#'
#' @return A logical vector the same size as the common size of `x` and `y`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Detecting relationships][relation-detect]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' @name relation-detect-parallel
#'
#' @examples
#' library(vctrs)
#'
#' x <- iv_pairs(
#'   as.Date(c("2019-01-05", "2019-01-10")),
#'   as.Date(c("2019-01-07", "2019-01-15")),
#'   as.Date(c("2019-01-20", "2019-01-31"))
#' )
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
#' # Does the i-th interval of `x` overlap the i-th interval of `y`?
#' iv_detect_parallel_overlaps(x, y)
#'
#' # Does the i-th interval of `x` contain the i-th interval of `y`?
#' iv_detect_parallel_overlaps(x, y, type = "contains")
#'
#' # Does the i-th interval of `x` follow the i-th interval of `y`?
#' iv_detect_parallel_follows(x, y)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- iv(c(1, NA), c(2, NA))
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # Missing intervals error by default
#' try(iv_detect_parallel_overlaps(a, b))
#'
#' # If you'd like missing intervals to match exactly, set `missing = "match"`
#' iv_detect_parallel_overlaps(a, b, missing = "match")
#'
#' # If you'd like missing intervals to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_detect_parallel_overlaps(a, b, missing = FALSE)
#'
#' # If you'd like to propagate missing intervals, set `missing = NA`
#' iv_detect_parallel_overlaps(a, b, missing = NA)
NULL

# ------------------------------------------------------------------------------

#' @rdname relation-locate
#' @export
iv_locate_overlaps <- function(needles,
                               haystack,
                               ...,
                               type = "any",
                               missing = "error",
                               no_match = NA_integer_,
                               remaining = "drop",
                               multiple = "all") {
  check_dots_empty0(...)

  args <- vec_cast_common(needles = needles, haystack = haystack)
  needles <- args[[1L]]
  haystack <- args[[2L]]

  args <- iv_prepare_overlaps(needles, haystack, type)
  needles <- args$needles
  haystack <- args$haystack
  condition <- args$condition

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

#' @rdname relation-detect
#' @export
iv_detect_overlaps <- function(needles,
                               haystack,
                               ...,
                               type = "any",
                               missing = "error") {
  check_dots_empty0(...)
  iv_detect_impl(needles, haystack, type, missing, iv_prepare_overlaps)
}

#' @rdname relation-detect-parallel
#' @export
iv_detect_parallel_overlaps <- function(x,
                                        y,
                                        ...,
                                        type = "any",
                                        missing = "error") {
  check_dots_empty0(...)
  iv_detect_parallel_impl(x, y, type, missing, iv_prepare_overlaps)
}

iv_prepare_overlaps <- function(needles, haystack, type) {
  type <- arg_match0(type, c("any", "equals", "contains", "within", "starts", "ends"))

  needles_proxy <- iv_proxy(needles)
  haystack_proxy <- iv_proxy(haystack)

  needles_start <- field_start(needles_proxy)
  haystack_start <- field_start(haystack_proxy)

  needles_end <- field_end(needles_proxy)
  haystack_end <- field_end(haystack_proxy)

  if (type == "any") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_end, b = haystack_start)
    condition <- c("<", ">")
  }

  if (type == "equals") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("==", "==")
  }

  if (type == "contains") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("<=", ">=")
  }

  if (type == "within") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c(">=", "<=")
  }

  if (type == "starts") {
    needles <- data_frame(a = needles_start)
    haystack <- data_frame(a = haystack_start)
    condition <- "=="
  }

  if (type == "ends") {
    needles <- data_frame(a = needles_end)
    haystack <- data_frame(a = haystack_end)
    condition <- "=="
  }

  list(needles = needles, haystack = haystack, condition = condition)
}

# ------------------------------------------------------------------------------

#' @rdname relation-locate
#' @export
iv_locate_precedes <- function(needles,
                               haystack,
                               ...,
                               closest = FALSE,
                               missing = "error",
                               no_match = NA_integer_,
                               remaining = "drop",
                               multiple = "all") {
  check_dots_empty0(...)

  iv_locate_positional(
    needles = needles,
    haystack = haystack,
    type = "precedes",
    closest = closest,
    missing = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  )
}

#' @rdname relation-locate
#' @export
iv_locate_follows <- function(needles,
                              haystack,
                              ...,
                              closest = FALSE,
                              missing = "error",
                              no_match = NA_integer_,
                              remaining = "drop",
                              multiple = "all") {
  check_dots_empty0(...)

  iv_locate_positional(
    needles = needles,
    haystack = haystack,
    type = "follows",
    closest = closest,
    missing = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  )
}

iv_locate_positional <- function(needles,
                                 haystack,
                                 type,
                                 closest,
                                 missing,
                                 no_match,
                                 remaining,
                                 multiple) {
  args <- vec_cast_common(needles = needles, haystack = haystack)
  needles <- args[[1L]]
  haystack <- args[[2L]]

  needles <- iv_proxy(needles)
  haystack <- iv_proxy(haystack)

  args <- iv_prepare_positional(needles, haystack, type)
  needles <- args$needles
  haystack <- args$haystack
  condition <- args$condition

  if (!is_bool(closest)) {
    abort("`closest` must be a single `TRUE` or `FALSE`.")
  }

  filter <- "none"

  if (closest) {
    if (type == "precedes") {
      filter <- "min"
    } else if (type == "follows") {
      filter <- "max"
    } else {
      abort("Unknown `type`.", .internal = TRUE)
    }
  }

  with_relation_errors(vec_locate_matches(
    needles = needles,
    haystack = haystack,
    filter = filter,
    condition = condition,
    incomplete = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  ))
}

#' @rdname relation-detect
#' @export
iv_detect_precedes <- function(needles,
                               haystack,
                               ...,
                               missing = "error") {
  check_dots_empty0(...)

  iv_detect_positional(
    needles = needles,
    haystack = haystack,
    type = "precedes",
    missing = missing
  )
}

#' @rdname relation-detect
#' @export
iv_detect_follows <- function(needles,
                              haystack,
                              ...,
                              missing = "error") {
  check_dots_empty0(...)

  iv_detect_positional(
    needles = needles,
    haystack = haystack,
    type = "follows",
    missing = missing
  )
}

iv_detect_positional <- function(needles,
                                 haystack,
                                 type,
                                 missing) {
  iv_detect_impl(needles, haystack, type, missing, iv_prepare_positional)
}

#' @rdname relation-detect-parallel
#' @export
iv_detect_parallel_precedes <- function(x,
                                        y,
                                        ...,
                                        missing = "error") {
  check_dots_empty0(...)

  iv_detect_parallel_positional(
    x = x,
    y = y,
    type = "precedes",
    missing = missing
  )
}

#' @rdname relation-detect-parallel
#' @export
iv_detect_parallel_follows <- function(x,
                                       y,
                                       ...,
                                       missing = "error") {
  check_dots_empty0(...)

  iv_detect_parallel_positional(
    x = x,
    y = y,
    type = "follows",
    missing = missing
  )
}

iv_detect_parallel_positional <- function(x,
                                          y,
                                          type,
                                          missing) {
  iv_detect_parallel_impl(x, y, type, missing, iv_prepare_positional)
}


iv_prepare_positional <- function(needles, haystack, type) {
  needles_proxy <- iv_proxy(needles)
  haystack_proxy <- iv_proxy(haystack)

  needles_start <- field_start(needles_proxy)
  haystack_start <- field_start(haystack_proxy)

  needles_end <- field_end(needles_proxy)
  haystack_end <- field_end(haystack_proxy)

  if (type == "precedes") {
    needles <- data_frame(a = needles_end)
    haystack <- data_frame(a = haystack_start)
    condition <- "<="
  }

  if (type == "follows") {
    needles <- data_frame(a = needles_start)
    haystack <- data_frame(a = haystack_end)
    condition <- ">="
  }

  list(needles = needles, haystack = haystack, condition = condition)
}

# ------------------------------------------------------------------------------

#' Locate relations from Allen's Interval Algebra
#'
#' @description
#' `iv_locate_relation()` is similar to [iv_locate_overlaps()], but it locates a
#' specific set of relations developed by James Allen in the paper:
#' [Maintaining Knowledge about Temporal Intervals](http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf).
#'
#' @section Allen's Interval Algebra:
#'
#' The interval algebra developed by James Allen serves as a basis and
#' inspiration for [iv_locate_overlaps()], [iv_locate_precedes()], and
#' [iv_locate_follows()]. The original algebra is composed of 13 relations
#' which have the following properties:
#'
#' - Distinct: No pair of intervals can be related by more than one `type`.
#'
#' - Exhaustive: All pairs of intervals are described by one of the `type`s.
#'
#' - Qualitative: No numeric intervals are considered. The relationships are
#'   computed by purely qualitative means.
#'
#' Take the notation that `x` and `y` represent two intervals. Now assume that
#' `x` can be represented as `[x_s, x_e)`, where `x_s` is the start of the
#' interval and `x_e` is the end of it. Additionally, assume that `x_s < x_e`.
#' With this notation, the 13 relations are as follows:
#'
#' - _Precedes_:
#'
#'   `x_e < y_s`
#'
#' - _Preceded-by_:
#'
#'   `x_s > y_e`
#'
#' - _Meets_:
#'
#'   `x_e == y_s`
#'
#' - _Met-by_:
#'
#'   `x_s == y_e`
#'
#' - _Overlaps_:
#'
#'   `(x_s < y_s) & (x_e > y_s) & (x_e < y_e)`
#'
#' - _Overlapped-by_:
#'
#'   `(x_e > y_e) & (x_s < y_e) & (x_s > y_s)`
#'
#' - _Starts_:
#'
#'   `(x_s == y_s) & (x_e < y_e)`
#'
#' - _Started-by_:
#'
#'   `(x_s == y_s) & (x_e > y_e)`
#'
#' - _Finishes_:
#'
#'   `(x_s > y_s) & (x_e == y_e)`
#'
#' - _Finished-by_:
#'
#'   `(x_s < y_s) & (x_e == y_e)`
#'
#' - _During_:
#'
#'   `(x_s > y_s) & (x_e < y_e)`
#'
#' - _Contains_:
#'
#'   `(x_s < y_s) & (x_e > y_e)`
#'
#' - _Equals_:
#'
#'   `(x_s == y_s) & (x_e == y_e)`
#'
#' ## Connection to other functions
#'
#' Note that some of the above relations are fairly restrictive. For example,
#' `"overlaps"` only detects cases where `x` straddles `y_s`. It does not
#' consider the case where `x` and `y` are equal to be an overlap (as this
#' is `"equals"`) nor does it consider when `x` straddles `y_e` to be an
#' overlap (as this is `"overlapped-by"`). This makes the relations extremely
#' useful from a theoretical perspective, because they can be combined without
#' fear of duplicating relations, but they don't match our typical expectations
#' for what an "overlap" is.
#'
#' [iv_locate_overlaps()], [iv_locate_precedes()], and [iv_locate_follows()] use
#' more intuitive `type`s that aren't distinct, but typically match your
#' expectations better. They can each be expressed in terms of Allen's
#' relations:
#'
#' - `iv_locate_overlaps()`:
#'
#'   - `"any"`:
#'
#'     `
#'     overlaps |
#'     overlapped-by |
#'     starts |
#'     started-by |
#'     finishes |
#'     finished-by |
#'     during |
#'     contains |
#'     equals
#'     `
#'
#'   - `"contains"`:
#'
#'     `
#'     contains |
#'     started-by |
#'     finished-by |
#'     equals
#'     `
#'
#'   - `"within"`:
#'
#'     `
#'     during |
#'     starts |
#'     finishes |
#'     equals
#'     `
#'
#'   - `"starts"`:
#'
#'     `
#'     starts |
#'     started-by |
#'     equals
#'     `
#'
#'   - `"ends"`:
#'
#'     `
#'     finishes |
#'     finished-by |
#'     equals
#'     `
#'
#'   - `"equals"`:
#'
#'     `
#'     equals
#'     `
#'
#' - `iv_locate_precedes()`:
#'
#'   `
#'   precedes |
#'   meets
#'   `
#'
#' - `iv_locate_follows()`:
#'
#'   `
#'   preceded-by |
#'   met-by
#'   `
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param type `[character(1)]`
#'
#'   The type of relationship to find. See the Allen's Interval Algebra section
#'   for a complete description of each type. One of:
#'
#'   - `"precedes"`
#'   - `"preceded-by"`
#'   - `"meets"`
#'   - `"met-by"`
#'   - `"overlaps"`
#'   - `"overlapped-by"`
#'   - `"starts"`
#'   - `"started-by"`
#'   - `"during"`
#'   - `"contains"`
#'   - `"finishes"`
#'   - `"finished-by"`
#'   - `"equals"`
#'
#' @inherit iv_locate_overlaps return
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Detecting relations from Allen's Interval Algebra][allen-relation-detect]
#'
#' [Detecting relations from Allen's Interval Algebra in parallel][allen-relation-detect-parallel]
#'
#' @references
#' Allen, James F. (26 November 1983). "Maintaining knowledge about temporal
#' intervals". Communications of the ACM. 26 (11): 832–843.
#'
#' @name allen-relation-locate
#' @export
#' @examples
#' x <- iv(1, 3)
#' y <- iv(3, 4)
#'
#' # `"precedes"` is strict, and doesn't let the endpoints match
#' iv_locate_relation(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_locate_relation(x, y, type = "meets")
#'
#' # `"overlaps"` is a very specific type of overlap where an interval in
#' # `needles` straddles the start of an interval in `haystack`
#' x <- iv_pairs(c(1, 4), c(1, 3), c(0, 3), c(2, 5))
#' y <- iv(1, 4)
#'
#' # It doesn't match equality, or when the starts match, or when the end
#' # of the interval in `haystack` is straddled instead
#' iv_locate_relation(x, y, type = "overlaps")
iv_locate_relation <- function(needles,
                               haystack,
                               ...,
                               type,
                               missing = "error",
                               no_match = NA_integer_,
                               remaining = "drop",
                               multiple = "all") {
  check_dots_empty0(...)

  args <- vec_cast_common(needles = needles, haystack = haystack)
  needles <- args[[1L]]
  haystack <- args[[2L]]

  args <- iv_prepare_relation(needles, haystack, type)
  needles <- args$needles
  haystack <- args$haystack
  condition <- args$condition

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

#' Detect relations from Allen's Interval Algebra
#'
#' @description
#' `iv_detect_relation()` is similar to [iv_detect_overlaps()], but it detects a
#' specific set of relations developed by James Allen in the paper:
#' [Maintaining Knowledge about Temporal Intervals](http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf).
#'
#' @inheritSection allen-relation-locate Allen's Interval Algebra
#'
#' @inheritParams iv_detect_overlaps
#'
#' @param type `[character(1)]`
#'
#'   The type of relationship to find. See the Allen's Interval Algebra section
#'   for a complete description of each type. One of:
#'
#'   - `"precedes"`
#'   - `"preceded-by"`
#'   - `"meets"`
#'   - `"met-by"`
#'   - `"overlaps"`
#'   - `"overlapped-by"`
#'   - `"starts"`
#'   - `"started-by"`
#'   - `"during"`
#'   - `"contains"`
#'   - `"finishes"`
#'   - `"finished-by"`
#'   - `"equals"`
#'
#' @inherit iv_detect_overlaps return
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' [Detecting relations from Allen's Interval Algebra in parallel][allen-relation-detect-parallel]
#'
#' @name allen-relation-detect
#' @export
#' @examples
#' x <- iv(1, 3)
#' y <- iv(3, 4)
#'
#' # `"precedes"` is strict, and doesn't let the endpoints match
#' iv_detect_relation(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_detect_relation(x, y, type = "meets")
#'
#' # `"overlaps"` is a very specific type of overlap where an interval in
#' # `needles` straddles the start of an interval in `haystack`
#' x <- iv_pairs(c(1, 4), c(1, 3), c(0, 3), c(2, 5))
#' y <- iv(1, 4)
#'
#' # It doesn't match equality, or when the starts match, or when the end
#' # of the interval in `haystack` is straddled instead
#' iv_detect_relation(x, y, type = "overlaps")
iv_detect_relation <- function(needles,
                               haystack,
                               ...,
                               type,
                               missing = "error") {
  check_dots_empty0(...)
  iv_detect_impl(needles, haystack, type, missing, iv_prepare_relation)
}

#' Detect relations from Allen's Interval Algebra in parallel
#'
#' @description
#' `iv_detect_parallel_relation()` is similar to
#' [iv_detect_parallel_overlaps()], but it detects a specific set of relations
#' developed by James Allen in the paper: [Maintaining Knowledge about Temporal
#' Intervals](http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf).
#'
#' @inheritSection allen-relation-locate Allen's Interval Algebra
#'
#' @inheritParams iv_detect_parallel_overlaps
#'
#' @param type `[character(1)]`
#'
#'   The type of relationship to find. See the Allen's Interval Algebra section
#'   for a complete description of each type. One of:
#'
#'   - `"precedes"`
#'   - `"preceded-by"`
#'   - `"meets"`
#'   - `"met-by"`
#'   - `"overlaps"`
#'   - `"overlapped-by"`
#'   - `"starts"`
#'   - `"started-by"`
#'   - `"during"`
#'   - `"contains"`
#'   - `"finishes"`
#'   - `"finished-by"`
#'   - `"equals"`
#'
#' @inherit iv_detect_parallel_overlaps return
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' [Detecting relations from Allen's Interval Algebra][allen-relation-detect]
#'
#' @name allen-relation-detect-parallel
#' @export
#' @examples
#' x <- iv_pairs(c(1, 3), c(3, 5))
#' y <- iv_pairs(c(3, 4), c(6, 7))
#'
#' # `"precedes"` is strict, and doesn't let the endpoints match
#' iv_detect_parallel_relation(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_detect_parallel_relation(x, y, type = "meets")
#'
#' # `"during"` only matches when `x` is completely contained in `y`, and
#' # doesn't allow any endpoints to match
#' x <- iv_pairs(c(1, 3), c(4, 5), c(8, 9))
#' y <- iv_pairs(c(1, 4), c(3, 8), c(8, 9))
#'
#' iv_detect_parallel_relation(x, y, type = "during")
iv_detect_parallel_relation <- function(x,
                                        y,
                                        ...,
                                        type,
                                        missing = "error") {
  check_dots_empty0(...)
  iv_detect_parallel_impl(x, y, type, missing, iv_prepare_relation)
}

iv_prepare_relation <- function(needles, haystack, type) {
  type <- arg_match0(
    type,
    c(
      "precedes",
      "preceded-by",
      "meets",
      "met-by",
      "overlaps",
      "overlapped-by",
      "starts",
      "started-by",
      "finishes",
      "finished-by",
      "during",
      "contains",
      "equals"
    )
  )

  needles_proxy <- iv_proxy(needles)
  haystack_proxy <- iv_proxy(haystack)

  needles_start <- field_start(needles_proxy)
  haystack_start <- field_start(haystack_proxy)

  needles_end <- field_end(needles_proxy)
  haystack_end <- field_end(haystack_proxy)

  if (type == "precedes") {
    needles <- data_frame(a = needles_end)
    haystack <- data_frame(a = haystack_start)
    condition <- "<"
  }

  if (type == "preceded-by") {
    needles <- data_frame(a = needles_start)
    haystack <- data_frame(a = haystack_end)
    condition <- ">"
  }

  if (type == "meets") {
    needles <- data_frame(a = needles_end)
    haystack <- data_frame(a = haystack_start)
    condition <- "=="
  }

  if (type == "met-by") {
    needles <- data_frame(a = needles_start)
    haystack <- data_frame(a = haystack_end)
    condition <- "=="
  }

  if (type == "overlaps") {
    needles <- data_frame(a = needles_start, b = needles_end, c = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_start, c = haystack_end)
    condition <- c("<", ">", "<")
  }

  if (type == "overlapped-by") {
    needles <- data_frame(a = needles_end, b = needles_start, c = needles_start)
    haystack <- data_frame(a = haystack_end, b = haystack_end, c = haystack_start)
    condition <- c(">", "<", ">")
  }

  if (type == "starts") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("==", "<")
  }

  if (type == "started-by") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("==", ">")
  }

  if (type == "finishes") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c(">", "==")
  }

  if (type == "finished-by") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("<", "==")
  }

  if (type == "during") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c(">", "<")
  }

  if (type == "contains") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("<", ">")
  }

  if (type == "equals") {
    needles <- data_frame(a = needles_start, b = needles_end)
    haystack <- data_frame(a = haystack_start, b = haystack_end)
    condition <- c("==", "==")
  }

  list(needles = needles, haystack = haystack, condition = condition)
}

# ------------------------------------------------------------------------------

iv_detect_impl <- function(needles,
                           haystack,
                           type,
                           missing,
                           iv_prepare_impl,
                           ...,
                           call = caller_env()) {
  check_dots_empty0(...)

  incomplete <- check_detect_missing(missing)

  args <- vec_cast_common(needles = needles, haystack = haystack)
  needles <- args[[1L]]
  haystack <- args[[2L]]

  args <- iv_prepare_impl(needles, haystack, type)
  needles <- args$needles
  haystack <- args$haystack
  condition <- args$condition

  matches <- with_relation_errors(vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = 0L,
    multiple = "any"
  ), call = call)

  # 0L -> FALSE
  # NA_integer -> NA
  # otherwise -> TRUE
  out <- as.logical(matches$haystack)

  out
}

# Returns an `incomplete` argument value
check_detect_missing <- function(missing) {
  if (identical(missing, "match")) {
    "match"
  } else if (identical(missing, "error")) {
    "error"
  } else if (identical(missing, TRUE)) {
    1L
  } else if (identical(missing, FALSE)) {
    0L
  } else if (identical(missing, NA)) {
    NA_integer_
  } else {
    abort('`missing` must be "match", "error", or a single logical value.')
  }
}

# ------------------------------------------------------------------------------

iv_detect_parallel_impl <- function(x,
                                    y,
                                    type,
                                    missing,
                                    iv_prepare_impl,
                                    ...,
                                    call = caller_env()) {
  check_dots_empty0(...)

  missing <- check_detect_parallel_missing(missing)

  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1L]]
  y <- args[[2L]]

  args <- iv_prepare_impl(x, y, type)
  # https://github.com/r-lib/rlang/issues/1346
  args <- map(args, unname)
  args <- transpose(args)
  args <- map(args, apply_parallel_comparator)

  out <- reduce(args, `&`)
  out <- apply_detect_parallel_missing(out, x, y, missing, call = call)

  out
}

check_detect_parallel_missing <- function(missing) {
  ok <-
    identical(missing, "match") ||
    identical(missing, "error") ||
    identical(missing, TRUE) ||
    identical(missing, FALSE) ||
    identical(missing, NA)

  if (!ok) {
    abort('`missing` must be "match", "error", or a single logical value.')
  }

  missing
}

apply_parallel_comparator <- function(elt) {
  x <- elt$needles
  y <- elt$haystack
  condition <- elt$condition

  compare <- vec_compare(x, y)

  switch(
    condition,
    ">=" = compare >= 0L,
    ">" = compare == 1L,
    "==" = compare == 0L,
    "<" = compare == -1L,
    "<=" = compare <= 0L,
    abort("Unknown `condition`.", .internal = TRUE)
  )
}

apply_detect_parallel_missing <- function(out,
                                          x,
                                          y,
                                          missing,
                                          ...,
                                          call = caller_env()) {
  if (identical(missing, NA)) {
    return(out)
  }

  are_missing <- vec_equal_na(out)

  if (!any(are_missing)) {
    return(out)
  }

  if (is_true(missing) || is_false(missing)) {
    out <- vec_assign(out, are_missing, missing)
    return(out)
  }

  if (identical(missing, "error")) {
    loc <- which(are_missing)[[1]]
    stop_relation_missing_parallel(loc, call = call)
  }

  if (identical(missing, "match")) {
    x_missing <- vec_equal_na(x)
    y_missing <- vec_equal_na(y)

    x_and_y_missing <- x_missing & y_missing
    x_or_y_but_not_both_missing <- xor(x_missing, y_missing)

    if (any(x_and_y_missing)) {
      out <- vec_assign(out, x_and_y_missing, TRUE)
    }
    if (any(x_or_y_but_not_both_missing)) {
      out <- vec_assign(out, x_or_y_but_not_both_missing, FALSE)
    }

    return(out)
  }

  abort("`missing` should have been processed by now.", .internal = TRUE)
}

# ------------------------------------------------------------------------------

with_relation_errors <- function(expr, ..., call = caller_env()) {
  check_dots_empty0(...)

  try_fetch(
    expr,
    vctrs_error_matches_incomplete = function(cnd) rethrow_relation_missing(cnd, call)
  )
}

rethrow_relation_missing <- function(cnd, call) {
  stop_relation_missing(cnd$i, call = call)
}

stop_relation_missing <- function(i, ..., call = caller_env()) {
  check_dots_empty0(...)

  message <- c(
    "Can't have missing values in `needles`.",
    i = glue("A value at location {i} is missing."),
    i = "Use `missing` to control how missing values should be handled if they are expected."
  )

  stop_iv(
    message = message,
    i = i,
    class = "iv_error_relation_missing",
    call = call
  )
}

stop_relation_missing_parallel <- function(i, ..., call = caller_env()) {
  check_dots_empty0(...)

  message <- c(
    "Can't have missing values in `x` or `y`.",
    i = glue("A value at location {i} is missing."),
    i = "Use `missing` to control how missing values should be handled if they are expected."
  )

  stop_iv(
    message = message,
    i = i,
    class = "iv_error_relation_missing_parallel",
    call = call
  )
}
