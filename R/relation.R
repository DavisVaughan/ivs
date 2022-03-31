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
#' @param missing `[integer(1) / "equals" / "drop" / "error"]`
#'
#'   Handling of missing intervals in `needles`.
#'
#'   - `"equals"` considers missing intervals in `needles` as exactly equal
#'     to missing intervals in `haystack` when determining if there is a
#'     matching relationship between them.
#'
#'   - `"drop"` drops missing intervals in `needles` from the result.
#'
#'   - `"error"` throws an error if any intervals in `needles` are missing.
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
#' [Detecting relationships pairwise][relation-detect-pairwise]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' @name relation-locate
#'
#' @examples
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
#' iv_align(x, y, locations = loc)
#'
#' # Find where `x` contains `y` and drop results when there isn't a match
#' loc <- iv_locate_overlaps(x, y, type = "contains", no_match = "drop")
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # Find where `x` precedes `y`
#' loc <- iv_locate_precedes(x, y)
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # Filter down to find only the closest interval in `y` of all the intervals
#' # where `x` preceded it
#' loc <- iv_locate_precedes(x, y, closest = TRUE)
#'
#' iv_align(x, y, locations = loc)
#'
#' # Note that `closest` can result in duplicates if there is a tie.
#' # `2019-01-20` appears as an end date twice in `haystack`.
#' loc <- iv_locate_follows(x, y, closest = TRUE)
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # Force just one of the ties to be returned by using `multiple`.
#' # Here we just request any of the ties, with no guarantee on which one.
#' loc <- iv_locate_follows(x, y, closest = TRUE, multiple = "any")
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- iv(NA, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing intervals in `needles` are seen as exactly equal to
#' # missing intervals in `haystack`, which means that they overlap
#' iv_locate_overlaps(a, b)
#'
#' # If you'd like missing intervals in `needles` to always be considered
#' # unmatched, set `missing = NA`
#' iv_locate_overlaps(a, b, missing = NA)
NULL

#' Count relationships between two ivs
#'
#' @description
#' This family of functions counts different types of relationships between
#' two ivs. It works similar to [base::match()], where `needles[i]` checks for
#' a relationship in all of `haystack`.
#'
#' - `iv_count_overlaps()` counts instances of a specific `type` of overlap
#'   between the two ivs.
#'
#' - `iv_count_precedes()` counts instances when `needles[i]` precedes (i.e.
#'   comes before) any interval in `haystack`.
#'
#' - `iv_count_follows()` counts instances when `needles[i]` follows (i.e.
#'   comes after) any interval in `haystack`.
#'
#' These functions return an integer vector the same size as `needles`
#' containing a count of the times a particular relationship between the `i`-th
#' interval of `needles` and any interval of `haystack` occurred.
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param missing `[integer(1) / "equals" / "error"]`
#'
#'   Handling of missing intervals in `needles`.
#'
#'   - `"equals"` considers missing intervals in `needles` as exactly equal
#'     to missing intervals in `haystack` when determining if there is a
#'     matching relationship between them.
#'
#'   - `"error"` throws an error if any intervals in `needles` are missing.
#'
#'   - If a single integer value is provided, this represents the count returned
#'     for a missing interval in `needles`. Use `0L` to force missing intervals
#'     to never match.
#'
#' @param no_match `[integer(1) / "error"]`
#'
#'   Handling of `needles` without a match.
#'
#'   - `"error"` throws an error if any needles have zero matches.
#'
#'   - If a single integer is provided, this represents the count returned for
#'     a needle with zero matches. The default value gives unmatched needles
#'     a count of `0L`.
#'
#' @return An integer vector the same size as `needles`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' @name relation-count
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
#' # Count the number of times `x` overlaps `y` at all
#' iv_count_overlaps(x, y)
#'
#' # Count the number of times `y` is within an interval in `x`
#' iv_count_overlaps(y, x, type = "within")
#'
#' # Count the number of times `x` precedes `y`
#' iv_count_precedes(x, y)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- iv(c(1, NA), c(2, NA))
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # Missing intervals are seen as exactly equal by default, so they are
#' # considered to overlap
#' iv_count_overlaps(a, b)
#'
#' # If you'd like missing intervals to be treated as unmatched, set
#' # `missing = 0L`
#' iv_count_overlaps(a, b, missing = 0L)
#'
#' # If you'd like to propagate missing intervals, set `missing = NA`
#' iv_count_overlaps(a, b, missing = NA)
NULL

#' Detect a relationship between two ivs
#'
#' @description
#' This family of functions detects different types of relationships between
#' two ivs. It works similar to [base::%in%], where `needles[i]` checks for
#' a relationship in all of `haystack`.
#'
#' - `iv_overlaps()` detects a specific `type` of overlap between the two ivs.
#'
#' - `iv_precedes()` detects if `needles[i]` precedes (i.e. comes before) any
#'   interval in `haystack`.
#'
#' - `iv_follows()` detects if `needles[i]` follows (i.e. comes after) any
#'   interval in `haystack`.
#'
#' These functions return a logical vector the same size as `needles` containing
#' `TRUE` if the interval in `needles` has a matching relationship in
#' `haystack` and `FALSE` otherwise.
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param missing `[logical(1) / "equals" / "error"]`
#'
#'   Handling of missing intervals in `needles`.
#'
#'   - `"equals"` considers missing intervals in `needles` as exactly equal
#'     to missing intervals in `haystack` when determining if there is a
#'     matching relationship between them. Matched missing intervals in
#'     `needles` result in a `TRUE` value in the result, and unmatched missing
#'     intervals result in a `FALSE` value.
#'
#'   - `"error"` throws an error if any intervals in `needles` are missing.
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
#' [Detecting relationships pairwise][relation-detect-pairwise]
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
#' iv_overlaps(x, y)
#'
#' # Which intervals of `y` are within an interval in `x`?
#' iv_overlaps(y, x, type = "within")
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- iv(c(1, NA), c(2, NA))
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # Missing intervals are seen as exactly equal by default, so they are
#' # considered to overlap
#' iv_overlaps(a, b)
#'
#' # If you'd like missing intervals to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_overlaps(a, b, missing = FALSE)
#'
#' # If you'd like to propagate missing intervals, set `missing = NA`
#' iv_overlaps(a, b, missing = NA)
NULL


#' Pairwise detect a relationship between two ivs
#'
#' @description
#' This family of functions detects different types of relationships between
#' two ivs _pairwise_, where pairwise means that the i-th interval of
#' `x` is compared against the i-th interval of `y`. This is in contrast to
#' [iv_overlaps()], which works more like [base::%in%].
#'
#' - `iv_pairwise_overlaps()` detects a specific `type` of overlap
#'   between the i-th interval of `x` and the i-th interval of `y`.
#'
#' - `iv_pairwise_precedes()` detects if the i-th interval of `x`
#'   precedes (i.e. comes before) the i-th interval of `y`.
#'
#' - `iv_pairwise_follows()` detects if the i-th interval of `x`
#'   follows (i.e. comes after) the i-th interval of `y`.
#'
#' These functions return a logical vector the same size as the common size of
#' `x` and `y`.
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param x,y `[iv]`
#'
#'   A pair of interval vectors.
#'
#'   These will be recycled against each other and cast to the same type.
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
#' @name relation-detect-pairwise
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
#' iv_pairwise_overlaps(x, y)
#'
#' # Does the i-th interval of `x` contain the i-th interval of `y`?
#' iv_pairwise_overlaps(x, y, type = "contains")
#'
#' # Does the i-th interval of `x` follow the i-th interval of `y`?
#' iv_pairwise_follows(x, y)
#'
#' a <- iv_pairs(c(1, 2), c(NA, NA), c(NA, NA))
#' b <- iv_pairs(c(NA, NA), c(3, 4), c(NA, NA))
#'
#' # Missing intervals always propagate
#' iv_pairwise_overlaps(a, b)
NULL

# ------------------------------------------------------------------------------

#' @rdname relation-locate
#' @export
iv_locate_overlaps <- function(needles,
                               haystack,
                               ...,
                               type = "any",
                               missing = "equals",
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

  incomplete <- check_locate_missing(missing, "match")

  vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  )
}

#' @rdname relation-count
#' @export
iv_count_overlaps <- function(needles,
                              haystack,
                              ...,
                              type = "any",
                              missing = "equals",
                              no_match = 0L) {
  check_dots_empty0(...)

  missing <- check_count_missing(missing)
  no_match <- check_count_no_match(no_match)

  locations <- iv_locate_overlaps(
    needles = needles,
    haystack = haystack,
    type = type,
    missing = translate_count_missing(missing),
    no_match = translate_count_no_match(no_match)
  )

  iv_count_locations(locations, missing, no_match)
}

#' @rdname relation-detect
#' @export
iv_overlaps <- function(needles,
                        haystack,
                        ...,
                        type = "any",
                        missing = "equals") {
  check_dots_empty0(...)
  incomplete <- check_detect_missing(missing, "match")
  iv_detect_impl(needles, haystack, type, incomplete, iv_prepare_overlaps)
}

#' @rdname relation-detect-pairwise
#' @export
iv_pairwise_overlaps <- function(x,
                                 y,
                                 ...,
                                 type = "any") {
  check_dots_empty0(...)
  iv_detect_pairwise_impl(x, y, type, iv_prepare_overlaps)
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
                               missing = "equals",
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
                              missing = "equals",
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

  # In the case of `equals`, missing values will never match,
  # so we just force the equivalent `no_match` value.
  incomplete <- check_locate_missing(missing, no_match)

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

  vec_locate_matches(
    needles = needles,
    haystack = haystack,
    filter = filter,
    condition = condition,
    incomplete = incomplete,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  )
}

#' @rdname relation-count
#' @export
iv_count_precedes <- function(needles,
                              haystack,
                              ...,
                              closest = FALSE,
                              missing = "equals",
                              no_match = 0L) {
  check_dots_empty0(...)

  iv_count_positional(
    needles = needles,
    haystack = haystack,
    type = "precedes",
    closest = closest,
    missing = missing,
    no_match = no_match
  )
}

#' @rdname relation-count
#' @export
iv_count_follows <- function(needles,
                             haystack,
                             ...,
                             closest = FALSE,
                             missing = "equals",
                             no_match = 0L) {
  check_dots_empty0(...)

  iv_count_positional(
    needles = needles,
    haystack = haystack,
    type = "follows",
    closest = closest,
    missing = missing,
    no_match = no_match
  )
}

iv_count_positional <- function(needles,
                                haystack,
                                type,
                                closest,
                                missing,
                                no_match,
                                ...,
                                call = caller_env()) {
  if (type == "precedes") {
    iv_locate_fn <- iv_locate_precedes
  } else if (type == "follows") {
    iv_locate_fn <- iv_locate_follows
  } else {
    abort("Unknown `type`.", .internal = TRUE)
  }

  missing <- check_count_missing(missing, call = call)
  no_match <- check_count_no_match(no_match, call = call)

  locations <- iv_locate_fn(
    needles = needles,
    haystack = haystack,
    closest = closest,
    missing = translate_count_missing(missing),
    no_match = translate_count_no_match(no_match)
  )

  iv_count_locations(locations, missing, no_match)
}

#' @rdname relation-detect
#' @export
iv_precedes <- function(needles,
                        haystack,
                        ...,
                        missing = "equals") {
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
iv_follows <- function(needles,
                       haystack,
                       ...,
                       missing = "equals") {
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
  # In the case of `equals`, missing values will never match,
  # so we force a `0L` which results in `FALSE` for missings.
  incomplete <- check_detect_missing(missing, 0L)
  iv_detect_impl(needles, haystack, type, incomplete, iv_prepare_positional)
}

#' @rdname relation-detect-pairwise
#' @export
iv_pairwise_precedes <- function(x, y) {
  iv_detect_pairwise_positional(
    x = x,
    y = y,
    type = "precedes"
  )
}

#' @rdname relation-detect-pairwise
#' @export
iv_pairwise_follows <- function(x, y) {
  iv_detect_pairwise_positional(
    x = x,
    y = y,
    type = "follows"
  )
}

iv_detect_pairwise_positional <- function(x,
                                          y,
                                          type) {
  iv_detect_pairwise_impl(x, y, type, iv_prepare_positional)
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

  list(
    needles = needles,
    haystack = haystack,
    condition = condition
  )
}

# ------------------------------------------------------------------------------

#' Locate relations from Allen's Interval Algebra
#'
#' @description
#' `iv_locate_relates()` is similar to [iv_locate_overlaps()], but it locates a
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
#' Note that when `missing = "equals"`, missing intervals will only match
#' the `type = "equals"` relation. This ensures that the distinct property
#' of the algebra is maintained.
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
#' [Detecting relations from Allen's Interval Algebra pairwise][allen-relation-detect-pairwise]
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
#' iv_locate_relates(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_locate_relates(x, y, type = "meets")
#'
#' # `"overlaps"` is a very specific type of overlap where an interval in
#' # `needles` straddles the start of an interval in `haystack`
#' x <- iv_pairs(c(1, 4), c(1, 3), c(0, 3), c(2, 5))
#' y <- iv(1, 4)
#'
#' # It doesn't match equality, or when the starts match, or when the end
#' # of the interval in `haystack` is straddled instead
#' iv_locate_relates(x, y, type = "overlaps")
iv_locate_relates <- function(needles,
                              haystack,
                              ...,
                              type,
                              missing = "equals",
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

  equals <- compute_relation_equals(type, no_match)
  incomplete <- check_locate_missing(missing, equals)

  vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple
  )
}

#' Count relations from Allen's Interval Algebra
#'
#' @description
#' `iv_count_relates()` is similar to [iv_count_overlaps()], but it counts a
#' specific set of relations developed by James Allen in the paper:
#' [Maintaining Knowledge about Temporal Intervals](http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf).
#'
#' @inheritSection allen-relation-locate Allen's Interval Algebra
#'
#' @inheritParams iv_count_overlaps
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
#' @inherit iv_count_overlaps return
#'
#' @seealso
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' @name allen-relation-count
#' @export
#' @examples
#' x <- iv(1, 3)
#' y <- iv(3, 4)
#'
#' # `"precedes"` is strict, and doesn't let the endpoints match
#' iv_count_relates(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_count_relates(x, y, type = "meets")
#'
#' # `"overlaps"` is a very specific type of overlap where an interval in
#' # `needles` straddles the start of an interval in `haystack`
#' x <- iv_pairs(c(1, 4), c(1, 3), c(0, 3), c(2, 5))
#' y <- iv(1, 4)
#'
#' # It doesn't match equality, or when the starts match, or when the end
#' # of the interval in `haystack` is straddled instead
#' iv_count_relates(x, y, type = "overlaps")
iv_count_relates <- function(needles,
                             haystack,
                             ...,
                             type,
                             missing = "equals",
                             no_match = 0L) {
  check_dots_empty0(...)

  missing <- check_count_missing(missing)
  no_match <- check_count_no_match(no_match)

  locations <- iv_locate_relates(
    needles = needles,
    haystack = haystack,
    type = type,
    missing = translate_count_missing(missing),
    no_match = translate_count_no_match(no_match)
  )

  iv_count_locations(locations, missing, no_match)
}

#' Detect relations from Allen's Interval Algebra
#'
#' @description
#' `iv_relates()` is similar to [iv_overlaps()], but it detects a
#' specific set of relations developed by James Allen in the paper:
#' [Maintaining Knowledge about Temporal Intervals](http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf).
#'
#' @inheritSection allen-relation-locate Allen's Interval Algebra
#'
#' @inheritParams iv_overlaps
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
#' @inherit iv_overlaps return
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' [Detecting relations from Allen's Interval Algebra pairwise][allen-relation-detect-pairwise]
#'
#' @name allen-relation-detect
#' @export
#' @examples
#' x <- iv(1, 3)
#' y <- iv(3, 4)
#'
#' # `"precedes"` is strict, and doesn't let the endpoints match
#' iv_relates(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_relates(x, y, type = "meets")
#'
#' # `"overlaps"` is a very specific type of overlap where an interval in
#' # `needles` straddles the start of an interval in `haystack`
#' x <- iv_pairs(c(1, 4), c(1, 3), c(0, 3), c(2, 5))
#' y <- iv(1, 4)
#'
#' # It doesn't match equality, or when the starts match, or when the end
#' # of the interval in `haystack` is straddled instead
#' iv_relates(x, y, type = "overlaps")
iv_relates <- function(needles,
                       haystack,
                       ...,
                       type,
                       missing = "equals") {
  check_dots_empty0(...)

  equals <- compute_relation_equals(type, 0L)
  incomplete <- check_detect_missing(missing, equals)

  iv_detect_impl(needles, haystack, type, incomplete, iv_prepare_relation)
}

#' Pairwise detect relations from Allen's Interval Algebra
#'
#' @description
#' `iv_pairwise_relates()` is similar to
#' [iv_pairwise_overlaps()], but it detects a specific set of relations
#' developed by James Allen in the paper: [Maintaining Knowledge about Temporal
#' Intervals](http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf).
#'
#' @inheritSection allen-relation-locate Allen's Interval Algebra
#'
#' @inheritParams iv_pairwise_overlaps
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
#' @inherit iv_pairwise_overlaps return
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating relations from Allen's Interval Algebra][allen-relation-locate]
#'
#' [Detecting relations from Allen's Interval Algebra][allen-relation-detect]
#'
#' @name allen-relation-detect-pairwise
#' @export
#' @examples
#' x <- iv_pairs(c(1, 3), c(3, 5))
#' y <- iv_pairs(c(3, 4), c(6, 7))
#'
#' # `"precedes"` is strict, and doesn't let the endpoints match
#' iv_pairwise_relates(x, y, type = "precedes")
#'
#' # Since that is what `"meets"` represents
#' iv_pairwise_relates(x, y, type = "meets")
#'
#' # `"during"` only matches when `x` is completely contained in `y`, and
#' # doesn't allow any endpoints to match
#' x <- iv_pairs(c(1, 3), c(4, 5), c(8, 9))
#' y <- iv_pairs(c(1, 4), c(3, 8), c(8, 9))
#'
#' iv_pairwise_relates(x, y, type = "during")
iv_pairwise_relates <- function(x, y, ..., type) {
  check_dots_empty0(...)
  iv_detect_pairwise_impl(x, y, type, iv_prepare_relation)
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

compute_relation_equals <- function(type, no_match) {
  # If `missing = "equals"`, then we only want the `"equals"` relation type
  # to match missing values (to ensure disjoint relations). All others must
  # use the `no_match` value. In particular, we don't want to match during
  # `type = "meets"` or `"met-at"`.
  if (type == "equals") {
    "match"
  } else {
    no_match
  }
}

# ------------------------------------------------------------------------------

# Returns an `incomplete` value
check_locate_missing <- function(missing, equals) {
  if (identical(missing, "equals")) {
    # Map `equals` to the correct `incomplete` value for this usage of `missing`
    equals
  } else {
    # Let `vec_locate_matches()` handle it
    missing
  }
}

# ------------------------------------------------------------------------------

iv_detect_impl <- function(needles,
                           haystack,
                           type,
                           incomplete,
                           iv_prepare_impl,
                           ...,
                           call = caller_env()) {
  check_dots_empty0(...)

  args <- vec_cast_common(needles = needles, haystack = haystack)
  needles <- args[[1L]]
  haystack <- args[[2L]]

  args <- iv_prepare_impl(needles, haystack, type)
  needles <- args$needles
  haystack <- args$haystack
  condition <- args$condition

  matches <- vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = 0L,
    multiple = "any"
  )

  # 0L -> FALSE
  # NA_integer -> NA
  # otherwise -> TRUE
  out <- as.logical(matches$haystack)

  out
}

# Returns an `incomplete` argument value
check_detect_missing <- function(missing, equals) {
  if (identical(missing, "equals")) {
    equals
  } else if (identical(missing, "error")) {
    "error"
  } else if (identical(missing, TRUE)) {
    1L
  } else if (identical(missing, FALSE)) {
    0L
  } else if (identical(missing, NA)) {
    NA_integer_
  } else {
    abort('`missing` must be "equals", "error", or a single logical value.')
  }
}

# ------------------------------------------------------------------------------

iv_detect_pairwise_impl <- function(x,
                                    y,
                                    type,
                                    iv_prepare_impl,
                                    ...,
                                    call = caller_env()) {
  check_dots_empty0(...)

  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args, .call = call)
  args <- vec_recycle_common(!!!args, .call = call)
  x <- args[[1L]]
  y <- args[[2L]]

  args <- iv_prepare_impl(x, y, type)
  # https://github.com/r-lib/rlang/issues/1346
  args <- map(args, unname)
  args <- transpose(args)
  args <- map(args, apply_pairwise_comparator)

  out <- reduce(args, `&`)

  out
}

apply_pairwise_comparator <- function(elt) {
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
