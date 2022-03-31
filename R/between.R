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
#' @inheritParams iv_locate_overlaps
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
#' @param missing `[integer(1) / "equals" / "drop" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"equals"` considers missing values in `needles` as exactly equal
#'     to missing intervals in `haystack` when determining if there is a
#'     matching relationship between them.
#'
#'   - `"drop"` drops missing values in `needles` from the result.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
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
#' [Detect when a vector falls between an iv][iv_between]
#'
#' [Pairwise detect when a vector falls between an iv][iv_pairwise_between]
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
#' # Find any location where `x` is between the intervals in `y`
#' loc <- iv_locate_between(x, y)
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # Drop values in `x` without a match
#' loc <- iv_locate_between(x, y, no_match = "drop")
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` are treated as being exactly
#' # equal to missing intervals in `haystack`, so the missing value in `a` is
#' # considered between the missing interval in `b`.
#' iv_locate_between(a, b)
#'
#' # If you'd like missing values in `needles` to always be considered
#' # unmatched, set `missing = NA`
#' iv_locate_between(a, b, missing = NA)
iv_locate_between <- function(needles,
                              haystack,
                              ...,
                              missing = "equals",
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

  incomplete <- check_locate_missing(missing, "match")

  needles <- data_frame(a = needles, b = needles)
  haystack <- data_frame(a = haystack_start, b = haystack_end)
  condition <- c(">=", "<")

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

#' Count when a vector falls between an iv
#'
#' @description
#' `iv_count_between()` counts instances of when `needles`, a vector, falls
#' between the bounds of `haystack`, an iv. It works similar to [base::match()],
#' where `needles[i]` checks for a match in all of `haystack`.
#'
#' This function returns an integer vector the same size as `needles`
#' containing a count of the times where the `i`-th value of `needles`
#' fell between any interval of `haystack`.
#'
#' @inheritParams iv_locate_between
#'
#' @param missing `[integer(1) / "equals" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"equals"` considers missing values in `needles` as exactly equal
#'     to missing intervals in `haystack` when determining if there is a
#'     matching relationship between them.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
#'
#'   - If a single integer value is provided, this represents the count returned
#'     for a missing value in `needles`. Use `0L` to force missing values
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
#' [Locating where a vector falls between an iv][iv_locate_between]
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
#' # Count the number of times `x` is between the intervals in `y`
#' iv_count_between(x, y)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` are treated as being exactly
#' # equal to missing intervals in `haystack`, so the missing value in `a` is
#' # considered between the missing interval in `b`.
#' iv_count_between(a, b)
#'
#' # If you'd like to propagate missing values, set `missing = NA`
#' iv_count_between(a, b, missing = NA)
#'
#' # If you'd like missing values to be treated as unmatched, set
#' # `missing = 0L`
#' iv_count_between(a, b, missing = 0L)
iv_count_between <- function(needles,
                             haystack,
                             ...,
                             missing = "equals",
                             no_match = 0L) {
  check_dots_empty0(...)

  missing <- check_count_missing(missing)
  no_match <- check_count_no_match(no_match)

  locations <- iv_locate_between(
    needles = needles,
    haystack = haystack,
    missing = translate_count_missing(missing),
    no_match = translate_count_no_match(no_match)
  )

  iv_count_locations(locations, missing, no_match)
}

#' Detect when a vector falls between an iv
#'
#' @description
#' `iv_between()` detects when `needles`, a vector, falls between the
#' bounds of `haystack`, an iv. It works similar to [base::%in%], where
#' `needles[i]` checks for a match in all of `haystack`.
#'
#' This function returns a logical vector the same size as `needles` containing
#' `TRUE` if the value in `needles` is between any interval in `haystack` and
#' `FALSE` otherwise.
#'
#' @inheritParams iv_locate_between
#'
#' @param missing `[logical(1) / "equals" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"equals"` considers missing values in `needles` as exactly equal
#'     to missing intervals in `haystack` when determining if there is a
#'     matching relationship between them. Matched missing values in
#'     `needles` result in a `TRUE` value in the result, and unmatched missing
#'     values result in a `FALSE` value.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
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
#' [Pairwise detect when a vector falls between an iv][iv_pairwise_between]
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
#' iv_between(x, y)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` are treated as being exactly
#' # equal to missing intervals in `haystack`, so the missing value in `a` is
#' # considered between the missing interval in `b`.
#' iv_between(a, b)
#'
#' # If you'd like to propagate missing values, set `missing = NA`
#' iv_between(a, b, missing = NA)
#'
#' # If you'd like missing values to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_between(a, b, missing = FALSE)
iv_between <- function(needles,
                       haystack,
                       ...,
                       missing = "equals") {
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

  incomplete <- check_detect_missing(missing, "match")

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

#' Pairwise detect when a vector falls between an iv
#'
#' @description
#' `iv_pairwise_between()` detects when `x`, a vector, falls between the
#' bounds of `y`, an iv, _pairwise_, where pairwise means that the i-th value
#' of `x` is compared against the i-th interval of `y`. This is in contrast to
#' [iv_between()], which works more like [base::%in%].
#'
#' These functions return a logical vector the same size as the common size of
#' `x` and `y`.
#'
#' @param x,y `[vector, iv]`
#'
#'   `x` should be a vector and `y` should be an iv. `x` should have the same
#'   type as the start/end components of `y`.
#'
#'   These will be recycled against each other.
#'
#' @return A logical vector the same size as the common size of `x` and `y`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating where a vector falls between an iv][iv_locate_between]
#'
#' [Detecting when a vector falls between an iv][iv_between]
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
#' iv_pairwise_between(x, y)
#'
#' a <- c(1, NA, NA)
#' b <- iv_pairs(c(NA, NA), c(3, 4), c(NA, NA))
#'
#' # Missing intervals always propagate
#' iv_pairwise_between(a, b)
iv_pairwise_between <- function(x, y) {
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

  out
}
