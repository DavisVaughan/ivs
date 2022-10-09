#' Diff a vector to create an interval vector
#'
#' @description
#' `iv_diff()` is a convenient way to generate an iv from a preexisting vector,
#' as long as that vector is in strictly increasing order. It returns an iv
#' that is 1 element shorter than `x` (unless `x` is already empty).
#'
#' It is particularly useful for creating an iv column from an existing column
#' inside of [dplyr::mutate()], but requires you to explicitly handle padding
#' in that case, see the examples.
#'
#' Missing values are allowed, and will be propagated to each side of the
#' resulting interval after applying the diff.
#'
#' @details
#' `iv_diff()` is inspired by [diff()].
#'
#' @param x `[vector]`
#'
#'   A vector in strictly increasing order.
#'
#' @return
#' An iv using `x` as the inner type, with size equal to
#' `max(0L, vec_size(x) - 1L)`.
#'
#' @export
#' @examples
#' x <- as.Date("2019-01-01") + c(0, 5, 7, 10, 19)
#' x
#'
#' # Notice how the boundaries don't overlap, because the closing `)` aligns
#' # with an opening `[`.
#' iv_diff(x)
#'
#' # Like `iv()`, missing values propagate to both boundaries of the interval.
#' # Before missing value propagation was applied, it looked like this:
#' # [1, NA), [NA, 2), [2, 3)
#' x <- c(1, NA, 2, 3)
#' iv_diff(x)
#'
#' # Values in `x` must be in strictly increasing order to generate a valid
#' # interval vector
#' x <- c(1, 0, 2, 2)
#' try(iv_diff(x))
#'
#' x <- c(1, NA, 0)
#' try(iv_diff(x))
#'
#' # ---------------------------------------------------------------------------
#' # Use with `mutate()`
#'
#' library(dplyr)
#'
#' # `iv_diff()` is useful for converting a pre-existing column into an interval
#' # vector, but you'll need to apply padding to ensure that the size of the
#' # diff-ed result is the same as the number of rows in your data frame. There
#' # are two main ways to pad, which are explored below.
#' df <- tibble(x = c(1, 3, 6))
#'
#' # Pad with a known lower/upper bound
#' df %>% mutate(iv = iv_diff(c(0, x)))
#' df %>% mutate(iv = iv_diff(c(x, Inf)))
#'
#' # Pad with a missing value, which results in a fully missing interval
#' df %>% mutate(iv = iv_diff(c(NA, x)))
#' df %>% mutate(iv = iv_diff(c(x, NA)))
iv_diff <- function(x) {
  vec_assert(x)

  rank <- vec_rank(x, ties = "dense", incomplete = "na")
  is_strictly_increasing <- !is.unsorted(rank, na.rm = TRUE, strictly = TRUE)

  if (!is_strictly_increasing) {
    index <- vec_seq_along(rank)

    complete <- vec_detect_complete(rank)
    rank <- vec_slice(rank, complete)
    index <- vec_slice(index, complete)

    loc <- which(diff(rank) < 1L)
    loc <- loc + 1L
    loc <- index[loc]

    loc <- err_locs(loc)

    abort(c(
      "`x` must be in strictly increasing order.",
      i = glue("`x` is not in strictly increasing order at locations: {loc}.")
    ))
  }

  size <- vec_size(x)

  start <- seq2(1L, size - 1L)
  end <- seq2(2L, size)

  start <- vec_slice(x, start)
  end <- vec_slice(x, end)

  iv(start, end)
}
