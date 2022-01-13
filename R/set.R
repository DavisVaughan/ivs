#' Complement
#'
#' `iv_complement()` computes the set theoretical complement of an interval
#' vector. The "universe" to compute the complement over is considered to be
#' the minimal and maximal values of `x`, but this can be adjusted with `lower`
#' and `upper`.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @param lower,upper `[vector(1) / NULL]`
#'
#'   Bounds for the universe over which to compute the complement. These should
#'   have the same type as the element type of the interval vector. It is
#'   often useful to expand the universe to, say, `-Inf` to `Inf`.
#'
#' @return An object with the same type as `x` representing its complement.
#'
#' @export
#' @examples
#' x <- new_iv(c(10, 0, 3, -5), c(12, 5, 6, -2))
#' x
#'
#' # Complement contains any values from `[-5, 12)` that aren't represented
#' # in these intervals
#' iv_complement(x)
#'
#' # Expand out the "universe" of possible values
#' iv_complement(x, lower = -Inf)
#' iv_complement(x, lower = -Inf, upper = Inf)
iv_complement <- function(x, ..., lower = NULL, upper = NULL) {
  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  out <- vec_interval_complement(
    start = start,
    end = end,
    ...,
    lower = lower,
    upper = upper
  )

  out <- new_iv(out$start, out$end)
  out <- iv_restore(out, x)

  out
}
