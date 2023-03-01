#' Pairwise span
#'
#' @description
#' `iv_pairwise_span()` computes the _pairwise_ "span" between the i-th interval
#' of `x` and the i-th interval of `y`. The pairwise span of two intervals is
#' a new interval containing the minimum start and maximum end of the original
#' intervals. It is similar to [iv_pairwise_set_union()], except it fills across
#' gaps.
#'
#' @inheritParams iv_pairwise_set_union
#'
#' @inherit iv_pairwise_set_union return
#'
#' @export
#' @examples
#' x <- iv_pairs(c(1, 3), c(6, 8))
#' y <- iv_pairs(c(5, 7), c(2, 3))
#'
#' # Can't take the set union when there are gaps
#' try(iv_pairwise_set_union(x, y))
#'
#' # But you can compute the span of the intervals
#' iv_pairwise_span(x, y)
iv_pairwise_span <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  start <- vec_pairwise_min(x_start, y_start)
  end <- vec_pairwise_max(x_end, y_end)

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}
