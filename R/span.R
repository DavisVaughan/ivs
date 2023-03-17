#' Span
#'
#' @description
#' `iv_span()` computes the span of an iv. The span is a single interval which
#' encompasses the entire range of the iv. It is similar to [iv_groups()], if
#' groups were also merged across gaps.
#'
#' `iv_span()` is a _summary_ function, like [min()] and [max()], so it always
#' returns a size 1 iv, even for empty ivs. The `empty` argument can be used to
#' control what is returned in the empty case.
#'
#' @details
#' `iv_span()` is currently limited by the fact that it calls [min()] and
#' [max()] internally, which doesn't work for all vector types that ivs
#' supports (mainly data frames). In the future, we hope to be able to leverage
#' `vctrs::vec_min()` and `vctrs::vec_max()`, which don't exist yet.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams iv_groups
#'
#' @param missing `["propagate" / "drop" / "error" / iv(1)]`
#'
#'   Handling of missing intervals in `x`.
#'
#'   - `"propagate"` forces `iv_span()` to return a missing interval if any
#'     missing intervals are detected in `x`.
#'
#'   - `"drop"` drops missing intervals before computing the span. If this
#'     results in an empty vector, then `empty` will be applied.
#'
#'   - `"error"` throws an error if any missing intervals are detected.
#'
#'   - If an iv of size 1 is supplied, then this is returned if any missing
#'     intervals are detected. It is cast to the type of `x` before returning.
#'
#' @param empty `["missing" / "error" / iv(1)]`
#'
#'   Handling of empty `x` vectors.
#'
#'   - `"missing"` forces `iv_span()` to return a missing interval if `x` is
#'     empty.
#'
#'   - `"error"` throws an error if `x` is empty.
#'
#'   - If an iv of size 1 is supplied, then this is returned if `x` is empty. It
#'     is cast to the type of `x` before returning.
#'
#' @export
#' @examples
#' x <- iv_pairs(c(1, 5), c(2, 6), c(9, 10))
#'
#' # The span covers the full range of values seen in `x`
#' iv_span(x)
#'
#' # Compare against `iv_groups()`, which merges overlaps but doesn't merge
#' # across gaps
#' iv_groups(x)
#'
#' x <- iv_pairs(c(1, 3), c(NA, NA), c(5, 6), c(NA, NA))
#'
#' # Because `iv_span()` is a summary function, if any missing intervals are
#' # present then it returns a missing interval by default
#' iv_span(x)
#'
#' # Further control this with `missing`
#' iv_span(x, missing = "drop")
#' try(iv_span(x, missing = "error"))
#' iv_span(x, missing = iv(-1, 0))
#'
#' x <- iv(double(), double())
#'
#' # If `x` is empty, then by default a missing interval is returned
#' iv_span(x)
#'
#' # Control this with `empty`
#' try(iv_span(x, empty = "error"))
#' iv_span(x, empty = iv(-Inf, Inf))
#'
#' # `empty` kicks in if `missing = "drop"` is used and all elements were
#' # missing
#' x <- iv(c(NA, NA), c(NA, NA), ptype = double())
#' iv_span(x, missing = "drop", empty = iv(-Inf, Inf))
iv_span <- function(x, ..., missing = "propagate", empty = "missing") {
  check_dots_empty0(...)

  proxy <- iv_proxy(x)
  check_iv(proxy, arg = "x")

  missing <- check_span_missing(missing, x)
  empty <- check_span_empty(empty, x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  if (vec_any_missing(start)) {
    switch(
      missing$string,
      drop = {
        not_missing <- !vec_detect_missing(start)
        not_missing <- which(not_missing)
        start <- vec_slice(start, not_missing)
        end <- vec_slice(end, not_missing)
      },
      propagate = {
        return(vec_init(x))
      },
      value = {
        return(missing$value)
      },
      error = {
        abort("`x` can't contain missing values.")
      }
    )
  }

  if (vec_is_empty(start)) {
    switch(
      empty$string,
      missing = {
        return(vec_init(x))
      },
      value = {
        return(empty$value)
      },
      error = {
        abort("`x` can't be empty.")
      }
    )
  }

  # TODO: `vec_min()` and `vec_max()`
  # https://github.com/r-lib/vctrs/issues/86
  start <- min(start)
  end <- max(end)

  out <- new_bare_iv(start, end)
  out <- iv_restore(out, x)

  out
}

check_span_missing <- function(missing, x, ..., error_call = caller_env()) {
  if (is_string(missing)) {
    missing <- arg_match0(
      arg = missing,
      values = c("propagate", "drop", "error"),
      error_call = error_call
    )

    out <- list(
      string = missing,
      value = NULL
    )

    return(out)
  }

  if (is_iv(missing) || is_iv_extension(missing)) {
    vec_check_size(missing, size = 1L, call = error_call)

    missing <- vec_cast(
      x = missing,
      to = x,
      x_arg = "missing",
      to_arg = "x",
      call = error_call
    )

    out <- list(
      string = "value",
      value = missing
    )

    return(out)
  }

  stop_input_type(missing, what = "a string or an iv", call = error_call)
}

check_span_empty <- function(empty, x, ..., error_call = caller_env()) {
  if (is_string(empty)) {
    empty <- arg_match0(
      arg = empty,
      values = c("missing", "error"),
      error_call = error_call
    )

    out <- list(
      string = empty,
      value = NULL
    )

    return(out)
  }

  if (is_iv(empty) || is_iv_extension(empty)) {
    vec_check_size(empty, size = 1L, call = error_call)

    empty <- vec_cast(
      x = empty,
      to = x,
      x_arg = "empty",
      to_arg = "x",
      call = error_call
    )

    out <- list(
      string = "value",
      value = empty
    )

    return(out)
  }

  stop_input_type(empty, what = "a string or an iv", call = error_call)
}

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
