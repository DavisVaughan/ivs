#' Containers
#'
#' @description
#' This family of functions revolves around computing interval _containers_.
#' A container is defined as the widest interval that isn't contained by any
#' other interval.
#'
#' - `iv_containers()` returns all of the containers found within `x`.
#'
#' - `iv_identify_containers()` identifies the containers that each interval in
#' `x` falls in. It replaces `x` with a list of the same size where each element
#' of the list contains the containers that the corresponding interval in `x`
#' falls in. This is particularly useful alongside [tidyr::unnest()].
#'
#' - `iv_identify_container()` is similar in spirit to
#' `iv_identify_containers()`, but is useful when you suspect that each interval
#' in `x` is contained within exactly 1 container. It replaces `x` with an iv of
#' the same size where each interval is the container that the corresponding
#' interval in `x` falls in. If any interval falls in more than one container,
#' an error is thrown.
#'
#' - `iv_locate_containers()` returns a two column data frame with a `key`
#' column containing the result of `iv_containers()` and a `loc` list-column
#' containing integer vectors that map each interval in `x` to the container
#' that it falls in.
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @return
#' - For `iv_containers()`, an iv with the same type as `x`.
#'
#' - For `iv_identify_containers()`, a list-of containing ivs with the same size
#' as `x`.
#'
#' - For `iv_identify_container()`, an iv with the same type as `x`.
#'
#' - For `iv_locate_containers()`, a two column data frame with a `key` column
#' containing the result of `iv_containers()` and a `loc` list-column containing
#' integer vectors.
#'
#' @name iv-containers
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(tidyr)
#'
#' x <- iv_pairs(
#'   c(4, 6),
#'   c(1, 5),
#'   c(2, 3),
#'   c(NA, NA),
#'   c(NA, NA),
#'   c(9, 12),
#'   c(9, 14)
#' )
#' x
#'
#' # Containers are intervals which aren't contained in any other interval.
#' # They are always returned in ascending order.
#' # If any missing intervals are present, a single one is retained.
#' iv_containers(x)
#'
#' # `iv_identify_container()` is useful alongside `group_by()` and
#' # `summarize()` if you know that each interval is contained within exactly
#' # 1 container
#' df <- tibble(x = x)
#' df <- mutate(df, container = iv_identify_container(x))
#' df
#'
#' df %>%
#'   group_by(container) %>%
#'   summarize(n = n())
#'
#' # If any interval is contained within multiple containers,
#' # then you can't use `iv_identify_container()`
#' y <- c(x, iv_pairs(c(0, 3), c(8, 13)))
#' y
#'
#' try(iv_identify_container(y))
#'
#' # Instead, use `iv_identify_containers()` to identify every container
#' # that each interval falls in
#' df <- tibble(y = y, container = iv_identify_containers(y))
#' df
#'
#' # You can use `tidyr::unchop()` to see the containers that each interval
#' # falls in
#' df %>%
#'   mutate(row = row_number(), .before = 1) %>%
#'   unchop(container)
#'
#' # A more programmatic interface to `iv_identify_containers()` is
#' # `iv_locate_containers()`, which returns the containers you get from
#' # `iv_containers()` alongside the locations in the input that they contain.
#' iv_locate_containers(y)
NULL

#' @rdname iv-containers
#' @export
iv_containers <- function(x) {
  proxy <- iv_proxy(x)
  check_iv(proxy, arg = "x")

  start <- field_start(proxy)
  end <- field_end(proxy)

  loc <- vec_interval_locate_containers(
    start = start,
    end = end
  )

  both <- data_frame(start = start, end = end)
  both <- vec_slice(both, loc)

  start <- both$start
  end <- both$end

  out <- new_iv(start, end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-containers
#' @export
iv_identify_containers <- function(x) {
  containers <- iv_containers(x)

  loc <- iv_locate_overlaps(
    needles = x,
    haystack = containers,
    type = "within"
  )
  # TODO: https://github.com/r-lib/vctrs/issues/1210
  # vec_partition(loc$haystack, vec_identify_runs(loc$needles))
  loc <- vec_split(loc$haystack, by = loc$needles)

  ptype <- vec_ptype(containers)
  ptype <- vec_ptype_finalise(ptype)

  out <- vec_chop(containers, loc$val)
  out <- new_list_of(out, ptype = ptype)

  out
}

#' @rdname iv-containers
#' @export
iv_identify_container <- function(x) {
  containers <- iv_containers(x)

  loc <- with_multiple_containers_errors(iv_locate_overlaps(
    needles = x,
    haystack = containers,
    type = "within",
    relationship = "many-to-one"
  ))

  out <- vec_slice(containers, loc$haystack)

  out
}

#' @rdname iv-containers
#' @export
iv_locate_containers <- function(x) {
  containers <- iv_containers(x)

  loc <- iv_locate_overlaps(
    needles = containers,
    haystack = x,
    type = "contains"
  )
  # TODO: https://github.com/r-lib/vctrs/issues/1210
  # vec_partition(loc$haystack, vec_identify_runs(loc$needles))
  loc <- vec_split(loc$haystack, by = loc$needles)

  key <- vec_slice(containers, loc$key)
  loc <- loc$val

  out <- data_frame(key = key, loc = loc)

  out
}



with_multiple_containers_errors <- function(expr, error_call = caller_env()) {
  withCallingHandlers(
    expr = expr,
    vctrs_error_matches_relationship_many_to_one = function(cnd) {
      stop_multiple_containers(cnd$i, error_call = error_call)
    }
  )
}

stop_multiple_containers <- function(i, error_call = caller_env()) {
  message <- c(
    "Intervals in `x` can't fall within multiple containers.",
    i = glue("Location {i} falls within multiple containers."),
    i = paste0(
      "Use `iv_identify_containers()` to identify all of the containers ",
      "that a particular interval is contained by."
    )
  )

  abort(message, call = error_call)
}
