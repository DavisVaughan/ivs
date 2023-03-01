# ------------------------------------------------------------------------------
# ivs 0.2.0

# ivs 0.2.0: Deprecated

#' Set operations
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favor of their `set_` prefixed equivalents.
#'
#' - `iv_complement()` -> [iv_set_complement()]
#' - `iv_union()` -> [iv_set_union()]
#' - `iv_intersect()` -> [iv_set_intersect()]
#' - `iv_difference()` -> [iv_set_difference()]
#' - `iv_symmetric_difference()` -> [iv_set_symmetric_difference()]
#'
#' @inheritParams iv-sets
#'
#' @name iv-sets-deprecated
#' @keywords internal
#'
NULL

#' @export
#' @rdname iv-sets-deprecated
iv_complement <- function(x, ..., lower = NULL, upper = NULL) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "iv_complement()",
    with = "iv_set_complement()",
    always = TRUE
  )
  iv_set_complement(x, ..., lower = lower, upper = upper)
}

#' @export
#' @rdname iv-sets-deprecated
iv_union <- function(x, y) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "iv_union()",
    with = "iv_set_union()",
    always = TRUE
  )
  iv_set_union(x, y)
}

#' @export
#' @rdname iv-sets-deprecated
iv_intersect <- function(x, y) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "iv_intersect()",
    with = "iv_set_intersect()",
    always = TRUE
  )
  iv_set_intersect(x, y)
}

#' @export
#' @rdname iv-sets-deprecated
iv_difference <- function(x, y) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "iv_difference()",
    with = "iv_set_difference()",
    always = TRUE
  )
  iv_set_difference(x, y)
}
