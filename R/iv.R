#' Construct a new iv
#'
#' `new_iv()` is a developer focused function for creating a new interval
#' vector. It does minimal checks on the inputs, for performance.
#'
#' @param start,end `[vector]`
#'
#'   A pair of vectors to represent the bounds of the intervals.
#'
#'   To be a valid interval vector, `start` must be strictly less than `end`,
#'   or both `start` and `end` must be a missing value.
#'
#' @param ... `[name-value pairs]`
#'
#'   Additional named attributes to attach to the result.
#'
#' @param class `[character]`
#'
#'   The name of the subclass to create.
#'
#' @export
#' @examples
#' new_iv(1, 2)
new_iv <- function(start, end, ..., class = character()) {
  fields <- list(start = start, end = end)
  new_rcrd(fields, ..., class = c(class, "iv"))
}
