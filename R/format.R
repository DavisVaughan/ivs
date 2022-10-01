#' Formatting
#'
#' @description
#' `iv_format()` is an S3 generic intended as a developer tool for making a
#' custom class print nicely when stored in an iv. The default method simply
#' calls [format()], and in many cases this is enough for most classes.
#' However, if your class automatically adds justification or padding when
#' formatting a single vector, you might need to implement an `iv_format()`
#' method to avoid that padding, since it often looks strange when nested
#' in an interval vector.
#'
#' @param x `[vector]`
#'
#'   A vector to format. This will be called on the [iv_start()] and [iv_end()]
#'   vectors of an iv.
#'
#' @return A character vector, likely generated through a call to `format()`.
#'
#' @export
#' @examples
#' # Numeric values get padding automatically through `format()`
#' x <- c(1, 100)
#' format(x)
#'
#' # This ends up looking strange in an iv, so an `iv_format()` method for
#' # numeric values is implemented which turns off that padding
#' iv_format(x)
iv_format <- function(x) {
  UseMethod("iv_format")
}

#' @export
iv_format.default <- function(x) {
  format(x)
}

#' @export
iv_format.logical <- function(x) {
  format(x, trim = TRUE)
}

#' @export
iv_format.integer <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

#' @export
iv_format.double <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

#' @export
iv_format.character <- function(x) {
  format(x, justify = "none", na.encode = TRUE)
}

#' @export
iv_format.factor <- function(x) {
  format(x, justify = "none", na.encode = TRUE)
}

#' @export
iv_format.data.frame <- function(x) {
  # Printing data would be too complicated,
  # just print a type summary and the row number
  abbr <- vec_ptype_abbr(x, suffix_shape = FALSE)

  out <- vec_paste0(abbr, "[", vec_seq_along(x), ",]")

  missing <- vec_detect_missing(x)
  if (any(missing)) {
    out[missing] <- "NA"
  }

  out
}

#' @export
iv_format.Date <- function(x) {
  format(x, format = "%Y-%m-%d")
}

#' @export
iv_format.POSIXt <- function(x) {
  format(x, format = "%Y-%m-%d %H:%M:%S")
}

#' @export
iv_format.difftime <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

#' @export
iv_format.integer64 <- function(x) {
  # We implement this for convenience, since it won't make it upstream.
  # It goes through `format.character()`, so we use that method.
  iv_format.character(x)
}

# ------------------------------------------------------------------------------

#' @export
format.iv <- function(x, ...) {
  proxy <- iv_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  start <- iv_format(start)
  end <- iv_format(end)

  out <- vec_paste0("[", start, ", ", end, ")")

  out
}
