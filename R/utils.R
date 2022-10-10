vec_paste0 <- function(...) {
  # Use tidyverse recycling rules to avoid size zero recycling bugs
  args <- vec_recycle_common(...)
  exec(paste0, !!!args)
}

err_locs <- function(x) {
  if (!is.integer(x)) {
    abort("`x` must be an integer vector of locations.", .internal = TRUE)
  }

  size <- length(x)

  if (size == 0L) {
    abort("`x` must have at least 1 location.", .internal = TRUE)
  } else if (size == 1L) {
    glue("`{x}`")
  } else if (size <= 5L) {
    x <- glue_collapse(x, sep = ", ")
    glue("`c({x})`")
  } else {
    x <- x[1:5]
    x <- glue_collapse(x, sep = ", ")
    glue("`c({x})` and {size - 5L} more")
  }
}
