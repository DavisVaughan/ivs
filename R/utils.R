vec_paste0 <- function(...) {
  # Use tidyverse recycling rules to avoid size zero recycling bugs
  args <- vec_recycle_common(...)
  exec(paste0, !!!args)
}

stop_iv <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  abort(message, class = c(class, "iv_error"), ..., call = call)
}

err_locs <- function(x) {
  if (!is.integer(x)) {
    abort("`x` must be an integer vector of locations.", .internal = TRUE)
  }

  size <- length(x)

  if (size == 0L) {
    abort("`x` must have at least 1 location.", .internal = TRUE)
  }

  if (size > 5L) {
    x <- x[1:5]
    extra <- glue(" and {size - 5L} more")
  } else {
    extra <- ""
  }

  x <- glue_collapse(x, sep = ", ")

  glue("`c({x})`{extra}")
}
