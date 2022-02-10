vec_paste0 <- function(...) {
  # Use tidyverse recycling rules to avoid size zero recycling bugs
  args <- vec_recycle_common(...)
  exec(paste0, !!!args)
}

stop_iv <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  abort(message, class = c(class, "iv_error"), ..., call = call)
}
