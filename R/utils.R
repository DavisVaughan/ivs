vec_locate_interval_merge_bounds <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             empty = "overlap",
                                             missing = "overlap") {
  name <- "vec_locate_interval_merge_bounds"
  fn <- import_vctrs(name)

  fn(
    start = start,
    end = end,
    ...,
    abutting = abutting,
    empty = empty,
    missing = missing
  )
}

vec_locate_interval_merge_groups <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             empty = "overlap",
                                             missing = "overlap") {
  name <- "vec_locate_interval_merge_groups"
  fn <- import_vctrs(name)

  fn(
    start = start,
    end = end,
    ...,
    abutting = abutting,
    empty = empty,
    missing = missing
  )
}

vec_interval_complement <- function(start,
                                    end,
                                    ...,
                                    lower = NULL,
                                    upper = NULL) {
  name <- "vec_interval_complement"
  fn <- import_vctrs(name)

  fn(
    start = start,
    end = end,
    ...,
    lower = lower,
    upper = upper
  )
}


import_vctrs <- function(name) {
  import_from(name, "vctrs")
}

import_from <- function(name, package) {
  ns <- getNamespace(package)

  if (!exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    abort(sprintf("No such '%s' function: `%s()`.", package, name))
  }

  get(name, mode = "function", envir = ns, inherits = FALSE)
}

vec_paste0 <- function(...) {
  # Use tidyverse recycling rules to avoid size zero recycling bugs
  args <- vec_recycle_common(...)
  exec(paste0, !!!args)
}

stop_iv <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  abort(message, class = c(class, "iv_error"), ..., call = call)
}
