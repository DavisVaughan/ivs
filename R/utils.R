vec_interval_locate_minimal <- function(start,
                                        end,
                                        ...,
                                        merge_abutting = TRUE,
                                        keep_empty = FALSE,
                                        keep_missing = FALSE) {
  name <- "vec_interval_locate_minimal"
  fn <- import_vctrs(name)

  fn(
    start = start,
    end = end,
    ...,
    merge_abutting = merge_abutting,
    keep_empty = keep_empty,
    keep_missing = keep_missing
  )
}

vec_interval_locate_minimal_groups <- function(start,
                                               end,
                                               ...,
                                               merge_abutting = TRUE,
                                               keep_empty = FALSE,
                                               keep_missing = FALSE) {
  name <- "vec_interval_locate_minimal_groups"
  fn <- import_vctrs(name)

  fn(
    start = start,
    end = end,
    ...,
    merge_abutting = merge_abutting,
    keep_empty = keep_empty,
    keep_missing = keep_missing
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
