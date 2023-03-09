vec_paste0 <- function(...) {
  # Use tidyverse recycling rules to avoid size zero recycling bugs
  args <- vec_recycle_common(...)
  exec(paste0, !!!args)
}

vec_run_sizes_to_starts <- function(sizes) {
  n_sizes <- length(sizes)

  if (n_sizes > 0L) {
    cumsum(c(1L, sizes[-n_sizes]))
  } else {
    integer()
  }
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

obj_s3_method_exists <- function(x, generic) {
  !is_null(obj_s3_method_lookup(x, generic))
}

obj_s3_method_lookup <- function(x, generic) {
  if (!is.object(x)) {
    return(NULL)
  }

  classes <- class(x)

  if (!is_character(classes)) {
    abort("`class(x)` didn't return a character vector.", .internal = TRUE)
  }

  for (class in classes) {
    method <- paste0(generic, ".", class)
    method <- s3_method_get(method)

    if (!is_null(method)) {
      return(method)
    }
  }

  NULL
}

s3_method_get <- function(name) {
  # Try global env first in case the user registered a method interactively
  env <- global_env()
  fn <- env_get(env, name, default = NULL)

  if (is_function(fn)) {
    return(fn)
  }

  # Then try the package S3 methods table
  env <- the$env_s3_methods_table
  fn <- env_get(env, name, default = NULL)

  if (is_function(fn)) {
    return(fn)
  }

  # Symbol not bound to the `env`, or it was bound to a non-function
  NULL
}
