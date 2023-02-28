SIGNAL_MISSING <- 0L
SIGNAL_NO_MATCH <- -1L

# TODO: Ideally we'd be able to use `vec_count_matches()`,
# but this doesn't exist yet. That should be more memory efficient because
# it doesn't have to materialize `"all"` of the matches.
iv_count_locations <- function(locations, missing, no_match) {
  # Sort by `location` to ensure we match input order
  res <- vec_count(locations$needles, sort = "location")
  out <- res$count

  if (is_scalar_integer(missing)) {
    detect_missing <- locations$haystack == SIGNAL_MISSING

    if (any(detect_missing)) {
      needles_missing <- locations$needles[detect_missing]
      out[res$key %in% needles_missing] <- missing
    }
  }

  if (is_scalar_integer(no_match)) {
    detect_no_match <- locations$haystack == SIGNAL_NO_MATCH

    if (any(detect_no_match)) {
      needles_no_match <- locations$needles[detect_no_match]
      out[res$key %in% needles_no_match] <- no_match
    }
  }

  out
}

check_count_missing <- function(missing, ..., call = caller_env()) {
  check_dots_empty0(...)

  if (is_string(missing)) {
    # `"drop"` doesn't make sense here
    missing <- arg_match0(
      arg = missing,
      values = c("equals", "error"),
      arg_nm = "missing",
      error_call = call
    )
  } else {
    missing <- vec_cast(missing, to = integer(), call = call)
    vec_check_size(missing, size = 1L, call = call)
  }

  missing
}

translate_count_missing <- function(missing) {
  if (is_scalar_integer(missing)) {
    SIGNAL_MISSING
  } else {
    missing
  }
}

check_count_no_match <- function(no_match, ..., call = caller_env()) {
  check_dots_empty0(...)

  if (is_string(no_match)) {
    # `"drop"` doesn't make sense here
    no_match <- arg_match0(
      arg = no_match,
      values = "error",
      arg_nm = "no_match",
      error_call = call
    )
  } else {
    no_match <- vec_cast(no_match, to = integer(), call = call)
    vec_check_size(no_match, size = 1L, call = call)
  }

  no_match
}

translate_count_no_match <- function(no_match) {
  if (is_scalar_integer(no_match)) {
    SIGNAL_NO_MATCH
  } else {
    no_match
  }
}
