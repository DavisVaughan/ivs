SIGNAL_MISSING <- 0L
SIGNAL_NO_MATCH <- -1L

iv_count_locations <- function(locations, missing, no_match) {
  # No need to worry about `NA` values in `needles` because `remaining` isn't
  # exposed in the count functions (it doesn't make sense to do so)
  out <- vec_run_sizes(locations$needles)

  value_missing <- is_scalar_integer(missing)
  value_no_match <- is_scalar_integer(no_match)

  if (!value_missing && !value_no_match) {
    # Some combination of these that doesn't require special post-processing:
    # `missing = "equals" / "error"`
    # `no_match = "error"`
    return(out)
  }

  ones <- out == 1L
  any_ones <- any(ones)

  if (!any_ones) {
    # No missing or unmatched `needles` are possible
    return(out)
  }

  ones <- which(ones)
  starts <- vec_run_sizes_to_starts(out)
  starts <- vec_slice(starts, ones)
  haystack <- vec_slice(locations$haystack, starts)

  if (value_missing) {
    where_missing <- haystack == SIGNAL_MISSING

    if (any(where_missing)) {
      out_missing <- vec_slice(ones, where_missing)
      out <- vec_assign(out, out_missing, missing)
    }
  }

  if (value_no_match) {
    where_no_match <- haystack == SIGNAL_NO_MATCH

    if (any(where_no_match)) {
      out_no_match <- vec_slice(ones, where_no_match)
      out <- vec_assign(out, out_no_match, no_match)
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
