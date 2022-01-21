# between takes the common type

    Code
      (expect_error(iv_locate_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# missing values error by default

    Code
      (expect_error(iv_locate_between(NA_character_, iv("a", "b"))))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_locate_between()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

# detect between takes the common type

    Code
      (expect_error(iv_detect_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# detect between has missing values error by default

    Code
      (expect_error(iv_detect_between(NA_character_, iv("a", "b"))))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_detect_between()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_parallel_between(NA_character_, iv("a", "b"))))
    Output
      <error/iv_error_relation_missing_parallel>
      Error in `iv_detect_parallel_between()`:
      ! Can't have missing values in `x` or `y`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.
    Code
      (expect_error(iv_detect_parallel_between(1, iv(NA, NA))))
    Output
      <error/iv_error_relation_missing_parallel>
      Error in `iv_detect_parallel_between()`:
      ! Can't have missing values in `x` or `y`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

# detect parallel between takes the common type

    Code
      (expect_error(iv_detect_parallel_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `x` <double> and `iv_start(y)` <character>.

