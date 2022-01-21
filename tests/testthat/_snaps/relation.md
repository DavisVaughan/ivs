# iv_locate_overlaps - takes common type

    Code
      (expect_error(iv_locate_overlaps(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# errors on missing by default

    Code
      (expect_error(iv_locate_overlaps(x, y)))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_locate_overlaps()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_overlaps(x, y)))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_detect_overlaps()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_parallel_overlaps(x, y)))
    Output
      <error/iv_error_relation_missing_parallel>
      Error in `iv_detect_parallel_overlaps()`:
      ! Can't have missing values in `x` or `y`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_locate_precedes(x, y)))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_locate_positional()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_locate_follows(x, y)))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_locate_positional()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_precedes(x, y)))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_detect_positional()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_follows(x, y)))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_detect_positional()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_parallel_precedes(x, y)))
    Output
      <error/iv_error_relation_missing_parallel>
      Error in `iv_detect_parallel_positional()`:
      ! Can't have missing values in `x` or `y`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_parallel_follows(x, y)))
    Output
      <error/iv_error_relation_missing_parallel>
      Error in `iv_detect_parallel_positional()`:
      ! Can't have missing values in `x` or `y`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_locate_relation(x, y, type = "equals")))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_locate_relation()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_relation(x, y, type = "overlaps")))
    Output
      <error/iv_error_relation_missing>
      Error in `iv_detect_relation()`:
      ! Can't have missing values in `needles`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

---

    Code
      (expect_error(iv_detect_parallel_relation(x, y, type = "equals")))
    Output
      <error/iv_error_relation_missing_parallel>
      Error in `iv_detect_parallel_relation()`:
      ! Can't have missing values in `x` or `y`.
      i A value at location 1 is missing.
      i Use `missing` to control how missing values should be handled if they are expected.

# iv_locate_precedes - takes common type

    Code
      (expect_error(iv_locate_precedes(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# iv_locate_precedes - validates 'closest'

    Code
      (expect_error(iv_locate_precedes(iv(1, 2), iv(1, 2), closest = "x")))
    Output
      <error/rlang_error>
      Error in `iv_locate_positional()`:
      ! `closest` must be a single `TRUE` or `FALSE`.

# iv_locate_relation - takes common type

    Code
      (expect_error(iv_locate_relation(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# iv_detect_impl - takes common type

    Code
      (expect_error(iv_detect_overlaps(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# iv_detect_impl - validates 'missing'

    Code
      (expect_error(iv_detect_overlaps(iv(1, 2), iv(1, 2), missing = 1)))
    Output
      <error/rlang_error>
      Error in `check_detect_missing()`:
      ! `missing` must be "match", "error", or a single logical value.
    Code
      (expect_error(iv_detect_overlaps(iv(1, 2), iv(1, 2), missing = "x")))
    Output
      <error/rlang_error>
      Error in `check_detect_missing()`:
      ! `missing` must be "match", "error", or a single logical value.
    Code
      (expect_error(iv_detect_overlaps(iv(1, 2), iv(1, 2), missing = c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `check_detect_missing()`:
      ! `missing` must be "match", "error", or a single logical value.

# iv_detect_parallel_impl - recycles correctly

    Code
      (expect_error(iv_detect_parallel_overlaps(iv(1:2, 2:3), iv(1:3, 2:4))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `stop_vctrs()`:
      ! Can't recycle `x` (size 2) to match `y` (size 3).

# iv_detect_parallel_impl - takes common type

    Code
      (expect_error(iv_detect_parallel_overlaps(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `x` <double> and `y` <character>.

# iv_detect_parallel_impl - validates 'missing'

    Code
      (expect_error(iv_detect_parallel_overlaps(iv(1, 2), iv(1, 2), missing = 1)))
    Output
      <error/rlang_error>
      Error in `check_detect_parallel_missing()`:
      ! `missing` must be "match", "error", or a single logical value.
    Code
      (expect_error(iv_detect_parallel_overlaps(iv(1, 2), iv(1, 2), missing = "x")))
    Output
      <error/rlang_error>
      Error in `check_detect_parallel_missing()`:
      ! `missing` must be "match", "error", or a single logical value.
    Code
      (expect_error(iv_detect_parallel_overlaps(iv(1, 2), iv(1, 2), missing = c(TRUE,
        FALSE))))
    Output
      <error/rlang_error>
      Error in `check_detect_parallel_missing()`:
      ! `missing` must be "match", "error", or a single logical value.

