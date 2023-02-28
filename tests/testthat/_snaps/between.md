# between takes the common type

    Code
      (expect_error(iv_locate_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_locate_between()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# between can error on missing needles

    Code
      (expect_error(iv_locate_between(NA, iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_between()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

---

    Code
      (expect_error(iv_count_between(NA, iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_count_between()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# includes takes the common type

    Code
      (expect_error(iv_locate_includes(iv("a", "b"), 1)))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_locate_includes()`:
      ! Can't combine `haystack` <double> and `iv_start(needles)` <character>.

# includes can error on missing needles

    Code
      (expect_error(iv_locate_includes(iv(NA, NA), 1, missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_includes()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

---

    Code
      (expect_error(iv_count_includes(iv(NA, NA), 2, missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_count_includes()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# between can error on unmatched needles

    Code
      (expect_error(iv_count_between(3, iv(1, 2), no_match = "error")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `iv_count_between()`:
      ! Each value of `needles` must have a match in `haystack`.
      x Location 1 of `needles` does not have a match.

# includes can error on unmatched needles

    Code
      (expect_error(iv_count_includes(iv(1, 2), 3, no_match = "error")))
    Output
      <error/vctrs_error_matches_nothing>
      Error in `iv_count_includes()`:
      ! Each value of `needles` must have a match in `haystack`.
      x Location 1 of `needles` does not have a match.

# detect between takes the common type

    Code
      (expect_error(iv_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_between()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# detect between can error on missing needles

    Code
      (expect_error(iv_between(NA, iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_between()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# detect pairwise between takes the common type

    Code
      (expect_error(iv_pairwise_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_pairwise_between()`:
      ! Can't combine `x` <double> and `iv_start(y)` <character>.

