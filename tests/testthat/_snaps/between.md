# between takes the common type

    Code
      (expect_error(iv_locate_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `iv_locate_between()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# between can error on missing needles

    Code
      (expect_error(iv_locate_between(NA, iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `stop_matches()`:
      ! No element can contain missing values.
      x The element at location 1 contains missing values.

# detect between takes the common type

    Code
      (expect_error(iv_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `iv_between()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# detect between can error on missing needles

    Code
      (expect_error(iv_between(NA, iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `stop_matches()`:
      ! No element can contain missing values.
      x The element at location 1 contains missing values.

# detect pairwise between takes the common type

    Code
      (expect_error(iv_pairwise_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `iv_pairwise_between()`:
      ! Can't combine `x` <double> and `iv_start(y)` <character>.

