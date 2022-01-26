# between takes the common type

    Code
      (expect_error(iv_locate_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# detect between takes the common type

    Code
      (expect_error(iv_detect_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `needles` <double> and `iv_start(haystack)` <character>.

# detect parallel between takes the common type

    Code
      (expect_error(iv_detect_parallel_between(1, iv("a", "b"))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `x` <double> and `iv_start(y)` <character>.

