# iv_locate_overlaps - takes common type

    Code
      (expect_error(iv_locate_overlaps(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_locate_overlaps()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# can error on missing needles

    Code
      (expect_error(iv_locate_overlaps(iv(NA, NA), iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_overlaps()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# iv_count_overlaps - can error on missing needles

    Code
      (expect_error(iv_count_overlaps(iv(NA, NA), iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_overlaps()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# iv_locate_precedes - takes common type

    Code
      (expect_error(iv_locate_precedes(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_locate_precedes()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# iv_locate_precedes - validates 'closest'

    Code
      (expect_error(iv_locate_precedes(iv(1, 2), iv(1, 2), closest = "x")))
    Output
      <error/rlang_error>
      Error in `iv_locate_precedes()`:
      ! `closest` must be a single `TRUE` or `FALSE`.

# iv_locate_precedes - can error on missing needles

    Code
      (expect_error(iv_locate_precedes(iv(NA, NA), iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_precedes()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# iv_locate_relates - takes common type

    Code
      (expect_error(iv_locate_relates(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_locate_relates()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# iv_locate_relates - can error on missing needles

    Code
      (expect_error(iv_locate_relates(iv(NA, NA), iv(1, 2), type = "equals", missing = "error"))
      )
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_relates()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# iv_count_relates - can error on missing needles

    Code
      (expect_error(iv_count_relates(iv(NA, NA), iv(1, 2), type = "equals", missing = "error"))
      )
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_relates()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# iv_detect_impl - takes common type

    Code
      (expect_error(iv_overlaps(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_overlaps()`:
      ! Can't combine `needles` <double> and `haystack` <character>.

# iv_detect_impl - validates 'missing'

    Code
      (expect_error(iv_overlaps(iv(1, 2), iv(1, 2), missing = 1)))
    Output
      <error/rlang_error>
      Error in `iv_overlaps()`:
      ! `missing` must be "equals", "error", or a single logical value.
    Code
      (expect_error(iv_overlaps(iv(1, 2), iv(1, 2), missing = "x")))
    Output
      <error/rlang_error>
      Error in `iv_overlaps()`:
      ! `missing` must be "equals", "error", or a single logical value.
    Code
      (expect_error(iv_overlaps(iv(1, 2), iv(1, 2), missing = c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `iv_overlaps()`:
      ! `missing` must be "equals", "error", or a single logical value.

# detect can error on missing needles

    Code
      (expect_error(iv_overlaps(iv(NA, NA), iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_overlaps()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# iv_detect_pairwise_impl - recycles correctly

    Code
      (expect_error(iv_pairwise_overlaps(iv(1:2, 2:3), iv(1:3, 2:4))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `iv_pairwise_overlaps()`:
      ! Can't recycle `x` (size 2) to match `y` (size 3).

# iv_detect_pairwise_impl - takes common type

    Code
      (expect_error(iv_pairwise_overlaps(iv(1, 2), iv("a", "b"))))
    Output
      <error/vctrs_error_ptype2>
      Error in `iv_pairwise_overlaps()`:
      ! Can't combine `x` <double> and `y` <character>.

