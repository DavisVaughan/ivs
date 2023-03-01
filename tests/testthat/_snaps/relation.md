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

# iv_locate_overlaps - can error on invalid relationships

    Code
      (expect_error(iv_locate_overlaps(x, y, relationship = "many-to-one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `iv_locate_overlaps()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# iv_count_overlaps - can error on missing needles

    Code
      (expect_error(iv_count_overlaps(iv(NA, NA), iv(1, 2), missing = "error")))
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_overlaps()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# validates overlaps `type`

    Code
      iv_overlaps(iv(1, 2), iv(1, 2), type = "foo")
    Condition
      Error in `iv_overlaps()`:
      ! `type` must be one of "any", "equals", "contains", "within", "starts", or "ends", not "foo".

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

# iv_locate_precedes - can error on invalid relationships

    Code
      (expect_error(iv_locate_precedes(x, y, relationship = "many-to-one")))
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `iv_locate_precedes()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

---

    Code
      (expect_error(iv_locate_precedes(x, y, relationship = "many-to-one", closest = TRUE))
      )
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `iv_locate_precedes()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

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

# iv_locate_relates - can error on invalid relationships

    Code
      (expect_error(iv_locate_relates(x, y, type = "overlaps", relationship = "many-to-one"))
      )
    Output
      <error/vctrs_error_matches_relationship_many_to_one>
      Error in `iv_locate_relates()`:
      ! Each value of `needles` can match at most 1 value from `haystack`.
      x Location 1 of `needles` matches multiple values.

# iv_count_relates - can error on missing needles

    Code
      (expect_error(iv_count_relates(iv(NA, NA), iv(1, 2), type = "equals", missing = "error"))
      )
    Output
      <error/vctrs_error_matches_incomplete>
      Error in `iv_locate_relates()`:
      ! `needles` can't contain missing values.
      x Location 1 contains missing values.

# validates relation `type`

    Code
      iv_relates(iv(1, 2), iv(1, 2), type = "foo")
    Condition
      Error in `iv_relates()`:
      ! `type` must be one of "precedes", "preceded-by", "meets", "met-by", "overlaps", "overlapped-by", "starts", "started-by", "finishes", "finished-by", "during", "contains", or "equals", not "foo".

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

