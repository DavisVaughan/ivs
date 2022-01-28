# `locations` is validated

    Code
      (expect_error(iv_align(1, 2, locations = 1)))
    Output
      <error/rlang_error>
      Error in `iv_align()`:
      ! `locations` must be a data frame.
    Code
      (expect_error(iv_align(1, 2, locations = data_frame())))
    Output
      <error/rlang_error>
      Error in `iv_align()`:
      ! `locations` must be a two column data frame.
    Code
      (expect_error(iv_align(1, 2, locations = data_frame(x = 1, haystack = 2))))
    Output
      <error/rlang_error>
      Error in `iv_align()`:
      ! `locations` must have a column named "needles".
    Code
      (expect_error(iv_align(1, 2, locations = data_frame(needles = 1, x = 2))))
    Output
      <error/rlang_error>
      Error in `iv_align()`:
      ! `locations` must have a column named "haystack".
    Code
      (expect_error(iv_align(1, 2, locations = data_frame(needles = 1, haystack = 2L)))
      )
    Output
      <error/rlang_error>
      Error in `iv_align()`:
      ! `locations$needles` must be an integer vector.
    Code
      (expect_error(iv_align(1, 2, locations = data_frame(needles = 1L, haystack = 2)))
      )
    Output
      <error/rlang_error>
      Error in `iv_align()`:
      ! `locations$haystack` must be an integer vector.

