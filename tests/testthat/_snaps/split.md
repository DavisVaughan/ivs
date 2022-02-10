# casts `on` to type of `x` bounds

    Code
      (expect_error(iv_split(x, on = "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `iv_split()`:
      ! Can't convert `on` <character> to match type of `iv_start(x)` <double>.

