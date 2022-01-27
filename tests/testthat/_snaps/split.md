# casts `on` to type of `x` bounds

    Code
      (expect_error(iv_split(x, on = "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't convert `on` <character> to match type of `iv_start(x)` <double>.

