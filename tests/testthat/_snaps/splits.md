# casts `on` to type of `x` bounds

    Code
      (expect_error(iv_splits(x, on = "x")))
    Output
      <error/vctrs_error_cast>
      Error in `iv_splits()`:
      ! Can't convert `on` <character> to match type of `iv_start(x)` <double>.

