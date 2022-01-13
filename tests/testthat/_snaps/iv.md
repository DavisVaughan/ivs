# default proxy error works

    Code
      iv_proxy(1)
    Condition
      Error in `iv_proxy()`:
      ! Object `x`, with type <numeric>, is not an <iv> and does not implement an `iv_proxy()` method.

# default restore error works

    Code
      iv_restore(1, 2)
    Condition
      Error in `iv_restore()`:
      ! Object `to`, with type <numeric>, is not an <iv> and does not implement an `iv_restore()` method.

