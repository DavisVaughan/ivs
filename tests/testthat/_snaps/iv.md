# `start` must be less than `end`

    Code
      iv(2, 2)
    Condition
      Error in `iv()`:
      ! `start` must be less than `end`.

---

    Code
      iv(3, 2)
    Condition
      Error in `iv()`:
      ! `start` must be less than `end`.

# inputs must be type compatible

    Code
      iv("x", 1)
    Condition
      Error in `stop_vctrs()`:
      ! Can't combine `start` <character> and `end` <double>.

# inputs must be size compatible

    Code
      iv(1:2, 1:3)
    Condition
      Error in `stop_vctrs()`:
      ! Can't recycle `start` (size 2) to match `end` (size 3).

# inputs must be vectors

    Code
      iv(NULL, 2)
    Condition
      Error in `iv()`:
      ! `start` must be a vector.

---

    Code
      iv(2, NULL)
    Condition
      Error in `iv()`:
      ! `end` must be a vector.

# inputs must be in pairs

    Code
      iv_pairs(c(1, 2), 3)
    Condition
      Error in `iv_pairs()`:
      ! All inputs must be in pairs of size 2.
      i Input 2 is size 1.

# must have at least one input

    Code
      iv_pairs()
    Condition
      Error in `iv_pairs()`:
      ! Must supply at least one input.

# pairs must be type compatible

    Code
      iv_pairs(c("a", "b"), c(1, 2))
    Condition
      Error in `stop_vctrs()`:
      ! Can't combine `..1` <character> and `..2` <double>.

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

