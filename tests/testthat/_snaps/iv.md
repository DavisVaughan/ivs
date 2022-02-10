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
      Error:
      ! Can't combine `start` <character> and `end` <double>.

# inputs must be size compatible

    Code
      iv(1:2, 1:3)
    Condition
      Error:
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
      Error:
      ! Can't combine `..1` <character> and `..2` <double>.

# ptype2 errors as needed

    Code
      vec_ptype2(iv("x", "y"), iv(1L, 2L))
    Condition
      Error:
      ! Can't combine <character> and <integer>.

# cast errors as needed

    Code
      vec_cast(iv("x", "y"), iv(1L, 2L))
    Condition
      Error:
      ! Can't convert <character> to <integer>.

# abbreviation is passed through to inner type

    Code
      vec_ptype_abbr(iv(1, 2))
    Output
      [1] "iv<dbl>"

---

    Code
      vec_ptype_abbr(iv(data_frame(x = 1), data_frame(x = 2)))
    Output
      [1] "iv<df[,1]>"

# full ptype is passed through to inner type

    Code
      vec_ptype_full(iv(1, 2))
    Output
      [1] "iv<double>"

---

    Code
      vec_ptype_full(iv(data_frame(x = 1, y = 2), data_frame(x = 2, y = 3)))
    Output
      [1] "iv<data.frame<\n  x: double\n  y: double\n>>"

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

