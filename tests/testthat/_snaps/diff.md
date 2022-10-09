# checks that `x` is a vector

    Code
      iv_diff(x)
    Condition
      Error in `iv_diff()`:
      ! `x` must be a vector, not an environment.

# detects strictly increasing violations

    Code
      iv_diff(x)
    Condition
      Error in `iv_diff()`:
      ! `x` must be in strictly increasing order.
      i `x` is not in strictly increasing order at locations: `c(2, 4)`.

# can detect strictly increasing violations in the presence of missings

    Code
      iv_diff(x)
    Condition
      Error in `iv_diff()`:
      ! `x` must be in strictly increasing order.
      i `x` is not in strictly increasing order at locations: `c(3)`.

---

    Code
      iv_diff(x)
    Condition
      Error in `iv_diff()`:
      ! `x` must be in strictly increasing order.
      i `x` is not in strictly increasing order at locations: `c(5, 6)`.

---

    Code
      iv_diff(x)
    Condition
      Error in `iv_diff()`:
      ! `x` must be in strictly increasing order.
      i `x` is not in strictly increasing order at locations: `c(3)`.

