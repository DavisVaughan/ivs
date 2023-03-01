# pairwise complement of interval with itself is not allowed

    Code
      (expect_error(iv_pairwise_set_complement(x, x)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

# pairwise complement of abutting intervals is not allowed

    Code
      (expect_error(iv_pairwise_set_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

---

    Code
      (expect_error(iv_pairwise_set_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

# pairwise complement of overlapping intervals is not allowed

    Code
      (expect_error(iv_pairwise_set_complement(x, x)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

---

    Code
      (expect_error(iv_pairwise_set_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.
    Code
      (expect_error(iv_pairwise_set_complement(y, x)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

---

    Code
      (expect_error(iv_pairwise_set_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.
    Code
      (expect_error(iv_pairwise_set_complement(y, x)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

# errors on gaps

    Code
      (expect_error(iv_pairwise_set_union(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap.
      i Use `iv_pairwise_span()` to combine across gaps.

---

    Code
      (expect_error(iv_pairwise_set_union(y, x)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap.
      i Use `iv_pairwise_span()` to combine across gaps.

# pairwise intersection between non-overlapping intervals errors

    Code
      (expect_error(iv_pairwise_set_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

---

    Code
      (expect_error(iv_pairwise_set_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

---

    Code
      (expect_error(iv_pairwise_set_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

---

    Code
      (expect_error(iv_pairwise_set_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

# pairwise difference between interval and itself is not allowed

    Code
      (expect_error(iv_pairwise_set_difference(x, x)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_difference()`:
      ! Can't compute a difference when `y` completely contains `x`.
      i This would result in an empty interval.
      i Location 1 contains this issue.

# throws error when `y` is contained within `x`

    Code
      (expect_error(iv_pairwise_set_difference(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_difference()`:
      ! Can't compute a difference when `y` is completely contained within `x`.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.

# throws error when `y` contains `x`

    Code
      (expect_error(iv_pairwise_set_difference(x, y)))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_difference()`:
      ! Can't compute a difference when `y` completely contains `x`.
      i This would result in an empty interval.
      i Location 1 contains this issue.

# throws error when neither endpoint matches

    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 2), iv(3, 4))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` don't share an endpoint.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.
    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(3, 4), iv(1, 2))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` don't share an endpoint.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.
    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 3), iv(2, 4))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` don't share an endpoint.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.
    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(2, 4), iv(1, 3))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` don't share an endpoint.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.
    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 4), iv(2, 3))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` don't share an endpoint.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.
    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(2, 3), iv(1, 4))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` don't share an endpoint.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.

# throws error when both endpoints match

    Code
      (expect_error(iv_pairwise_set_symmetric_difference(iv(1, 2), iv(1, 2))))
    Output
      <error/rlang_error>
      Error in `iv_pairwise_set_symmetric_difference()`:
      ! Can't compute a symmetric difference when `x` and `y` are equal.
      i This would result in an empty interval.
      i Location 1 contains this issue.

