# drop is not allowed

    Code
      check_count_missing("drop")
    Condition
      Error:
      ! `missing` must be one of "equals" or "error", not "drop".

---

    Code
      check_count_no_match("drop")
    Condition
      Error:
      ! `no_match` must be one of "error", not "drop".

# `missing` is checked

    Code
      check_count_missing(1.5)
    Condition
      Error:
      ! Can't convert from `missing` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      check_count_missing(c(1, 2))
    Condition
      Error:
      ! `missing` must have size 1, not size 2.

# `no_match` is checked

    Code
      check_count_no_match(1.5)
    Condition
      Error:
      ! Can't convert from `no_match` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      check_count_no_match(c(1, 2))
    Condition
      Error:
      ! `no_match` must have size 1, not size 2.

