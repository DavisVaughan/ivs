# ivs 0.2.0

* The family of "set" functions has been renamed to include a `set_` prefix,
  for example, `iv_union()` is now `iv_set_union()`. This has been done to
  align with the new family of vctrs set functions, like `vec_set_union()`, and
  to reduce the ambiguity between the new `iv_diff()` helper and
  `iv_difference()` (now `iv_set_difference()`). The old names are deprecated,
  and will be removed in a future release (#35).
  
  * `iv_complement()` -> `iv_set_complement()`
  * `iv_union()` -> `iv_set_union()`
  * `iv_intersect()` -> `iv_set_intersect()`
  * `iv_difference()` -> `iv_set_difference()`
  * `iv_symmetric_difference()` -> `iv_set_symmetric_difference()`
  * The same changes have been made for the `iv_pairwise_*()` functions that
    share the same suffixes.

* New family of functions that perform the inverse of `iv_between()`, i.e.
  rather than detecting if `needles[i]`, a vector value, falls _between_ any
  intervals in `haystack`, an iv, these detect if `needles[i]`, an interval,
  _includes_ any value from `haystack`, a vector. These functions are:
  `iv_includes()`, `iv_locate_includes()`, `iv_count_includes()`, and
  `iv_pairwise_includes()` (#41).
  
* New family of functions for identifying interval _containers_, which are
  intervals that aren't contained within any other interval. These functions
  are: `iv_containers()`, `iv_identify_containers()`, `iv_identify_container()`,
  and `iv_locate_containers()` (#20).

* New `iv_diff()` for generating an iv from an existing vector that is in
  strictly increasing order (#17).

* New `iv_span()` for computing a summary interval that encompasses the entire
  range of an existing iv (#49).

* New Examples vignette that links out to Stack Overflow questions that were
  solved with ivs. View it locally with `vignette("examples", package = "ivs")`.

* `vec_ptype()` and `vec_ptype_finalise()` methods have been added for the iv
  class. This should result in slightly better performance when combining many
  ivs together (#27).

* `iv_locate_overlaps()`, `iv_locate_precedes()`, `iv_locate_follows()`,
  `iv_locate_between()`, and `iv_locate_includes()` have all gained the
  `relationship` argument from the underlying engine,
  `vctrs::vec_locate_matches()` (#45).

* `iv_proxy()` now returns the input unchanged if it doesn't implement an S3
  method, rather than erroring. In combination with `is_iv()`, this provides a
  way to check if an input implements a proxy method and to implement different
  behaviors depending on the result.

* In `iv()`, incomplete value propagation is now done before the `start < end`
  check, which fixes an inconsistent edge case (#36).

* You can now combine an iv containing unspecified components with any other iv
  (#33).

* The `"iv"` class name has been renamed to the more specific `"ivs_iv"` to
  better insulate it from potential collisions with classes from other packages
  (#25).

* Improved on the call reported by errors thrown in ivs (#23).

* Switched to using `vec_run_sizes()` and `vec_chop(sizes =)` internally, which
  improves performance in some cases (#50).

* Added a `NEWS.md` file to track changes to the package.
