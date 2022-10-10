# ivs 0.1.0.9000

* In `iv()`, incomplete value propagation is now done before the `start < end`
  check, which fixes an inconsistent edge case (#36).

* New `iv_diff()` for generating an iv from an existing vector that is in
  strictly increasing order (#17).

* You can now combine an iv containing unspecified components with any other iv
  (#33).

* `vec_ptype()` and `vec_ptype_finalise()` methods have been added for the iv
  class. This should result in slightly better performance when combining many
  ivs together (#27).

* The `"iv"` class name has been renamed to the more specific `"ivs_iv"` to
  better insulate it from potential collisions with classes from other packages
  (#25).

* Improved on the call reported by errors thrown in ivs (#23).

* New family of functions for identifying interval _containers_, which are
  intervals that aren't contained within any other interval. These functions
  are: `iv_containers()`, `iv_identify_containers()`, `iv_identify_container()`,
  and `iv_locate_containers()` (#20).

* New Examples vignette that links out to Stack Overflow questions that were
  solved with ivs. View it locally with `vignette("examples", package = "ivs")`.

* Added a `NEWS.md` file to track changes to the package.
