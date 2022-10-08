# ivs 0.1.0.9000

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
