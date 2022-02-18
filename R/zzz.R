# nocov start

.onLoad <- function(libname, pkgname) {
  ns <- ns_env(pkgname)

  env_bind(
    .env = ns,
    vec_interval_groups = import_vctrs("exp_vec_interval_groups"),
    vec_interval_locate_groups = import_vctrs("exp_vec_interval_locate_groups"),
    vec_interval_complement = import_vctrs("exp_vec_interval_complement")
  )
}

import_vctrs <- function(name) {
  import_from(name, "vctrs")
}
import_from <- function(name, package) {
  ns <- getNamespace(package)

  if (!exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    abort(sprintf("No such '%s' function: `%s()`.", package, name))
  }

  get(name, mode = "function", envir = ns, inherits = FALSE)
}

# nocov end
