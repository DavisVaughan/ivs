# nocov start

.onLoad <- function(libname, pkgname) {
  env_ns <- ns_env(pkgname)

  the$env_ns <- env_ns
  the$env_s3_methods_table <- env_ns[[".__S3MethodsTable__."]]

  env_bind(
    .env = env_ns,
    vec_interval_groups = import_vctrs("exp_vec_interval_groups"),
    vec_interval_locate_groups = import_vctrs("exp_vec_interval_locate_groups"),
    vec_interval_complement = import_vctrs("exp_vec_interval_complement"),
    vec_interval_locate_containers = import_vctrs("exp_vec_interval_locate_containers")
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
