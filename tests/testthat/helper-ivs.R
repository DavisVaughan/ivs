testthat_import_from <- function(ns, names, env = caller_env()) {
  skip_if_not_installed(ns)
  import_from(ns, names, env = env)
}
import_from <- function(ns, names, env = caller_env()) {
  objs <- env_get_list(ns_env(ns), names)
  env_bind(env, !!!objs)
}
