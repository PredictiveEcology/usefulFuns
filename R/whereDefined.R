#' Find the environment in which a name is defined
#'
#' Walks up the environment chain from `env` and returns the first environment
#' in which `name` is bound, erroring if it is not found anywhere. This is a
#' drop-in replacement for `pryr::where()`, which was removed as a dependency
#' when `pryr` was archived on CRAN (2026) -- an archived package cannot be
#' resolved by `pak`, so it broke dependency installation entirely.
#'
#' The error-on-not-found behaviour is deliberate and load-bearing: callers
#' use it inside `tryCatch()` to distinguish "already defined somewhere up the
#' chain" from "not defined at all".
#'
#' @param name Character. The name to look for.
#' @param env  Environment to start searching from. Defaults to the caller.
#'
#' @return The environment in which `name` is bound.
#' @keywords internal
#' @rdname whereDefined
whereDefined <- function(name, env = parent.frame()) {
  stopifnot(is.character(name), length(name) == 1L)
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) {
      return(env)
    }
    env <- parent.env(env)
  }
  stop("Can't find `", name, "`", call. = FALSE)
}
