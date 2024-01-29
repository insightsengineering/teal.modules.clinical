.onLoad <- function(libname, pkgname) { # nolint
  teal.logger::register_logger(namespace = "teal.modules.clinical")
  tern::set_default_na_str("<Missing>")
}
