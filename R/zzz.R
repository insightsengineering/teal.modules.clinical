.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger(namespace = "teal.modules.clinical")
  teal.logger::register_handlers("teal.modules.clinical")
  tern::set_default_na_str("<Missing>")
}
