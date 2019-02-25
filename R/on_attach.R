.onAttach <- function(libname, pkgname) {
  return()
  packageStartupMessage(paste(
    "\nThis is a beta version for teal.modules.clinical v0.6.1. Install the current stable version (v0.6.0) with:",
    '  devtools::install_github("NEST/teal.modules.clinical", "v0.6.0", host = "https://github.roche.com/api/v3")',
    sep = "\n"
  ))
}
