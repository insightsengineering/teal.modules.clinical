

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "\nThis is a beta version for teal.tern v0.5.1. Install the current stable version (v0.5.0) with:",
    '  devtools::install_github("Rpackages/teal.tern", "v0.5.0", host = "https://github.roche.com/api/v3")',
    sep = "\n"
  ))
}
