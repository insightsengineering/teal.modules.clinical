options(shiny.port = 3333)

check <- function(error_modules) {
  error_message <- paste("Error while testing these modules:", paste(error_modules, collapse = ", "))
  success_message <- "All the module examples ran successfully."
  if (length(error_modules) > 0) {
    stop(error_message)
  } else {
    print(success_message)
  }
}

test_module <- function(module, pkg_name) {
  print(paste0("Running example: ", pkg_name, "::", module))
  command <- "npm"
  args <- c("run", "test-e2e", paste0("--app_name=", module), paste0("--pkg_name=", pkg_name))
  system2("npm", args, wait = TRUE)
}

run_e2e_test <- function(pkg_name) {
  pkg_exports <- getNamespaceExports(pkg_name)
  pkg_modules <- pkg_exports[grep("^tm_", pkg_exports)]

  error_modules <- character(0)
  for (module in pkg_modules) {
    has_error <- test_module(module, pkg_name)
    if (has_error) {
      error_modules <- c(error_modules, module)
    }
  }

  return(error_modules)
}

error_modules <- run_e2e_test("teal.modules.clinical")
check(error_modules)
