# Run a module's @examples teal app for manual browser / MCP preview.
# Usage: Rscript dev/run_example_in_browser.R tm_t_glm_counts [port]
# Blocks until interrupted; binds to 127.0.0.1 only.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript dev/run_example_in_browser.R <topic> [port]", call. = FALSE)
}
topic <- args[[1]]
port <- if (length(args) >= 2L) as.integer(args[[2]]) else 8787L

cmd_args <- commandArgs(trailingOnly = FALSE)
ff <- sub("^--file=", "", cmd_args[startsWith(cmd_args, "--file=")])
if (length(ff) == 1L && nzchar(ff[[1]])) {
  root <- normalizePath(file.path(dirname(ff[[1]]), ".."), winslash = "/", mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(root)

teal_root <- normalizePath(file.path(root, "..", "teal"), winslash = "/", mustWork = FALSE)
if (dir.exists(teal_root)) {
  pkgload::load_all(teal_root, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
}
pkgload::load_all(root, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

rd <- file.path(root, "man", paste0(topic, ".Rd"))
if (!file.exists(rd)) {
  stop("No Rd for ", topic, call. = FALSE)
}

tf <- tempfile(fileext = ".R")
tools::Rd2ex(rd, out = tf, commentDontrun = TRUE, commentDonttest = FALSE)
lines <- readLines(tf, warn = FALSE)
lines <- lines[!grepl("^###", lines)]
ex <- paste(lines, collapse = "\n")
unlink(tf)

# Drop Rd autoprint guard; always launch on a fixed local port for browser / MCP.
# Some Rd files contain multiple `if (interactive()) { shinyApp(...) }` blocks.
pat <- "(?s)if\\s*\\(\\s*interactive\\s*\\(\\s*\\)\\s*\\)\\s*\\{\\s*shinyApp\\s*\\(\\s*app\\$ui\\s*,\\s*app\\$server\\s*\\)\\s*\\}"
while (grepl(pat, ex, perl = TRUE)) {
  ex <- gsub(pat, "", ex, perl = TRUE)
}
ex <- paste0(
  ex,
  "\nshiny::runApp(shiny::shinyApp(app$ui, app$server), host = \"127.0.0.1\", port = ",
  port,
  "L, launch.browser = FALSE)\n"
)

message("Listening on http://127.0.0.1:", port, " (Ctrl+C to stop)")
eval(parse(text = ex), envir = globalenv())
