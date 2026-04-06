# Print @examples body from man/<topic>.Rd (no ### headers), for PR markdown.
# Usage: Rscript dev/extract_rd_examples.R tm_t_glm_counts

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript dev/extract_rd_examples.R <topic>", call. = FALSE)
}
topic <- args[[1]]

cmd_args <- commandArgs(trailingOnly = FALSE)
ff <- sub("^--file=", "", cmd_args[startsWith(cmd_args, "--file=")])
root <- if (length(ff) == 1L && nzchar(ff[[1]])) {
  normalizePath(file.path(dirname(ff[[1]]), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

rd <- file.path(root, "man", paste0(topic, ".Rd"))
if (!file.exists(rd)) {
  stop("Missing ", rd, call. = FALSE)
}

tf <- tempfile(fileext = ".R")
tools::Rd2ex(rd, out = tf, commentDontrun = TRUE, commentDonttest = FALSE)
lines <- readLines(tf, warn = FALSE)
lines <- lines[!grepl("^###", lines)]
unlink(tf)
cat(paste(lines, collapse = "\n"), "\n", sep = "")
