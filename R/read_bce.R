#' Mask \code{read_bce} on r.roche.com to add meta-data to the
#' data set for reproducibility
#'
#' This version uses \code{read_bce} and adds the \code{'source'}, \code{'md5sum'},
#' \code{'accessed_by'} meta-data.
#'
#' @param file file path, needs to be an absolute file path
#' @param encoding encoding
#'
#' @export
#'
read_bce <- function(file, encoding = NULL) {
  if (!is.character(file) || !(length(file) == 1)) {
    stop("teal.modules.clinical::read_bce only allows to specify one file at a time")
  }

  if (substr(file, 1, 1) != "/") {
    stop("teal.modules.clinical::read_bce only accepts absolute paths")
  }

  if (!file.exists(file)) {
    stop("file", file, "either does not exists or you do not have access")
  }

  d <- haven::read_sas(file, encoding)

  attr(d, "md5sum") <- tools::md5sum(file)
  attr(d, "source") <- paste0(
    "read_bce(file='",
    file,
    "'",
    ", encoding=",
    if (is.null(encoding)) "NULL" else encoding,
    ")"
  )
  attr(d, "access_by") <- Sys.info()["user"]
  attr(d, "accessed_on") <- Sys.time()

  return(d)
}
