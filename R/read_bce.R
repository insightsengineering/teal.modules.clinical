


#' Mask \code{rocheBCE::read_bce} on r.roche.com to add meta-data to the data
#' set for reproducibility
#' 
#' This version uses read_bce and adds the \code{'source'}, \code{'md5sum'},
#' \code{'accessed_by'} meta-data.
#'
#' @param FILE file path, needs to be an absolute file path
#' @param encoding encoding, see \code{\link{[rocheBCE]{encoding}}}
#'
#' 
#' @export
#' 
read_bce <- function(FILE, encoding = NULL) {
  
  requireNamespace("rocheBCE") || stop("read_bce requires the rocheBCE package ()")
  
  if (!is.character(FILE) || !(length(FILE) == 1)) stop("teal.oncology::read_bce only allows to specify one FILE at a time")
  
  if (substr(FILE, 1,1) != "/") stop("teal.oncology::read_bce only accepts absolute paths")
  
  if (!file.exists(FILE)) stop("file", FILE, "either does not exists or you do not have access")
  
  d <- rocheBCE::read_bce(FILE, encoding)
  
  attr(d, "md5sum") <- tools::md5sum(FILE)
  attr(d, "source") <- paste0("read_bce(FILE='", FILE,"'", ", encoding=", if (is.null(encoding)) "NULL" else encoding, ")")   #deparse(match.call(expand.dots = TRUE))
  attr(d, "access_by") <- Sys.info()['user']
  attr(d, "accessed_on") <- Sys.time()
  
  d
}


