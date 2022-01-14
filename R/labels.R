#' Get full label, useful for annotating plots
#'
#' @return "Label `[Column name]`" if label exists, otherwise "Column name"
#'
#' @param dataset \code{data.frame} dataset
#' @param column \code{character} column to get label from
#' @param omit_raw_name \code{logical} omits the raw name in square brackets if label is found
#'
#' @examples
#' data <- mtcars
#' teal.modules.clinical:::column_annotation_label(data, "cyl")
#' attr(data[["cyl"]], "label") <- "Cylinder"
#' teal.modules.clinical:::column_annotation_label(data, "cyl")
#' teal.modules.clinical:::column_annotation_label(data, "cyl", omit_raw_name = TRUE)
#' \dontrun{
#' teal.modules.clinical:::column_annotation_label(ANL, "ACTARM")
#' }
column_annotation_label <- function(dataset, column, omit_raw_name = FALSE) {
  checkmate::assert_data_frame(dataset)
  checkmate::assert_string(column)
  checkmate::assert_flag(omit_raw_name)

  if (is.null(attr(dataset[[column]], "label"))) {
    column
  } else {
    col_label <- attr(dataset[[column]], "label")
    if (omit_raw_name) {
      col_label
    } else {
      sprintf("%s [%s]", col_label, column)
    }
  }
}
