#' Get full label, useful for annotating plots
#'
#' @param dataset (`data.frame`)\cr dataset
#' @param column (`character`)\cr column to get label from
#' @param omit_raw_name (`logical`)\cr omits the raw name in square brackets if label is found
#'
#' @return "Label `[Column name]`" if label exists, otherwise "Column name".
#'
#' @examples
#' data <- mtcars
#' column_annotation_label(data, "cyl")
#' attr(data[["cyl"]], "label") <- "Cylinder"
#' column_annotation_label(data, "cyl")
#' column_annotation_label(data, "cyl", omit_raw_name = TRUE)
#' column_annotation_label(tmc_ex_adsl, "ACTARM")
#' @export
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
