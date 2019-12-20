#' Parse relabel call to be used in modules
#'
#' @param datasets teal datasets object
#' @param dataname (\code{character}) name of data
#' @param vars (\code{character}) named vector where names are target names and values are actual names
#'
#' @return (\code{call}) object with relabel step
#'
#' @importFrom rtables var_relabel
get_relabel_call <- function(datasets, dataname, vars) {
  labels <- datasets$get_data_labels(dataname, vars)

  if (length(labels) == 0) {
    return(NULL)
  }

  return(
    as.call(append(
      quote(rtables::var_relabel),
      labels
    ))
  )
}