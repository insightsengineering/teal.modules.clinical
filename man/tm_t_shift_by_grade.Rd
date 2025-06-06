% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_shift_by_grade.R
\name{tm_t_shift_by_grade}
\alias{tm_t_shift_by_grade}
\title{teal Module: Grade Summary Table}
\usage{
tm_t_shift_by_grade(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  visit_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "AVISIT"), selected = "AVISIT", fixed = TRUE),
  paramcd,
  worst_flag_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")), selected = "WGRLOVFL"),
  worst_flag_indicator =
    teal.transform::choices_selected(teal.transform::value_choices(dataname, "WGRLOVFL"),
    selected = "Y", fixed = TRUE),
  anl_toxgrade_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    c("ATOXGR")), selected = c("ATOXGR"), fixed = TRUE),
  base_toxgrade_var =
    teal.transform::choices_selected(teal.transform::variable_choices(dataname, subset =
    c("BTOXGR")), selected = c("BTOXGR"), fixed = TRUE),
  id_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "USUBJID"), selected = "USUBJID", fixed = TRUE),
  add_total = FALSE,
  total_label = default_total_label(),
  drop_arm_levels = TRUE,
  pre_output = NULL,
  post_output = NULL,
  na_level = default_na_str(),
  code_missing_baseline = FALSE,
  basic_table_args = teal.widgets::basic_table_args(),
  transformators = list(),
  decorators = list()
)
}
\arguments{
\item{label}{(\code{character})\cr menu item label of the module in the teal app.}

\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{parentname}{(\code{character})\cr parent analysis data used in teal module, usually this refers to \code{ADSL}.}

\item{arm_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with all
available choices and preselected option for variable names that can be used as \code{arm_var}.
It defines the grouping variable in the results table.}

\item{visit_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with
all available choices and preselected option for variable names that can be used as \code{visit} variable.
Must be a factor in \code{dataname}.}

\item{paramcd}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with all
available choices and preselected option for the parameter code variable from \code{dataname}.}

\item{worst_flag_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object
with all available choices and preselected option for variable names that can be used as worst flag variable.}

\item{worst_flag_indicator}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr
value indicating worst grade.}

\item{anl_toxgrade_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr
variable for analysis toxicity grade.}

\item{base_toxgrade_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr
variable for baseline toxicity grade.}

\item{id_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object specifying
the variable name for subject id.}

\item{add_total}{(\code{logical})\cr whether to include column with total number of patients.}

\item{total_label}{(\code{string})\cr string to display as total column/row label if column/row is
enabled (see \code{add_total}). Defaults to \code{"All Patients"}. To set a new default \code{total_label} to
apply in all modules, run \code{set_default_total_label("new_default")}.}

\item{drop_arm_levels}{(\code{logical})\cr whether to drop unused levels of \code{arm_var}. If \code{TRUE}, \code{arm_var} levels are
set to those used in the \code{dataname} dataset. If \code{FALSE}, \code{arm_var} levels are set to those used in the
\code{parentname} dataset. If \code{dataname} and \code{parentname} are the same, then \code{drop_arm_levels} is set to \code{TRUE} and
user input for this parameter is ignored.}

\item{pre_output}{(\code{shiny.tag}) optional,\cr with text placed before the output to put the output into context.
For example a title.}

\item{post_output}{(\code{shiny.tag}) optional,\cr with text placed after the output to put the output into context.
For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}

\item{na_level}{(\code{string})\cr used to replace all \code{NA} or empty values
in character or factor variables in the data. Defaults to \code{"<Missing>"}. To set a
default \code{na_level} to apply in all modules, run \code{set_default_na_str("new_default")}.}

\item{code_missing_baseline}{(\code{logical})\cr whether missing baseline grades should be counted as grade 0.}

\item{basic_table_args}{(\code{basic_table_args}) optional\cr object created by \code{\link[teal.widgets:basic_table_args]{teal.widgets::basic_table_args()}}
with settings for the module table. The argument is merged with option \code{teal.basic_table_args} and with default
module arguments (hard coded in the module body).
For more details, see the vignette: \code{vignette("custom-basic-table-arguments", package = "teal.widgets")}.}

\item{transformators}{(\code{list} of \code{teal_transform_module}) that will be applied to transform module's data input.
To learn more check \code{vignette("transform-input-data", package = "teal")}.}

\item{decorators}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
(named \code{list} of lists of \code{teal_transform_module}) optional,
decorator for tables or plots included in the module output reported.
The decorators are applied to the respective output objects.

See section "Decorating Module" below for more details.}
}
\value{
a \code{teal_module} object.
}
\description{
This module produces a summary table of worst grades per subject by visit and parameter.
}
\section{Decorating Module}{


This module generates the following objects, which can be modified in place using decorators:
\itemize{
\item \code{table} (\code{TableTree} - output of \code{rtables::build_table()})
}

A Decorator is applied to the specific output using a named list of \code{teal_transform_module} objects.
The name of this list corresponds to the name of the output to which the decorator is applied.
See code snippet below:

\if{html}{\out{<div class="sourceCode">}}\preformatted{tm_t_shift_by_grade(
   ..., # arguments for module
   decorators = list(
     table = teal_transform_module(...) # applied only to `table` output
   )
)
}\if{html}{\out{</div>}}

For additional details and examples of decorators, refer to the vignette
\code{vignette("decorate-module-output", package = "teal.modules.clinical")}.

To learn more please refer to the vignette
\code{vignette("transform-module-output", package = "teal")} or the \code{\link[teal:teal_transform_module]{teal::teal_transform_module()}} documentation.
}

\examples{
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_shift_by_grade(
      label = "Grade Laboratory Abnormality Table",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      paramcd = choices_selected(
        choices = value_choices(ADLB, "PARAMCD", "PARAM"),
        selected = "ALT"
      ),
      worst_flag_var = choices_selected(
        choices = variable_choices(ADLB, subset = c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL")),
        selected = c("WGRLOVFL")
      ),
      worst_flag_indicator = choices_selected(
        value_choices(ADLB, "WGRLOVFL"),
        selected = "Y", fixed = TRUE
      ),
      anl_toxgrade_var = choices_selected(
        choices = variable_choices(ADLB, subset = c("ATOXGR")),
        selected = c("ATOXGR"),
        fixed = TRUE
      ),
      base_toxgrade_var = choices_selected(
        choices = variable_choices(ADLB, subset = c("BTOXGR")),
        selected = c("BTOXGR"),
        fixed = TRUE
      ),
      add_total = FALSE
    )
  ),
  filter = teal_slices(teal_slice("ADSL", "SAFFL", selected = "Y"))
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}

}
\seealso{
The \href{https://insightsengineering.github.io/tlg-catalog/stable/}{TLG Catalog} where additional example
apps implementing this module can be found.
}
\section{Examples in Shinylive}{
\describe{
  \item{example-1}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XCByCoqTgYAUwa-zHs4urptaF0PvHnJbXu8lGhUEV4tF2A0kjoALw6JK4Bp8IQiUSwnTI4RiSEQDIZEp+NaiOLGNb0Vh+ADmzEWOLxeOoUHocGo6MeAHEabp8kyiMxSHzWFl6BA+TBuNEhbYmSJHojcfT4aEjro4X9PnKGvSWDA-JoWOiCDFRgQxH5RCy4Oo4Pw6YqMkaTWJ0frGLQZXB1saZNjnilRIJ6BbSIbIU8sABZOU6P6RgDC2Uecjk8vtGQtImt-DZ4ajYC1eJTBYyqBYsA2hu9ptE5stWbt9sdPrRcP11EEnqb1fY-2aKUeAAVMlhMhGE9HB8PR0nU2mM1byNm1U98rZHsWdEWFfSInzRGtjIzKXqDXCu2b5-WNw6q87Wyx3fQRF6nTNe-7A8HQ48AOrsrD5AA8gAagAYi8eAxmAf4AYB4ETtB-4ABIAJJgRB-aIVgqHwWAyazval6Lt+WFAehSYblu9q7ow+5+IeUDHvEmyyAKjCVq+taZouDaKm2HYvs2PafJhMFkbhVFznWxHLgAmtGxi0BUNrovYziUQReJQBA-gChU1I1J6rocc2XELjavH0ueLY6K6j7PtZwktB+QZwCGZ5hpktiAQAGv+SaSYR0kqR5fzeX5WAzteJhKSFdiOC426FppGT0FAFqEkQ+lcie7FnreNZERZ0XWS6D4eoJ3bvjoAaue5OgEGGzThf5eGBYqRVLg1TUtZFeEpXiinKV1amJfa7UZDU-CZZ4rJwqBmT5LkY2Fg0kmKdQ5B5cUcDcOadDdr4+0+p5eQYVBuSZKBuH+sFXWPPJbVKIMtDGDo7DxFtUAWNo1g2GkCrEvErCZOg7AggAJIItApJDFqMNojCDH0ShgH0ZxAA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIACsieL8AazhWUUTQm3D+OGMoYVI-An5aUTKRscnp4Gh4GaS5AF1XCByCoqTgYAUwa-zHs4urptaF0PvHnJbXu8lGhUEV4tF2A0kjoALw6JK4Bp8IQiUSwnTI4RiSEQDIZEp+NaiOLGNb0Vh+ADmzEWOLxeOoUHocGo6MeAHEabp8kyiMxSHzWFl6BA+TBuNEhbYmSJHojcfT4aEjro4X9PnKGvSWDA-JoWOiCDFRgQxH5RCy4Oo4Pw6YqMkaTWJ0frGLQZXB1saZNjnilRIJ6BbSIbIU8sABZOU6P6RgDC2Uecjk8vtGQtImt-DZ4ajYC1eJTBYyqBYsA2hu9ptE5stWbt9sdPrRcP11EEnqb1fY-2aKUeAAVMlhMhGE9HB8PR0nU2mM1byNm1U98rZHsWdEWFfSInzRGtjIzKXqDXCu2b5-WNw6q87Wyx3fQRF6nTNe-7A8HQ48AOrsrD5AA8gAagAYi8eAxmAf4AYB4ETtB-4ABIAJJgRB-aIVgqHwWAyazval6Lt+WFAehSYblu9q7ow+5+IeUDHvEmyyAKjCVq+taZouDaKm2HYvs2PafJhMFkbhVFznWxHLgAmtGxi0BUNrovYziUQReJQBA-gChU1I1J6rocc2XELjavH0ueLY6K6j7PtZwktB+QZwCGZ5hpktiAQAGv+SaSYR0kqR5fzeX5WAzteJhKSFdiOC426FppGT0FAFqEkQ+lcie7FnreNZERZ0XWS6D4eoJ3bvjoAaue5OgEGGzThf5eGBYqRVLg1TUtZFeEpXiinKV1amJfa7UZDU-CZZ4rJwqBmT5LkY2Fg0kmKdQ5B5cUcDcOadDdr4+0+p5eQYVBuSZKBuH+sFXWPPJbVKIMtDGDo7DxFtUAWNo1g2GkCrEvErCZOg7AggAJIItApJDFqMNojCDH0ShgH0ZxAA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

