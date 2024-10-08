% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_events.R
\name{tm_t_events}
\alias{tm_t_events}
\title{teal Module: Events by Term}
\usage{
tm_t_events(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  hlt,
  llt,
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = default_na_str(),
  event_type = "event",
  sort_criteria = c("freq_desc", "alpha"),
  sort_freq_col = total_label,
  prune_freq = 0,
  prune_diff = 0,
  drop_arm_levels = TRUE,
  incl_overall_sum = TRUE,
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args()
)
}
\arguments{
\item{label}{(\code{character})\cr menu item label of the module in the teal app.}

\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{parentname}{(\code{character})\cr parent analysis data used in teal module, usually this refers to \code{ADSL}.}

\item{arm_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with all
available choices and preselected option for variable names that can be used as \code{arm_var}.
It defines the grouping variable(s) in the results table.
If there are two elements selected for \code{arm_var},
second variable will be nested under the first variable.}

\item{hlt}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr name of the variable
with high level term for events.}

\item{llt}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr name of the variable
with low level term for events.}

\item{add_total}{(\code{logical})\cr whether to include column with total number of patients.}

\item{total_label}{(\code{string})\cr string to display as total column/row label if column/row is
enabled (see \code{add_total}). Defaults to \code{"All Patients"}. To set a new default \code{total_label} to
apply in all modules, run \code{set_default_total_label("new_default")}.}

\item{na_level}{(\code{string})\cr used to replace all \code{NA} or empty values
in character or factor variables in the data. Defaults to \code{"<Missing>"}. To set a
default \code{na_level} to apply in all modules, run \code{set_default_na_str("new_default")}.}

\item{event_type}{(\code{character})\cr type of event that is summarized (e.g. adverse event, treatment). Default
is \code{"event"}.}

\item{sort_criteria}{(\code{character})\cr how to sort the final table. Default option \code{freq_desc} sorts
on column \code{sort_freq_col} by decreasing number of patients with event. Alternative option \code{alpha} sorts events
alphabetically.}

\item{sort_freq_col}{(\code{character})\cr column to sort by frequency on if \code{sort_criteria} is set to \code{freq_desc}.}

\item{prune_freq}{(\code{number})\cr threshold to use for trimming table using event incidence rate in any column.}

\item{prune_diff}{(\code{number})\cr threshold to use for trimming table using as criteria difference in
rates between any two columns.}

\item{drop_arm_levels}{(\code{logical})\cr whether to drop unused levels of \code{arm_var}. If \code{TRUE}, \code{arm_var} levels are
set to those used in the \code{dataname} dataset. If \code{FALSE}, \code{arm_var} levels are set to those used in the
\code{parentname} dataset. If \code{dataname} and \code{parentname} are the same, then \code{drop_arm_levels} is set to \code{TRUE} and
user input for this parameter is ignored.}

\item{incl_overall_sum}{(\code{flag})\cr  whether two rows which summarize the overall number of adverse events
should be included at the top of the table.}

\item{pre_output}{(\code{shiny.tag}) optional,\cr with text placed before the output to put the output into context.
For example a title.}

\item{post_output}{(\code{shiny.tag}) optional,\cr with text placed after the output to put the output into context.
For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}

\item{basic_table_args}{(\code{basic_table_args}) optional\cr object created by \code{\link[teal.widgets:basic_table_args]{teal.widgets::basic_table_args()}}
with settings for the module table. The argument is merged with option \code{teal.basic_table_args} and with default
module arguments (hard coded in the module body).
For more details, see the vignette: \code{vignette("custom-basic-table-arguments", package = "teal.widgets")}.}
}
\value{
a \code{teal_module} object.
}
\description{
This module produces a table of events by term.
}
\examples{
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADAE <- tmc_ex_adae
})
datanames(data) <- c("ADSL", "ADAE")
join_keys(data) <- default_cdisc_join_keys[datanames(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_events(
      label = "Adverse Event Table",
      dataname = "ADAE",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      llt = choices_selected(
        choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
        selected = c("AEDECOD")
      ),
      hlt = choices_selected(
        choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
        selected = "AEBODSYS"
      ),
      add_total = TRUE,
      event_type = "adverse event"
    )
  )
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
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOzMpyKSssrq9zglAF9FCCToeFFE0JtwgnYFMByC6ZTpnObp-oArIni-AGs4VhGk8aN+OGMoYVI-An5aUTL1zZ294EHYMVHPOQBdVwhZwvCksBgIs8vlpp9vr8mi0AaEgSDlmAIT80KgivFolMIBkkjoALw6JK4Bp8IQiUT4nSk4RvBoZEp+C5wbRkEZ0jI6ahQehwaiUxb8bSMUS6Jws0h2bkiebsnGhIa6AkIlx4WU6FgwPyaFiUggxDYEMR+EUidRwfjsSaLLAAWXmOmtNoAwtkVgsZra3WrqNQJQS9QajSa4GaLWqMgGZGJKdrGLQpXBLvqoyMlk4UlaZk5bE5PXgHVnsk4nQB5V1gORyYnYjm14Oh3VTQvFssrNVVtUxX265OG0TG3kh8hhmu1iO96ME2Px+giJOB1PQjNN5oAITLuQAmrl7YsnLkS06Vh3R2P68P+Vn13lt9N29Wx+r+PxGURPHyCfZnA+x8yKBdSFYVBFQLGohRFHQ-zIO9T36DJ+n6WhjB0dh4nINQNG0awbDSGtRDiCBWEydB2FRAASQRaBSciRUYIV+h6JQwB6T4gA}{Open in Shinylive}
    \if{html}{\out{<script type="text/javascript">$(function() {  var if_pkgdown = [...document.scripts].filter(x => x.src.includes("pkgdown.js")).length > 0;  if (if_pkgdown) {    $("iframe.iframe_shinylive").css("width", "150\%");  }});</script>}}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOzMpyKSssrq9zglAF9FCCToeFFE0JtwgnYFMByC6ZTpnObp-oArIni-AGs4VhGk8aN+OGMoYVI-An5aUTL1zZ294EHYMVHPOQBdVwhZwvCksBgIs8vlpp9vr8mi0AaEgSDlmAIT80KgivFolMIBkkjoALw6JK4Bp8IQiUT4nSk4RvBoZEp+C5wbRkEZ0jI6ahQehwaiUxb8bSMUS6Jws0h2bkiebsnGhIa6AkIlx4WU6FgwPyaFiUggxDYEMR+EUidRwfjsSaLLAAWXmOmtNoAwtkVgsZra3WrqNQJQS9QajSa4GaLWqMgGZGJKdrGLQpXBLvqoyMlk4UlaZk5bE5PXgHVnsk4nQB5V1gORyYnYjm14Oh3VTQvFssrNVVtUxX265OG0TG3kh8hhmu1iO96ME2Px+giJOB1PQjNN5oAITLuQAmrl7YsnLkS06Vh3R2P68P+Vn13lt9N29Wx+r+PxGURPHyCfZnA+x8yKBdSFYVBFQLGohRFHQ-zIO9T36DJ+n6WhjB0dh4nINQNG0awbDSGtRDiCBWEydB2FRAASQRaBSciRUYIV+h6JQwB6T4gA" style="height: 800px; width: 100\%; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: relative; z-index: 1"></iframe>}}
  }
}
}

