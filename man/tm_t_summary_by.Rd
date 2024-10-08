% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_summary_by.R
\name{tm_t_summary_by}
\alias{tm_t_summary_by}
\title{teal Module: Summarize Variables by Row Groups}
\usage{
tm_t_summary_by(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  by_vars,
  summarize_vars,
  id_var = teal.transform::choices_selected(teal.transform::variable_choices(dataname,
    subset = "USUBJID"), selected = "USUBJID", fixed = TRUE),
  paramcd = NULL,
  add_total = TRUE,
  total_label = default_total_label(),
  parallel_vars = FALSE,
  row_groups = FALSE,
  useNA = c("ifany", "no"),
  na_level = default_na_str(),
  numeric_stats = c("n", "mean_sd", "median", "range"),
  denominator = teal.transform::choices_selected(c("n", "N", "omit"), "omit", fixed =
    TRUE),
  drop_arm_levels = TRUE,
  drop_zero_levels = TRUE,
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

\item{by_vars}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with all
available choices and preselected option for variable names used to split the summary by rows.}

\item{summarize_vars}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr names of
the variables that should be summarized.}

\item{id_var}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object specifying
the variable name for subject id.}

\item{paramcd}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with all
available choices and preselected option for the parameter code variable from \code{dataname}.}

\item{add_total}{(\code{logical})\cr whether to include column with total number of patients.}

\item{total_label}{(\code{string})\cr string to display as total column/row label if column/row is
enabled (see \code{add_total}). Defaults to \code{"All Patients"}. To set a new default \code{total_label} to
apply in all modules, run \code{set_default_total_label("new_default")}.}

\item{parallel_vars}{(\code{logical})\cr whether summarized variables should be arranged in columns. Can only be set to
\code{TRUE} if all chosen analysis variables are numeric.}

\item{row_groups}{(\code{logical})\cr whether summarized variables should be arranged in row groups.}

\item{useNA}{(\code{character})\cr whether missing data (\code{NA}) should be displayed as a level.}

\item{na_level}{(\code{string})\cr used to replace all \code{NA} or empty values
in character or factor variables in the data. Defaults to \code{"<Missing>"}. To set a
default \code{na_level} to apply in all modules, run \code{set_default_na_str("new_default")}.}

\item{numeric_stats}{(\code{character})\cr names of statistics to display for numeric summary variables. Available
statistics are \code{n}, \code{mean_sd}, \code{mean_ci}, \code{median}, \code{median_ci}, \code{quantiles}, \code{range}, and \code{geom_mean}.}

\item{denominator}{(\code{character})\cr chooses how percentages are calculated. With option \code{N}, the reference
population from the column total is used as the denominator. With option \code{n}, the number of non-missing
records in this row and column intersection is used as the denominator. If \code{omit} is chosen, then the
percentage is omitted.}

\item{drop_arm_levels}{(\code{logical})\cr whether to drop unused levels of \code{arm_var}. If \code{TRUE}, \code{arm_var} levels are
set to those used in the \code{dataname} dataset. If \code{FALSE}, \code{arm_var} levels are set to those used in the
\code{parentname} dataset. If \code{dataname} and \code{parentname} are the same, then \code{drop_arm_levels} is set to \code{TRUE} and
user input for this parameter is ignored.}

\item{drop_zero_levels}{(\code{logical})\cr whether rows with zero counts in all columns should be removed from the table.}

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
This module produces a table to summarize variables by row groups.
}
\examples{
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADLB <- tmc_ex_adlb
})
datanames(data) <- c("ADSL", "ADLB")
join_keys(data) <- default_cdisc_join_keys[datanames(data)]

ADSL <- data[["ADSL"]]
ADLB <- data[["ADLB"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_summary_by(
      label = "Summary by Row Groups Table",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices(ADSL, c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      add_total = TRUE,
      by_vars = choices_selected(
        choices = variable_choices(ADLB, c("PARAM", "AVISIT")),
        selected = c("AVISIT")
      ),
      summarize_vars = choices_selected(
        choices = variable_choices(ADLB, c("AVAL", "CHG")),
        selected = c("AVAL")
      ),
      useNA = "ifany",
      paramcd = choices_selected(
        choices = value_choices(ADLB, "PARAMCD", "PARAM"),
        selected = "ALT"
      )
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
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIJOh4UUTQm3CCdgUwHILZlNmcltnBgCsieL8AazhWMaTJo344YyhhUj8CflpRMs3tvYPgYdgxcc85AF1XCHnCuEksBgMs8vlZt9fv8mq0gaEQWDVmAoX80KgivFojMIBkkjoALw6JK4Bp8IQiUSEnTk4QfBoZEp+K6iQQwGAsVh+egcBkZHTUKD0ODUamzXJsjlsHQ8nRYIgRHQAcUYREEqCptiFIkWfLxoRGuiJSOautx-IyLBgfk0LGpBBiWwIYj8ohFcHUcH4OItFodTrE1NtjFo2rg10dMg+AJS02WWAAsosdPGEwBhbJrOSk82+nRukSe-hiuaJ2Z6nTZis1fjMoieUVE+zOHN5nk2lhUon+qOiV3uos+vM6HvOrs6YOh+giCMBsYrZqxmZgAAKmSwmSTeBTcwAagBJXL72xZ1vDgse8jF7vLzIHo8nsCDPNV3P81nsli0ABe4eD49HF0L0HCsMkA8dJzDWde3YBcl2WXdMghbdZjTAAJJVT1A-MByve1b0Q5Dn19V880EN0ADlMhLWhzggVgzTzVAWFgG57UjMd+0LK8hzzcCg24QRw3A2DYSWVd103DNk1mNcNy3Ujz1wr0SyQx8K2IjJiMGQZaJ0dh4nINQNG0awbDSc1RDiejMnQdh0QAEkEWgUkct1GG0RhBj6JQwD6b4gA}{Open in Shinylive}
    \if{html}{\out{<script type="text/javascript">$(function() {  var if_pkgdown = [...document.scripts].filter(x => x.src.includes("pkgdown.js")).length > 0;  if (if_pkgdown) {    $("iframe.iframe_shinylive").css("width", "150\%");  }});</script>}}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpfyiko+o12oB9d09rNw8vQx0Ad1pSAAtaCHZAqFwdECUdHQBBABEAZQAZbx1SGAI-OAAPPyh+UWp0rOz8gCEikrLK6v5qeiUAX0UIJOh4UUTQm3CCdgUwHILZlNmcltnBgCsieL8AazhWMaTJo344YyhhUj8CflpRMs3tvYPgYdgxcc85AF1XCHnCuEksBgMs8vlZt9fv8mq0gaEQWDVmAoX80KgivFojMIBkkjoALw6JK4Bp8IQiUSEnTk4QfBoZEp+K6iQQwGAsVh+egcBkZHTUKD0ODUamzXJsjlsHQ8nRYIgRHQAcUYREEqCptiFIkWfLxoRGuiJSOautx-IyLBgfk0LGpBBiWwIYj8ohFcHUcH4OItFodTrE1NtjFo2rg10dMg+AJS02WWAAsosdPGEwBhbJrOSk82+nRukSe-hiuaJ2Z6nTZis1fjMoieUVE+zOHN5nk2lhUon+qOiV3uos+vM6HvOrs6YOh+giCMBsYrZqxmZgAAKmSwmSTeBTcwAagBJXL72xZ1vDgse8jF7vLzIHo8nsCDPNV3P81nsli0ABe4eD49HF0L0HCsMkA8dJzDWde3YBcl2WXdMghbdZjTAAJJVT1A-MByve1b0Q5Dn19V880EN0ADlMhLWhzggVgzTzVAWFgG57UjMd+0LK8hzzcCg24QRw3A2DYSWVd103DNk1mNcNy3Ujz1wr0SyQx8K2IjJiMGQZaJ0dh4nINQNG0awbDSc1RDiejMnQdh0QAEkEWgUkct1GG0RhBj6JQwD6b4gA" style="height: 800px; width: 100\%; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: relative; z-index: 1"></iframe>}}
  }
}
}

