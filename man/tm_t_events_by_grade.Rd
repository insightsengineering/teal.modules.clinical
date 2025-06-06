% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_events_by_grade.R
\name{tm_t_events_by_grade}
\alias{tm_t_events_by_grade}
\title{teal Module: Events by Grade}
\usage{
tm_t_events_by_grade(
  label,
  dataname,
  parentname = ifelse(inherits(arm_var, "data_extract_spec"),
    teal.transform::datanames_input(arm_var), "ADSL"),
  arm_var,
  hlt,
  llt,
  grade,
  grading_groups = list(`Any Grade (\%)` = c("1", "2", "3", "4", "5"), `Grade 1-2 (\%)` =
    c("1", "2"), `Grade 3-4 (\%)` = c("3", "4"), `Grade 5 (\%)` = "5"),
  col_by_grade = FALSE,
  prune_freq = 0,
  prune_diff = 0,
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = default_na_str(),
  drop_arm_levels = TRUE,
  pre_output = NULL,
  post_output = NULL,
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

\item{hlt}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr name of the variable
with high level term for events.}

\item{llt}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr name of the variable
with low level term for events.}

\item{grade}{(\code{character})\cr name of the severity level variable.}

\item{grading_groups}{(\code{list})\cr named list of grading groups used when \code{col_by_grade = TRUE}.}

\item{col_by_grade}{(\code{logical})\cr whether to display the grading groups in nested columns.}

\item{prune_freq}{(\code{number})\cr threshold to use for trimming table using event incidence rate in any column.}

\item{prune_diff}{(\code{number})\cr threshold to use for trimming table using as criteria difference in
rates between any two columns.}

\item{add_total}{(\code{logical})\cr whether to include column with total number of patients.}

\item{total_label}{(\code{string})\cr string to display as total column/row label if column/row is
enabled (see \code{add_total}). Defaults to \code{"All Patients"}. To set a new default \code{total_label} to
apply in all modules, run \code{set_default_total_label("new_default")}.}

\item{na_level}{(\code{string})\cr used to replace all \code{NA} or empty values
in character or factor variables in the data. Defaults to \code{"<Missing>"}. To set a
default \code{na_level} to apply in all modules, run \code{set_default_na_str("new_default")}.}

\item{drop_arm_levels}{(\code{logical})\cr whether to drop unused levels of \code{arm_var}. If \code{TRUE}, \code{arm_var} levels are
set to those used in the \code{dataname} dataset. If \code{FALSE}, \code{arm_var} levels are set to those used in the
\code{parentname} dataset. If \code{dataname} and \code{parentname} are the same, then \code{drop_arm_levels} is set to \code{TRUE} and
user input for this parameter is ignored.}

\item{pre_output}{(\code{shiny.tag}) optional,\cr with text placed before the output to put the output into context.
For example a title.}

\item{post_output}{(\code{shiny.tag}) optional,\cr with text placed after the output to put the output into context.
For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}

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
This module produces a table to summarize events by grade.
}
\section{Decorating Module}{


This module generates the following objects, which can be modified in place using decorators:
\itemize{
\item \code{table} (\code{TableTree} as created from \code{rtables::build_table})
}

A Decorator is applied to the specific output using a named list of \code{teal_transform_module} objects.
The name of this list corresponds to the name of the output to which the decorator is applied.
See code snippet below:

\if{html}{\out{<div class="sourceCode">}}\preformatted{tm_t_events_by_grade(
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
library(dplyr)
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  .lbls_adae <- col_labels(tmc_ex_adae)
  ADAE <- tmc_ex_adae \%>\%
    mutate_if(is.character, as.factor) #' be certain of having factors
  col_labels(ADAE) <- .lbls_adae
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_events_by_grade(
      label = "Adverse Events by Grade Table",
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
      grade = choices_selected(
        choices = variable_choices(ADAE, c("AETOXGR", "AESEV")),
        selected = "AETOXGR"
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
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEPxQpFD6RlzUAPpBIdZKMaGGOgDutKQAFrQQ7Am4OiBKOjoAggAiAMoAMmE6pDAEkXAAHpFQ-KLUhToY1PTUoq1BuknEUdRQ9HD9nPWNLW1QcAFFZcVONXUNzYOLOgCkAHx7XUUwgiHkkbTG7LTiBOksUOqqeVDixs+kRP46AMQAch0kx0BFUISyOiIxh0j00WQA5iYvj9RF1RpFxpNpqsnDYkj0+gMFnAlABfAIAKyIWUiAGs4KxRDlglB8UZ+HBPsJSJECPw7g1qbSGUzgNB4MyEnIALquCBlKo1BLAYAKMCKyrqmVyhWlNbK1mq9W47W6pRoVA1LJpdhdBI6AC8OlyXT4QhEoidOndwjEdogRSKdUivLg2jIA3orEiCOYnIDQaDWKm3pN-G0jFEuicEdIXujOgA4vHdLYJiJ1bgTkGEhLdM6TfqXHga0UWDBIpoWN6HjSwQNsyIXvx2AQ7RqsABZKs6E3TgDCpXVcjy85nYFXbZ01GopF76X7YkiQ7gI8TSaDfZkYm93cYtArcD5h5vzNxeXHJqctic09n36lE4C4APLLpuW6BpeRSniOvYTmsQGgeByyXpB0HpHuB5HoOUxnuQo7bkU14DneLCPn0z4kf6H6gghTgAEJgeUACa5QARqTjlCBC4ruh0E6LBBFppxTEVGx6rbvxSZxm0Dagq+A4nnh55EQpOFkQ+T4vjh7C0V+nG2CBAAaRZYBxazlE4ABqfHVlB0FCXA-AiWsRmmeZYBSTWqEBAE1w6LcKhqBo2jWDYBRQaImQQKwxToOwloACSCLQbzoEl2aMJmARkkoYBkjKQA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEPxQpFD6RlzUAPpBIdZKMaGGOgDutKQAFrQQ7Am4OiBKOjoAggAiAMoAMmE6pDAEkXAAHpFQ-KLUhToY1PTUoq1BuknEUdRQ9HD9nPWNLW1QcAFFZcVONXUNzYOLOgCkAHx7XUUwgiHkkbTG7LTiBOksUOqqeVDixs+kRP46AMQAch0kx0BFUISyOiIxh0j00WQA5iYvj9RF1RpFxpNpqsnDYkj0+gMFnAlABfAIAKyIWUiAGs4KxRDlglB8UZ+HBPsJSJECPw7g1qbSGUzgNB4MyEnIALquCBlKo1BLAYAKMCKyrqmVyhWlNbK1mq9W47W6pRoVA1LJpdhdBI6AC8OlyXT4QhEoidOndwjEdogRSKdUivLg2jIA3orEiCOYnIDQaDWKm3pN-G0jFEuicEdIXujOgA4vHdLYJiJ1bgTkGEhLdM6TfqXHga0UWDBIpoWN6HjSwQNsyIXvx2AQ7RqsABZKs6E3TgDCpXVcjy85nYFXbZ01GopF76X7YkiQ7gI8TSaDfZkYm93cYtArcD5h5vzNxeXHJqctic09n36lE4C4APLLpuW6BpeRSniOvYTmsQGgeByyXpB0HpHuB5HoOUxnuQo7bkU14DneLCPn0z4kf6H6gghTgAEJgeUACa5QARqTjlCBC4ruh0E6LBBFppxTEVGx6rbvxSZxm0Dagq+A4nnh55EQpOFkQ+T4vjh7C0V+nG2CBAAaRZYBxazlE4ABqfHVlB0FCXA-AiWsRmmeZYBSTWqEBAE1w6LcKhqBo2jWDYBRQaImQQKwxToOwloACSCLQbzoEl2aMJmARkkoYBkjKQA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

