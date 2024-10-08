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
  basic_table_args = teal.widgets::basic_table_args()
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
}
\value{
a \code{teal_module} object.
}
\description{
This module produces a table to summarize events by grade.
}
\examples{
library(dplyr)
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  lbls_adae <- col_labels(tmc_ex_adae)
  ADAE <- tmc_ex_adae \%>\%
    mutate_if(is.character, as.factor) #' be certain of having factors
  col_labels(ADAE) <- lbls_adae
})
datanames(data) <- c("ADSL", "ADAE")
join_keys(data) <- default_cdisc_join_keys[datanames(data)]

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
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEPxQpFD6RlzUAPpBIdZKMaGGOgDutKQAFrQQ7Am4OiBKOjoAggAiAMoAMmE6pDAEkXAAHpFQ-KLUhTrU9NSirUG6ScRR1FD0cH2c9Y0tbVBwAUVlxU41dQ3NAws6AKQAfLtdRTCCIeSRtMbstOIE6SxQ6qp5UOLGT6RE-joAxADkOgmOgIqhCWR0RGMOgemiyAHMTJ9vqIuiNImMJlMVk4bEken1tnAlABfAIJaDwUQ5YJQPFGAjsBRgMpVZl5Zk45kBABWRCykQA1nBWNSEvSdPw4B9hKRIgR+LcGnyBcLRcAKbAxDSQnIALquCCs6pJBLAYCciqVZl6g1G0qrGpmi0sh0uMC2w1oVA1LJpJkQIoJHQAXkltNwXT4QhEolDOmjwm1x1qMEicrg2jI-XorEi8OYUoDRRL3XGk3jnP42kYol0TizpDjuZ0AHFC7pbOMROyU0HaZTdGHLate4HSzoWGnNCx4-d+aD+nWRM9+OxGZysABZdk6TdbgDCpW5HJZ25Pfe61FIc-SC7EkWXcFXxYnRXnMjE8ZnjFo3bg8p3p+1I4nkG4sk4thOOeeB7hBpROAeADyx5gHIciRuOb5Pquc5MvBiEodyl4YZe6TXre95LpMz7kGul7vkBi7fiwf69ABH6LuwoEgvhqwAEIoeUACa5S7pyTjlEhB7cqRWETjhdGVhBgkVKJzIkZhb4Fm0Q4gkxD6KXA9HyaWnFfmGP5sSIgFUdxbpgXxkFIQAGq2WDiRB5ROAAarJWlvjoRn8Mpqy2K57kaaZOhLCWsUBAEVw6DcKhqBo2jWDYBTjqImQQKwxToOw3oACSCLQrzoCVdaMDWAQkkoYAknqQA}{Open in Shinylive}
    \if{html}{\out{<script type="text/javascript">$(function() {  var if_pkgdown = [...document.scripts].filter(x => x.src.includes("pkgdown.js")).length > 0;  if (if_pkgdown) {    $("iframe.iframe_shinylive").css("width", "150\%");  }});</script>}}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsO-VNVaNFEPxQpFD6RlzUAPpBIdZKMaGGOgDutKQAFrQQ7Am4OiBKOjoAggAiAMoAMmE6pDAEkXAAHpFQ-KLUhTrU9NSirUG6ScRR1FD0cH2c9Y0tbVBwAUVlxU41dQ3NAws6AKQAfLtdRTCCIeSRtMbstOIE6SxQ6qp5UOLGT6RE-joAxADkOgmOgIqhCWR0RGMOgemiyAHMTJ9vqIuiNImMJlMVk4bEken1tnAlABfAIJaDwUQ5YJQPFGAjsBRgMpVZl5Zk45kBABWRCykQA1nBWNSEvSdPw4B9hKRIgR+LcGnyBcLRcAKbAxDSQnIALquCCs6pJBLAYCciqVZl6g1G0qrGpmi0sh0uMC2w1oVA1LJpJkQIoJHQAXkltNwXT4QhEolDOmjwm1x1qMEicrg2jI-XorEi8OYUoDRRL3XGk3jnP42kYol0TizpDjuZ0AHFC7pbOMROyU0HaZTdGHLate4HSzoWGnNCx4-d+aD+nWRM9+OxGZysABZdk6TdbgDCpW5HJZ25Pfe61FIc-SC7EkWXcFXxYnRXnMjE8ZnjFo3bg8p3p+1I4nkG4sk4thOOeeB7hBpROAeADyx5gHIciRuOb5Pquc5MvBiEodyl4YZe6TXre95LpMz7kGul7vkBi7fiwf69ABH6LuwoEgvhqwAEIoeUACa5S7pyTjlEhB7cqRWETjhdGVhBgkVKJzIkZhb4Fm0Q4gkxD6KXA9HyaWnFfmGP5sSIgFUdxbpgXxkFIQAGq2WDiRB5ROAAarJWlvjoRn8Mpqy2K57kaaZOhLCWsUBAEVw6DcKhqBo2jWDYBTjqImQQKwxToOw3oACSCLQrzoCVdaMDWAQkkoYAknqQA" style="height: 800px; width: 100\%; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: relative; z-index: 1"></iframe>}}
  }
}
}

