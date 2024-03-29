% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_mult_events.R
\name{template_mult_events}
\alias{template_mult_events}
\title{Template: Multiple Events by Term}
\usage{
template_mult_events(
  dataname,
  parentname,
  arm_var,
  seq_var,
  hlt,
  llt,
  add_total = TRUE,
  total_label = default_total_label(),
  na_level = default_na_str(),
  event_type = "event",
  drop_arm_levels = TRUE,
  basic_table_args = teal.widgets::basic_table_args()
)
}
\arguments{
\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{parentname}{(\code{character})\cr parent analysis data used in teal module, usually this refers to \code{ADSL}.}

\item{arm_var}{(\code{character})\cr variable names that can be used as \code{arm_var}.}

\item{seq_var}{(\code{character})\cr name of analysis sequence number variable. Used for counting the unique number
of events.}

\item{hlt}{(\code{character})\cr name of the variable with high level term for events.}

\item{llt}{(\code{character})\cr name of the variable with low level term for events.}

\item{add_total}{(\code{logical})\cr whether to include column with total number of patients.}

\item{total_label}{(\code{string})\cr string to display as total column/row label if column/row is
enabled (see \code{add_total}). Defaults to \code{"All Patients"}. To set a new default \code{total_label} to
apply in all modules, run \code{set_default_total_label("new_default")}.}

\item{na_level}{(\code{string})\cr used to replace all \code{NA} or empty values
in character or factor variables in the data. Defaults to \code{"<Missing>"}. To set a
default \code{na_level} to apply in all modules, run \code{set_default_na_str("new_default")}.}

\item{event_type}{(\code{character})\cr type of event that is summarized (e.g. adverse event, treatment). Default
is \code{"event"}.}

\item{drop_arm_levels}{(\code{logical})\cr whether to drop unused levels of \code{arm_var}. If \code{TRUE}, \code{arm_var} levels are
set to those used in the \code{dataname} dataset. If \code{FALSE}, \code{arm_var} levels are set to those used in the
\code{parentname} dataset. If \code{dataname} and \code{parentname} are the same, then \code{drop_arm_levels} is set to \code{TRUE} and
user input for this parameter is ignored.}

\item{basic_table_args}{(\code{basic_table_args}) optional\cr object created by \code{\link[teal.widgets:basic_table_args]{teal.widgets::basic_table_args()}}
with settings for the module table. The argument is merged with option \code{teal.basic_table_args} and with default
module arguments (hard coded in the module body).
For more details, see the vignette: \code{vignette("custom-basic-table-arguments", package = "teal.widgets")}.}
}
\value{
a \code{list} of expressions to generate a table or plot object.
}
\description{
Creates a valid expression to generate a table of multiple events by term.
}
\seealso{
\code{\link[=tm_t_mult_events]{tm_t_mult_events()}}
}
\keyword{internal}
