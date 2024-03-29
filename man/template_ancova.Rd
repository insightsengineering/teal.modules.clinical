% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_ancova.R
\name{template_ancova}
\alias{template_ancova}
\title{Template: ANCOVA Summary}
\usage{
template_ancova(
  dataname = "ANL",
  parentname = "ADSL",
  arm_var,
  ref_arm = NULL,
  comp_arm = NULL,
  combine_comp_arms = FALSE,
  aval_var,
  label_aval = NULL,
  cov_var,
  include_interact = FALSE,
  interact_var = NULL,
  interact_y = FALSE,
  paramcd_levels = "",
  paramcd_var = "PARAMCD",
  label_paramcd = NULL,
  visit_levels = "",
  visit_var = "AVISIT",
  conf_level = 0.95,
  basic_table_args = teal.widgets::basic_table_args()
)
}
\arguments{
\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{parentname}{(\code{character})\cr parent analysis data used in teal module, usually this refers to \code{ADSL}.}

\item{arm_var}{(\code{character})\cr variable names that can be used as \code{arm_var}.}

\item{ref_arm}{(\code{character})\cr the level of reference arm in case of arm comparison.}

\item{comp_arm}{(\code{character})\cr the level of comparison arm in case of arm comparison.}

\item{combine_comp_arms}{(\code{logical})\cr triggers the combination of comparison arms.}

\item{aval_var}{(\code{character})\cr name of the analysis value variable.}

\item{label_aval}{(\code{character})\cr
label of value variable used for title rendering.}

\item{cov_var}{(\code{character})\cr names of the covariates variables.}

\item{include_interact}{(\code{logical})\cr whether an interaction term should be included in the model.}

\item{interact_var}{(\code{character})\cr name of the variable that should have interactions with arm. If the
interaction is not needed, the default option is \code{NULL}.}

\item{interact_y}{(\code{character})\cr a selected item from the \code{interact_var} column which will be used to select the
specific ANCOVA results. If the interaction is not needed, the default option is \code{FALSE}.}

\item{paramcd_levels}{(\code{character})\cr
variable levels for the studied parameter.}

\item{paramcd_var}{(\code{character})\cr
variable name for the studied parameter.}

\item{label_paramcd}{(\code{character})\cr
variable label used for title rendering.}

\item{visit_levels}{(\code{character})\cr
variable levels for studied visits.}

\item{visit_var}{(\code{character})\cr variable names that can be used as \code{visit} variable. Must be a factor in
\code{dataname}.}

\item{conf_level}{(\code{numeric})\cr value for the confidence level within the range of (0, 1).}

\item{basic_table_args}{(\code{basic_table_args}) optional\cr object created by \code{\link[teal.widgets:basic_table_args]{teal.widgets::basic_table_args()}}
with settings for the module table. The argument is merged with option \code{teal.basic_table_args} and with default
module arguments (hard coded in the module body).
For more details, see the vignette: \code{vignette("custom-basic-table-arguments", package = "teal.widgets")}.}
}
\value{
a \code{list} of expressions to generate a table or plot object.
}
\description{
Creates a valid expression to generate an analysis of variance summary table.
}
\seealso{
\code{\link[=tm_t_ancova]{tm_t_ancova()}}
}
\keyword{internal}
