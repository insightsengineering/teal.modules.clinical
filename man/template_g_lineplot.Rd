% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_g_lineplot.R
\name{template_g_lineplot}
\alias{template_g_lineplot}
\title{Template: Line Plot}
\usage{
template_g_lineplot(
  dataname = "ANL",
  strata = lifecycle::deprecated(),
  group_var = "ARM",
  x = "AVISIT",
  y = "AVAL",
  y_unit = "AVALU",
  paramcd = "PARAMCD",
  param = "ALT",
  mid = "mean",
  interval = "mean_ci",
  whiskers = c("mean_ci_lwr", "mean_ci_upr"),
  table = c("n", "mean_sd", "median", "range"),
  mid_type = "pl",
  conf_level = 0.95,
  incl_screen = TRUE,
  mid_point_size = 2,
  table_font_size = 4,
  title = "Line Plot",
  y_lab = "",
  ggplot2_args = teal.widgets::ggplot2_args()
)
}
\arguments{
\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{strata}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use the \code{group_var} argument instead.}

\item{group_var}{(\code{string} or \code{NA})\cr group variable name.}

\item{x}{(\code{string})\cr x-variable name.}

\item{y}{(\code{string})\cr y-variable name.}

\item{y_unit}{(\code{string} or \code{NA})\cr y-axis unit variable name.}

\item{paramcd}{(\code{string} or \code{NA})\cr parameter code variable name.}

\item{param}{(\code{character})\cr parameter to filter the data by.}

\item{mid}{(\code{character} or \code{NULL})\cr names of the statistics that will be plotted as midpoints.
All the statistics indicated in \code{mid} variable must be present in the object returned by \code{sfun},
and be of a \code{double} or \code{numeric} type vector of length one.}

\item{interval}{(\code{character} or \code{NULL})\cr names of the statistics that will be plotted as intervals.
All the statistics indicated in \code{interval} variable must be present in the object returned by \code{sfun},
and be of a \code{double} or \code{numeric} type vector of length two. Set \code{interval = NULL} if intervals should not be
added to the plot.}

\item{whiskers}{(\code{character})\cr names of the interval whiskers that will be plotted. Names must match names
of the list element \code{interval} that will be returned by \code{sfun} (e.g. \code{mean_ci_lwr} element of
\code{sfun(x)[["mean_ci"]]}). It is possible to specify one whisker only, or to suppress all whiskers by setting
\code{interval = NULL}.}

\item{table}{(\code{character} or \code{NULL})\cr names of the statistics that will be displayed in the table below the plot.
All the statistics indicated in \code{table} variable must be present in the object returned by \code{sfun}.}

\item{mid_type}{(\code{string})\cr controls the type of the \code{mid} plot, it can be point (\code{"p"}), line (\code{"l"}),
or point and line (\code{"pl"}).}

\item{conf_level}{(\code{numeric})\cr value for the confidence level within the range of (0, 1).}

\item{incl_screen}{(\code{logical})\cr whether the screening visit should be included.}

\item{mid_point_size}{(\code{numeric(1)})\cr font size of the \code{mid} plot points.}

\item{table_font_size}{(\code{numeric(1)})\cr font size of the text in the table.}

\item{title}{(\code{string})\cr plot title.}

\item{y_lab}{(\code{string} or \code{NULL})\cr y-axis label. If \code{NULL} then no label will be added.}

\item{ggplot2_args}{(\code{ggplot2_args}) optional\cr object created by \code{\link[teal.widgets:ggplot2_args]{teal.widgets::ggplot2_args()}} with settings
for the module plot. For this module, this argument will only accept \code{ggplot2_args} object with \code{labs} list of
following child elements: \code{title}, \code{subtitle}, \code{caption}, \code{y}, \code{lty}. No other elements would be taken into
account. The argument is merged with option \code{teal.ggplot2_args} and with default module arguments (hard coded in
the module body).

For more details, see the vignette: \code{vignette("custom-ggplot2-arguments", package = "teal.widgets")}.}
}
\value{
a \code{list} of expressions to generate a table or plot object.
}
\description{
Creates a valid expression to generate a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} line plot.
}
\seealso{
\code{\link[=tm_g_lineplot]{tm_g_lineplot()}}
}
\keyword{internal}
