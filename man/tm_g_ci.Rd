% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_g_ci.R
\name{tm_g_ci}
\alias{tm_g_ci}
\title{teal Module: Confidence Interval Plot}
\usage{
tm_g_ci(
  label,
  x_var,
  y_var,
  color,
  stat = c("mean", "median"),
  conf_level = teal.transform::choices_selected(c(0.95, 0.9, 0.8), 0.95, keep_order =
    TRUE),
  plot_height = c(700L, 200L, 2000L),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list(),
  decorators = list()
)
}
\arguments{
\item{label}{(\code{character})\cr menu item label of the module in the teal app.}

\item{x_var}{(\code{character})\cr name of the treatment variable to put on the x-axis.}

\item{y_var}{(\code{character})\cr name of the response variable to put on the y-axis.}

\item{color}{(\code{data_extract_spec})\cr the group variable used to determine the plot colors, shapes, and line types.}

\item{stat}{(\code{character})\cr statistic to plot. Options are \code{"mean"} and \code{"median"}.}

\item{conf_level}{(\code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}})\cr object with
all available choices and pre-selected option for the confidence level, each within range of (0, 1).}

\item{plot_height}{(\code{numeric}) optional\cr vector of length three with \code{c(value, min, max)}. Specifies the
height of the main plot and renders a slider on the plot to interactively adjust the plot height.}

\item{plot_width}{(\code{numeric}) optional\cr vector of length three with \code{c(value, min, max)}. Specifies the width
of the main plot and renders a slider on the plot to interactively adjust the plot width.}

\item{pre_output}{(\code{shiny.tag}) optional,\cr with text placed before the output to put the output into context.
For example a title.}

\item{post_output}{(\code{shiny.tag}) optional,\cr with text placed after the output to put the output into context.
For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}

\item{ggplot2_args}{(\code{ggplot2_args}) optional\cr object created by \code{\link[teal.widgets:ggplot2_args]{teal.widgets::ggplot2_args()}} with settings
for the module plot. The argument is merged with option \code{teal.ggplot2_args} and with default module arguments
(hard coded in the module body).
For more details, see the vignette: \code{vignette("custom-ggplot2-arguments", package = "teal.widgets")}.}

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
This module produces a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} type confidence interval plot consistent with the TLG Catalog template
\code{CIG01} available \href{https://insightsengineering.github.io/tlg-catalog/stable/graphs/other/cig01.html}{here}.
}
\section{Decorating Module}{


This module generates the following objects, which can be modified in place using decorators:
\itemize{
\item \code{plot} (\code{ggplot})
}

A Decorator is applied to the specific output using a named list of \code{teal_transform_module} objects.
The name of this list corresponds to the name of the output to which the decorator is applied.
See code snippet below:

\if{html}{\out{<div class="sourceCode">}}\preformatted{tm_g_ci(
   ..., # arguments for module
   decorators = list(
     plot = teal_transform_module(...) # applied only to `plot` output
   )
)
}\if{html}{\out{</div>}}

For additional details and examples of decorators, refer to the vignette
\code{vignette("decorate-module-output", package = "teal.modules.clinical")}.

To learn more please refer to the vignette
\code{vignette("transform-module-output", package = "teal")} or the \code{\link[teal:teal_transform_module]{teal::teal_transform_module()}} documentation.
}

\examples{
library(nestcolor)

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
    tm_g_ci(
      label = "Confidence Interval Plot",
      x_var = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = c("ARMCD", "BMRKR2"),
          selected = c("ARMCD"),
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y_var = data_extract_spec(
        dataname = "ADLB",
        filter = list(
          filter_spec(
            vars = "PARAMCD",
            choices = levels(ADLB$PARAMCD),
            selected = levels(ADLB$PARAMCD)[1],
            multiple = FALSE,
            label = "Select lab:"
          ),
          filter_spec(
            vars = "AVISIT",
            choices = levels(ADLB$AVISIT),
            selected = levels(ADLB$AVISIT)[1],
            multiple = FALSE,
            label = "Select visit:"
          )
        ),
        select = select_spec(
          label = "Analyzed Value",
          choices = c("AVAL", "CHG"),
          selected = "AVAL",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Color by variable",
          choices = c("SEX", "STRATA1", "STRATA2"),
          selected = c("STRATA1"),
          multiple = FALSE,
          fixed = FALSE
        )
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
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo6AIIAIgDKADKROqQwBDFwAB4xUPyi1KUVlbUAQg1NLe2d-NT0SgC+QQBWRLkxANZwrKL5yTap-HDGUMKkMQT8tKIti8trG8DQ8JsFcgC6rhBVdQ0FwMAKYB+1P7PV7vAbDXbJH5-KpDIEgpRoVANXKZdh9Ao6AC8OgKuD6fCEIlEWJ0BOEYjREDKZSaMQA5idaJTqdTqFB6HBqCS-gBhEjGWh7cy6ACSKkYmm4OgACv5SH88VSWToOpLGCSCq02qQ1MdRKg4ARmcqygV7rpsdCaoC8H0TaJOYbSCSHSJ1DF9YbjSaygQsksCGISUboVgALI8yoKnR-QZhrAAaSwACY-nJFT7qa6nXB+MG0f9w5G0xnM6SjrRUCISQAxcq1apOUuZgVtXO1+uNu3KoIm9PdsqsGJqjXJLU6qDuz0hpUms2wC0x-5ghUD6kC6jkdXYuiiUjen0brceg0zsvUtXEy1gaXlLDlCNR22zst+gNBndwbTUTYwwYAElve9H37F9M2zdR20-b9fzBQC7wfSM5GAABGZ5mzLGAKyrRc6wbJs1xNNkOS5a9qkddQdGIxA-kI6lQPPI9VBPL06JZS9uX+AA1EVqhFWxVzAn03xkD8qK-TlYKGf9yh4vjbAY88dAg8g82gyT2D-GS5P45C0IwzMsM3Stq2xPDGwMn1iM5TjyLdZ1NHOTIaLANidF7H1FPtCjnWxFSWLPMtrNIpdymgahWAAL3bLjuEEOBBPPETAyvHQQ24+to15AAJABxEs3JUqDQq4zLn3PIyNBwjt8MsllW2K8yXCEsoPJZLzqT8AJR3CcddQCg8WXneBOIBRKfRUl0fIGtzgs4vl-HVehWB0NVaHZERxszZKxPSxsAA0srAap7HKWxyhQo6Tvvc7UzADrvPs4q9tO87LvuurqUqkzcM7AiWvXWg2zUnQmrotr6O7DygiCWhjB0dhci3ScNG0awbBKJVRByCBWHKdB2ERf9BFoIoiYdCVVCCGYlDAGZniAA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMBOhFoFuASgA60snGYFStAG5wABAB4AtDoBmgiOtol2cnQBUsAVQCiSpXSYsOEMaWLUijIoQSvxQpFD6RlzUAPqh4dYhYRGGOgDutKQAFrQQ7PFQuDogSjo6AIIAIgDKADKROqQwBDFwAB4xUPyi1KUVlbUAQg1NLe2d-NT0SgC+QQBWRLkxANZwrKL5yTap-HDGUMKkMQT8tKIti8trG8DQ8JsFcgC6rhBVdQ0FwMAKYB+1P7PV7vAbDXbJH5-KpDIEgpRoVANXKZdh9Ao6AC8OgKuD6fCEIlEWJ0BOEYjREDKZSaMQA5idaJTqdTqFB6HBqCS-gBhEjGWh7cy6ACSKkYmm4OgACv5SH88VSWToOpLGCSCq02qQ1MdRKg4ARmcqygV7rpsdCaoC8H0TaJOYbSCSHSJ1DF9YbjSaygQsksCGISUboVgALI8yoKnR-QZhrAAaSwACY-nJFT7qa6nXB+MG0f9w5G0xnM6SjrRUCISQAxcq1apOUuZgVtXO1+uNu3KoIm9PdsqsGJqjXJLU6qDuz0hpUms2wC0x-5ghUD6kC6jkdXYuiiUjen0brceg0zsvUtXEy1gaXlLDlCNR22zst+gNBndwbTUTYwwYAElve9H37F9M2zdR20-b9fzBQC7wfSM5GAABGZ5mzLGAKyrRc6wbJs1xNNkOS5a9qkddQdGIxA-kI6lQPPI9VBPL06JZS9uX+AA1EVqhFWxVzAn03xkD8qK-TlYKGf9yh4vjbAY88dAg8g82gyT2D-GS5P45C0IwzMsM3Stq2xPDGwMn1iM5TjyLdZ1NHOTIaLANidF7H1FPtCjnWxFSWLPMtrNIpdymgahWAAL3bLjuEEOBBPPETAyvHQQ24+to15AAJABxEs3JUqDQq4zLn3PIyNBwjt8MsllW2K8yXCEsoPJZLzqT8AJR3CcddQCg8WXneBOIBRKfRUl0fIGtzgs4vl-HVehWB0NVaHZERxszZKxPSxsAA0srAap7HKWxyhQo6Tvvc7UzADrvPs4q9tO87LvuurqUqkzcM7AiWvXWg2zUnQmrotr6O7DygiCWhjB0dhci3ScNG0awbBKJVRByCBWHKdB2ERf9BFoIoiYdCVVCCGYlDAGZniAA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

