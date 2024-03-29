% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_input}
\alias{extract_input}
\title{Extracts html id for \code{data_extract_ui}}
\usage{
extract_input(varname, dataname, filter = FALSE)
}
\arguments{
\item{varname}{(\code{character})\cr
the original html id.  This should be retrieved with \code{ns("original id")} in the UI function
or \code{session$ns("original id")}/"original id" in the server function.}

\item{dataname}{(\code{character})\cr
\code{dataname} from data_extract input.
This might be retrieved like \code{data_extract_spec(...)[[1]]$dataname}.}

\item{filter}{(\code{logical}) optional,\cr
if the connected \code{extract_data_spec} has objects passed to its \code{filter} argument}
}
\value{
a string
}
\description{
The \code{data_extract_ui} is located under extended html id. We could not use \code{ns("original id")}
for reference, as it is extended with specific suffixes.
}
\examples{
extract_input("ARM", "ADSL")

}
