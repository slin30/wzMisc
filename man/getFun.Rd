% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFun.R
\name{getFun}
\alias{getFun}
\title{Extract a function by name with or without namespace prefix}
\usage{
getFun(fun)
}
\arguments{
\item{fun}{A character vector of length one, denoting the function to extract. May be
prefixed with a namespace (or not).}
}
\value{
A function matching \emph{fun}, or \code{NULL} (with a message) if an error.
}
\description{
Extract a function by name with or without namespace prefix
}
\details{
Most useful to create one-off functions where one wishes to call a specific function without
loading the library, and also to access non-exported functions. See examples.
}
\note{
See (and heed) documentation within \code{\link[utils]{getFromNamespace}}, which this function
uses.
}
\examples{
getFun("getFun")
getFun("getFun()") # paren pairs are stripped if included
getFun("stats::acf()")
getFun("stats:::[[.dendrogram")
}
